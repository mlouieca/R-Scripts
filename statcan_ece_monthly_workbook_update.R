library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)
library(openxlsx)
library(zip)

# ---------------------------------------------------
# Configuration
# ---------------------------------------------------

env_value <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)

  if (is.na(value) || value == "") {
    return(default)
  }

  value
}

env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)

  if (is.na(value) || value == "") {
    return(default)
  }

  tolower(value) %in% c("1", "true", "yes", "y")
}

first_existing_path <- function(paths, default = paths[1]) {
  matches <- paths[file.exists(paths)]

  if (length(matches) > 0) {
    return(matches[1])
  }

  default
}

current_script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_match <- command_args[str_starts(command_args, file_arg)]

  if (length(file_match) > 0) {
    return(normalizePath(str_remove(file_match[1], file_arg), mustWork = FALSE))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    active_document <- rstudioapi::getActiveDocumentContext()$path

    if (!is.null(active_document) && active_document != "") {
      return(normalizePath(active_document, mustWork = FALSE))
    }
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(normalizePath(frame$ofile, mustWork = FALSE))
    }
  }

  NA_character_
}

script_path <- current_script_path()
script_dir <- if (!is.na(script_path)) dirname(script_path) else getwd()
pipeline_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

default_distribution_folder <- first_existing_path(c(
  file.path(pipeline_root, "02_Input_Monthly_Distributions"),
  file.path("C:/Users/MLOUI/OneDrive/Work/PBI/PBI DATA FILES/ECE")
))
distribution_folder <- env_value(
  "ECE_DISTRIBUTION_FOLDER",
  default_distribution_folder
)
distribution_file_pattern <- env_value(
  "ECE_DISTRIBUTION_FILE_PATTERN",
  "^(ChildCareOcc_ESDC_LFS_Tables_\\d{6}|Monthly Distribution.*)\\.(xlsx|xls)$"
)

excel_workbook_folder <- env_value(
  "ECE_EXCEL_WORKBOOK_FOLDER",
  env_value(
    "ECE_POWERBI_WORKBOOK_FOLDER",
    file.path(pipeline_root, "03_Output_Excel")
  )
)
excel_workbook_path <- env_value(
  "ECE_EXCEL_WORKBOOK",
  env_value(
    "ECE_POWERBI_WORKBOOK",
    file.path(excel_workbook_folder, "Historical ECE data.xlsx")
  )
)
baseline_template_workbook_path <- env_value(
  "ECE_BASELINE_WORKBOOK_TEMPLATE",
  first_existing_path(c(
    file.path(
      "C:/Users/MLOUI/OneDrive/Work/PBI/PBI DATA FILES/ECE",
      "Historical ECE data.xlsx"
    ),
    file.path(
      "C:/Users/MLOUI/OneDrive/Work/PBI/PBI DATA FILES/ECE",
      "Historical ECE data - Dec 25.xlsx"
    )
  ))
)
backup_folder <- env_value(
  "ECE_BACKUP_FOLDER",
  file.path(pipeline_root, "04_Backups")
)

backup_existing_file <- env_flag("ECE_BACKUP_EXISTING_FILE", TRUE)
copy_template_if_missing <- env_flag("ECE_COPY_TEMPLATE_IF_MISSING", TRUE)

required_packages <- c(
  "readxl",
  "readr",
  "dplyr",
  "purrr",
  "stringr",
  "lubridate",
  "tibble",
  "openxlsx",
  "zip"
)

combined_noc_occupation <- "Combined NOCs: 42202 Early childhood educators and assistants; 44100 Home child care providers"

# ---------------------------------------------------
# Package check
# ---------------------------------------------------

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    "\nRun: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

# ---------------------------------------------------
# Helpers
# ---------------------------------------------------

extract_reference_period_from_filename <- function(path) {
  as.integer(str_extract(basename(path), "\\d{6}"))
}

find_distribution_files <- function(folder, pattern) {
  files <- list.files(
    path = folder,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    stop(
      "No monthly distribution workbooks found in: ",
      folder,
      "\nPattern: ",
      pattern,
      call. = FALSE
    )
  }

  tibble(
    path = files,
    filename_reference_period = extract_reference_period_from_filename(files),
    modified_time = file.info(files)$mtime
  ) |>
    arrange(filename_reference_period, modified_time, path) |>
    pull(path)
}

drop_empty_columns <- function(data) {
  data |>
    select(where(function(column) {
      values <- as.character(column)
      !all(is.na(values) | str_trim(values) == "")
    }))
}

repair_excel_names <- function(names) {
  missing_names <- is.na(names) | names == ""
  names[missing_names] <- paste0("blank_", which(missing_names))
  make.unique(names, sep = "...")
}

canonicalize_header <- function(header) {
  header |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_replace("\\.{3}\\d+$", "") |>
    str_replace("^Estimate \\(rounded\\)\\d*$", "Estimate (rounded)") |>
    str_replace(
      "^Coefficient of variation \\(%\\) of estimate\\d*$",
      "Coefficient of variation (%) of estimate"
    ) |>
    str_replace(
      "^Lower bound \\(95% CI\\) of estimate\\d*$",
      "Lower bound (95% CI) of estimate"
    ) |>
    str_replace(
      "^Upper bound \\(95% CI\\) of estimate\\d*$",
      "Upper bound (95% CI) of estimate"
    )
}

rename_first_match <- function(data, new_name, aliases) {
  if (new_name %in% names(data)) {
    return(data)
  }

  match_index <- which(names(data) %in% aliases)

  if (length(match_index) == 0) {
    return(data)
  }

  names(data)[match_index[1]] <- new_name
  data
}

rename_to_internal_names <- function(data) {
  names(data) <- canonicalize_header(names(data))

  aliases <- list(
    reference_period = c("reference_period", "Reference Period"),
    reference_month = c("reference_month", "reference_date", "Reference Month"),
    province = c("province", "Province"),
    occupation_5_digit_noc = c(
      "occupation_5_digit_noc",
      "occupation_5digit_noc",
      "Occupation (5-digit NOC)"
    ),
    variable = c("variable", "Variable"),
    estimate_rounded = c("estimate_rounded", "estimate", "Estimate (rounded)"),
    coefficient_of_variation_pct = c(
      "coefficient_of_variation_pct",
      "coefficient_of_variation_percent",
      "Coefficient of variation (%) of estimate"
    ),
    standard_error = c("standard_error", "Standard error of estimate"),
    lower_bound_95_ci = c(
      "lower_bound_95_ci",
      "Lower bound (95% CI) of estimate"
    ),
    upper_bound_95_ci = c(
      "upper_bound_95_ci",
      "Upper bound (95% CI) of estimate"
    ),
    source_table = c("source_table", "Source Table"),
    series_type = c("series_type", "Series Type"),
    moving_average_months = c(
      "moving_average_months",
      "Moving Average Months"
    ),
    source_file = c("source_file", "Source File"),
    loaded_at = c("loaded_at", "Loaded At")
  )

  reduce2(
    .x = names(aliases),
    .y = aliases,
    .init = data,
    .f = rename_first_match
  )
}

parse_number_safely <- function(value) {
  suppressWarnings(parse_number(as.character(value)))
}

parse_reference_period <- function(value) {
  reference_period <- as.integer(str_extract(as.character(value), "\\d{6}"))
  reference_month <- reference_period %% 100

  if_else(
    !is.na(reference_period) & reference_month >= 1L & reference_month <= 12L,
    reference_period,
    NA_integer_
  )
}

reference_period_to_month <- function(reference_period) {
  year_value <- reference_period %/% 100
  month_value <- reference_period %% 100

  ymd(sprintf("%04d-%02d-01", year_value, month_value))
}

format_period_label <- function(reference_period) {
  format(reference_period_to_month(reference_period), "%B %Y")
}

update_period_range_text <- function(text, latest_reference_period) {
  if (is.na(text) || is.na(latest_reference_period)) {
    return(text)
  }

  str_replace(
    text,
    "January 2019 to [A-Za-z]+ \\d{4}",
    paste("January 2019 to", format_period_label(latest_reference_period))
  )
}

standardize_numeric_columns <- function(data) {
  numeric_columns <- c(
    "estimate_rounded",
    "coefficient_of_variation_pct",
    "standard_error",
    "lower_bound_95_ci",
    "upper_bound_95_ci",
    "moving_average_months"
  )

  for (column in intersect(numeric_columns, names(data))) {
    data[[column]] <- parse_number_safely(data[[column]])
  }

  data
}

add_missing_column <- function(data, column_name, value = NA) {
  if (!column_name %in% names(data)) {
    data[[column_name]] <- value
  }

  data
}

select_output_columns <- function(data) {
  output_columns <- c(
    "reference_period",
    "reference_month",
    "province",
    "occupation_5_digit_noc",
    "variable",
    "estimate_rounded",
    "coefficient_of_variation_pct",
    "standard_error",
    "lower_bound_95_ci",
    "upper_bound_95_ci",
    "source_table",
    "series_type",
    "moving_average_months",
    "source_file",
    "loaded_at"
  )

  for (column in output_columns) {
    data <- add_missing_column(data, column)
  }

  data |>
    select(all_of(output_columns))
}

source_table_id <- function(source_table) {
  case_when(
    str_detect(source_table, regex("^Table 1", ignore_case = TRUE)) ~ "Table 1",
    str_detect(source_table, regex("^Table 2", ignore_case = TRUE)) ~ "Table 2",
    str_detect(source_table, regex("^Table 3", ignore_case = TRUE)) ~ "Table 3",
    TRUE ~ source_table
  )
}

source_sheet_name <- function(table_id) {
  case_when(
    table_id == "Table 1" ~ "Table 1 - monthly combined NOCs",
    table_id == "Table 2" ~ "Table 2 - 3MMA separate NOCs",
    table_id == "Table 3" ~ "Table 3 - monthly separate NOCs",
    TRUE ~ table_id
  )
}

normalized_series_type <- function(table_id) {
  case_when(
    table_id == "Table 1" ~ "monthly_combined_nocs",
    table_id == "Table 2" ~ "three_month_moving_average_separate_nocs",
    table_id == "Table 3" ~ "monthly_separate_nocs",
    TRUE ~ str_to_lower(str_replace_all(table_id, "[^A-Za-z0-9]+", "_"))
  )
}

# ---------------------------------------------------
# Read the StatCan distribution workbook
# ---------------------------------------------------

table_definitions <- tribble(
  ~sheet_pattern, ~source_table, ~series_type, ~moving_average_months, ~default_occupation,
  "^Table 1", "Table 1", "monthly_combined_nocs", 1L, combined_noc_occupation,
  "^Table 2", "Table 2", "three_month_moving_average_separate_nocs", 3L, NA_character_,
  "^Table 3", "Table 3", "monthly_separate_nocs", 1L, NA_character_
)

match_sheet_name <- function(available_sheets, sheet_pattern) {
  matches <- available_sheets[str_detect(available_sheets, regex(sheet_pattern, ignore_case = TRUE))]

  if (length(matches) == 0) {
    stop(
      "Could not find a sheet matching pattern: ",
      sheet_pattern,
      "\nAvailable sheets: ",
      paste(available_sheets, collapse = ", "),
      call. = FALSE
    )
  }

  matches[1]
}

read_distribution_sheet <- function(workbook_path, table_definition) {
  available_sheets <- excel_sheets(workbook_path)
  sheet_name <- match_sheet_name(available_sheets, table_definition$sheet_pattern)

  raw_data <- read_excel(
    path = workbook_path,
    sheet = sheet_name,
    skip = 2,
    col_types = "text",
    .name_repair = repair_excel_names
  )

  raw_data |>
    drop_empty_columns() |>
    rename_to_internal_names() |>
    mutate(
      source_table = table_definition$source_table,
      series_type = table_definition$series_type,
      moving_average_months = table_definition$moving_average_months,
      source_file = basename(workbook_path),
      loaded_at = as.character(Sys.time())
    ) |>
    add_missing_column("occupation_5_digit_noc", table_definition$default_occupation) |>
    mutate(
      occupation_5_digit_noc = if_else(
        is.na(occupation_5_digit_noc) | str_trim(occupation_5_digit_noc) == "",
        table_definition$default_occupation,
        occupation_5_digit_noc
      ),
      reference_period = parse_reference_period(reference_period)
    ) |>
    standardize_numeric_columns() |>
    filter(!is.na(reference_period)) |>
    mutate(reference_month = reference_period_to_month(reference_period)) |>
    select_output_columns()
}

read_monthly_distribution <- function(workbook_path) {
  pmap_dfr(
    table_definitions,
    function(sheet_pattern, source_table, series_type, moving_average_months, default_occupation) {
      read_distribution_sheet(
        workbook_path = workbook_path,
        table_definition = tibble(
          sheet_pattern = sheet_pattern,
          source_table = source_table,
          series_type = series_type,
          moving_average_months = moving_average_months,
          default_occupation = default_occupation
        )
      )
    }
  )
}

# ---------------------------------------------------
# Read and standardize the existing historical workbook
# ---------------------------------------------------

read_existing_workbook_sheet <- function(workbook_path, table_definition) {
  available_sheets <- excel_sheets(workbook_path)
  sheet_name <- source_sheet_name(table_definition$source_table)

  if (!sheet_name %in% available_sheets) {
    sheet_name <- match_sheet_name(available_sheets, table_definition$sheet_pattern)
  }

  raw_data <- read_excel(
    path = workbook_path,
    sheet = sheet_name,
    skip = 2,
    col_types = "text",
    .name_repair = repair_excel_names
  )

  raw_data |>
    drop_empty_columns() |>
    rename_to_internal_names() |>
    mutate(
      source_table = table_definition$source_table,
      series_type = table_definition$series_type,
      moving_average_months = table_definition$moving_average_months,
      source_file = basename(workbook_path),
      loaded_at = NA_character_
    ) |>
    add_missing_column("occupation_5_digit_noc", table_definition$default_occupation) |>
    mutate(
      occupation_5_digit_noc = if_else(
        is.na(occupation_5_digit_noc) | str_trim(occupation_5_digit_noc) == "",
        table_definition$default_occupation,
        occupation_5_digit_noc
      ),
      source_table = source_table_id(source_table),
      reference_period = parse_reference_period(reference_period)
    ) |>
    standardize_numeric_columns() |>
    filter(!is.na(reference_period)) |>
    mutate(reference_month = reference_period_to_month(reference_period)) |>
    select_output_columns()
}

read_existing_history <- function(workbook_path) {
  if (!file.exists(workbook_path)) {
    return(select_output_columns(tibble()))
  }

  pmap_dfr(
    table_definitions,
    function(sheet_pattern, source_table, series_type, moving_average_months, default_occupation) {
      read_existing_workbook_sheet(
        workbook_path = workbook_path,
        table_definition = tibble(
          sheet_pattern = sheet_pattern,
          source_table = source_table,
          series_type = series_type,
          moving_average_months = moving_average_months,
          default_occupation = default_occupation
        )
      )
    }
  )
}

# ---------------------------------------------------
# Workbook metadata and formatting
# ---------------------------------------------------

default_sheet_configs <- tribble(
  ~source_table, ~sheet_name, ~table_name, ~max_cols,
  "Table 1", "Table 1 - monthly combined NOCs", "Table1", 8L,
  "Table 2", "Table 2 - 3MMA separate NOCs", "Table2", 10L,
  "Table 3", "Table 3 - monthly separate NOCs", "Table3", 9L
)

read_sheet_metadata <- function(workbook_path, sheet_name, max_cols) {
  raw <- read_excel(
    path = workbook_path,
    sheet = sheet_name,
    col_names = FALSE,
    col_types = "text",
    .name_repair = repair_excel_names
  )

  first_column <- raw[[1]]
  data_rows <- which(!is.na(parse_reference_period(first_column)))
  last_data_row <- if (length(data_rows) > 0) max(data_rows) else 3L

  footnote_rows <- if (last_data_row < nrow(raw)) {
    text <- raw[[1]][seq.int(last_data_row + 1L, nrow(raw))]
    text[!is.na(text) & str_trim(text) != ""]
  } else {
    character()
  }

  list(
    title = raw[[1]][1],
    footnotes = footnote_rows,
    max_cols = max_cols
  )
}

read_workbook_metadata <- function(workbook_path) {
  configs <- default_sheet_configs |>
    mutate(
      metadata = pmap(
        list(sheet_name, max_cols),
        function(sheet_name, max_cols) {
          read_sheet_metadata(workbook_path, sheet_name, max_cols)
        }
      )
    )

  set_names(configs$metadata, configs$source_table)
}

display_columns_for_table <- function(source_table) {
  if (source_table == "Table 1") {
    return(c(
      reference_period = "Reference Period",
      province = "Province",
      variable = "Variable",
      estimate_rounded = "Estimate (rounded)5",
      coefficient_of_variation_pct = "Coefficient of\nvariation (%) of\nestimate6",
      standard_error = "Standard error of\nestimate",
      lower_bound_95_ci = "Lower bound (95%\nCI) of estimate",
      upper_bound_95_ci = "Upper bound (95%\nCI) of estimate"
    ))
  }

  if (source_table == "Table 2") {
    return(c(
      reference_period = "Reference Period",
      province = "Province",
      occupation_5_digit_noc = "Occupation (5-digit NOC)",
      variable = "Variable",
      estimate_rounded = "Estimate (rounded)5",
      coefficient_of_variation_pct = "Coefficient of\nvariation (%) of\nestimate6",
      standard_error = "Standard error of\nestimate",
      lower_bound_95_ci = "Lower bound (95%\nCI) of estimate",
      upper_bound_95_ci = "Upper bound (95%\nCI) of estimate",
      blank_column = "Column1"
    ))
  }

  c(
    reference_period = "Reference Period",
    province = "Province",
    occupation_5_digit_noc = "Occupation (5-digit NOC)",
    variable = "Variable",
    estimate_rounded = "Estimate (rounded)4",
    coefficient_of_variation_pct = "Coefficient of\nvariation (%) of\nestimate5",
    standard_error = "Standard error of\nestimate",
    lower_bound_95_ci = "Lower bound (95%\nCI) of estimate",
    upper_bound_95_ci = "Upper bound (95%\nCI) of estimate"
  )
}

format_for_workbook_sheet <- function(data, source_table) {
  display_columns <- display_columns_for_table(source_table)

  output <- data |>
    filter(.data$source_table == .env$source_table) |>
    arrange(
      reference_period,
      province,
      occupation_5_digit_noc,
      variable
    )

  if ("blank_column" %in% names(display_columns)) {
    output <- mutate(output, blank_column = NA_character_)
  }

  output <- output |>
    select(all_of(names(display_columns)))

  names(output) <- unname(display_columns)
  output
}

sheet_column_widths <- function(source_table) {
  if (source_table == "Table 1") {
    return(c(21.54, 24.45, 22.54, 21.54, 17.54, 19.54, 21.54, 21.54))
  }

  if (source_table == "Table 2") {
    return(c(17.36, 22.54, 41.54, 19.54, 19.45, 17.54, 19.54, 21.54, 21.54, 11.45))
  }

  c(17.36, 19.45, 43.54, 13.54, 21.54, 17.54, 19.54, 21.54, 21.54)
}

apply_sheet_styles <- function(wb, sheet_name, data_rows, source_table, max_cols) {
  title_style <- createStyle(
    fontName = "Arial",
    fontSize = 11,
    textDecoration = "bold",
    wrapText = TRUE,
    valign = "top"
  )
  body_style <- createStyle(
    fontName = "Arial",
    fontSize = 9.5,
    valign = "center"
  )
  wrap_style <- createStyle(
    fontName = "Arial",
    fontSize = 9.5,
    wrapText = TRUE,
    valign = "top"
  )
  ref_period_style <- createStyle(numFmt = "0")
  count_style <- createStyle(numFmt = "#,##0")
  decimal_style <- createStyle(numFmt = "#,##0.0")

  addStyle(wb, sheet_name, title_style, rows = 1, cols = 1:max_cols, gridExpand = TRUE)
  setRowHeights(wb, sheet_name, rows = 1, heights = 27)
  setRowHeights(wb, sheet_name, rows = 3, heights = 43.25)
  setColWidths(wb, sheet_name, cols = 1:max_cols, widths = sheet_column_widths(source_table))

  if (data_rows > 0) {
    table_rows <- 4:(data_rows + 3L)
    addStyle(wb, sheet_name, body_style, rows = table_rows, cols = 1:max_cols, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sheet_name, ref_period_style, rows = table_rows, cols = 1, stack = TRUE)

    if (source_table == "Table 1") {
      addStyle(wb, sheet_name, decimal_style, rows = table_rows, cols = 4:8, gridExpand = TRUE, stack = TRUE)
    } else {
      addStyle(wb, sheet_name, count_style, rows = table_rows, cols = 5:max_cols, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet_name, decimal_style, rows = table_rows, cols = 6, stack = TRUE)
    }
  }

  invisible(wrap_style)
}

write_sheet_footnotes <- function(wb, sheet_name, footnotes, start_row, max_cols, latest_reference_period) {
  footnote_style <- createStyle(
    fontName = "Arial",
    fontSize = 9.5,
    wrapText = TRUE,
    valign = "top"
  )
  merge_cols <- 1:min(8L, max_cols)
  current_row <- start_row

  for (note in footnotes) {
    note <- update_period_range_text(note, latest_reference_period)
    writeData(wb, sheet_name, note, startRow = current_row, startCol = 1, colNames = FALSE)
    mergeCells(wb, sheet_name, rows = current_row, cols = merge_cols)
    addStyle(wb, sheet_name, footnote_style, rows = current_row, cols = merge_cols, gridExpand = TRUE)

    height <- if_else(nchar(note) > 230, 56, if_else(nchar(note) > 120, 34, 18))
    setRowHeights(wb, sheet_name, rows = current_row, heights = height)
    current_row <- current_row + 1L
  }
}

assert_workbook_available <- function(workbook_path) {
  if (!file.exists(workbook_path)) {
    return(invisible(TRUE))
  }

  connection <- tryCatch(
    file(workbook_path, open = "r+b"),
    error = function(error) NULL
  )

  if (is.null(connection)) {
    stop(
      "Historical ECE data workbook appears to be open or locked:\n",
      workbook_path,
      "\nClose the workbook in Excel and rerun this script.",
      call. = FALSE
    )
  }

  close(connection)
  invisible(TRUE)
}

clean_generated_xlsx_package <- function(workbook_path) {
  temp_dir <- tempfile("xlsx_clean_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(workbook_path, exdir = temp_dir)

  rel_files <- list.files(
    file.path(temp_dir, "xl", "worksheets", "_rels"),
    pattern = "\\.xml\\.rels$",
    full.names = TRUE
  )

  for (rel_file in rel_files) {
    rel_text <- paste(readLines(rel_file, warn = FALSE), collapse = "")
    rel_text <- str_replace_all(
      rel_text,
      '<Relationship[^>]+Type="[^"]+/(drawing|vmlDrawing|printerSettings)"[^>]*/>',
      ""
    )
    writeLines(rel_text, rel_file, useBytes = TRUE)
  }

  sheet_files <- list.files(
    file.path(temp_dir, "xl", "worksheets"),
    pattern = "^sheet\\d+\\.xml$",
    full.names = TRUE
  )

  for (sheet_file in sheet_files) {
    sheet_text <- paste(readLines(sheet_file, warn = FALSE), collapse = "")
    sheet_text <- str_replace_all(sheet_text, '<drawing[^>]*/>', "")
    sheet_text <- str_replace_all(sheet_text, '<legacyDrawing[^>]*/>', "")
    sheet_text <- str_replace_all(sheet_text, '(<pageSetup[^>]*)\\s+r:id="[^"]+"', "\\1")
    writeLines(sheet_text, sheet_file, useBytes = TRUE)
  }

  cleaned_workbook_path <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(workbook_path)), "_clean_"),
    fileext = ".xlsx"
  )
  on.exit(unlink(cleaned_workbook_path), add = TRUE)

  old_working_directory <- getwd()
  on.exit(setwd(old_working_directory), add = TRUE)
  setwd(temp_dir)

  package_files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  zip::zipr(cleaned_workbook_path, files = package_files, root = ".", mode = "mirror")
  copied <- file.copy(cleaned_workbook_path, workbook_path, overwrite = TRUE)

  if (!isTRUE(copied)) {
    stop("Could not clean workbook package: ", workbook_path, call. = FALSE)
  }

  workbook_path
}

write_history_workbook <- function(data, workbook_path, metadata) {
  dir.create(dirname(workbook_path), recursive = TRUE, showWarnings = FALSE)

  latest_reference_period <- max(data$reference_period, na.rm = TRUE)
  wb <- createWorkbook()

  pwalk(
    default_sheet_configs,
    function(source_table, sheet_name, table_name, max_cols) {
      sheet_metadata <- metadata[[source_table]]
      display_data <- format_for_workbook_sheet(data, source_table)
      title <- update_period_range_text(sheet_metadata$title, latest_reference_period)

      addWorksheet(wb, sheetName = sheet_name, gridLines = FALSE)
      writeData(wb, sheet_name, title, startRow = 1, startCol = 1, colNames = FALSE)
      mergeCells(wb, sheet_name, rows = 1, cols = 1:max_cols)

      writeDataTable(
        wb,
        sheet = sheet_name,
        x = display_data,
        startRow = 3,
        startCol = 1,
        tableName = table_name,
        tableStyle = "TableStyleMedium2",
        withFilter = TRUE,
        keepNA = FALSE
      )

      apply_sheet_styles(
        wb = wb,
        sheet_name = sheet_name,
        data_rows = nrow(display_data),
        source_table = source_table,
        max_cols = max_cols
      )

      write_sheet_footnotes(
        wb = wb,
        sheet_name = sheet_name,
        footnotes = sheet_metadata$footnotes,
        start_row = nrow(display_data) + 5L,
        max_cols = max_cols,
        latest_reference_period = latest_reference_period
      )
    }
  )

  temp_workbook_path <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(workbook_path)), "_"),
    fileext = ".xlsx"
  )
  on.exit(unlink(temp_workbook_path), add = TRUE)

  saveWorkbook(wb, temp_workbook_path, overwrite = TRUE)
  clean_generated_xlsx_package(temp_workbook_path)

  if (file.exists(workbook_path)) {
    assert_workbook_available(workbook_path)
    unlink(workbook_path)
  }

  if (file.exists(workbook_path)) {
    stop(
      "Historical ECE data workbook could not be replaced, likely because it is open or locked:\n",
      workbook_path,
      "\nClose the workbook in Excel and rerun this script.",
      call. = FALSE
    )
  }

  copied <- file.copy(temp_workbook_path, workbook_path, overwrite = FALSE)

  if (!isTRUE(copied)) {
    stop(
      "Historical ECE data workbook could not be replaced:\n",
      workbook_path,
      "\nClose the workbook in Excel and rerun this script.",
      call. = FALSE
    )
  }

  workbook_path
}

# ---------------------------------------------------
# Merge and write history
# ---------------------------------------------------

deduplicate_history <- function(data) {
  key_columns <- c(
    "source_table",
    "reference_period",
    "province",
    "occupation_5_digit_noc",
    "variable"
  )

  data |>
    group_by(across(all_of(key_columns))) |>
    slice_tail(n = 1) |>
    ungroup()
}

backup_file <- function(file_path, backup_directory = dirname(file_path)) {
  if (!file.exists(file_path)) {
    return(invisible(NULL))
  }

  dir.create(backup_directory, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(
    backup_directory,
    paste0(tools::file_path_sans_ext(basename(file_path)), " backup ", timestamp, ".xlsx")
  )

  file.copy(file_path, backup_path, overwrite = FALSE)
  message("Backup created: ", backup_path)
}

ensure_baseline_workbook <- function(workbook_path, template_path, copy_if_missing = TRUE) {
  if (file.exists(workbook_path)) {
    return(workbook_path)
  }

  if (!copy_if_missing) {
    stop("Historical workbook does not exist: ", workbook_path, call. = FALSE)
  }

  if (!file.exists(template_path)) {
    stop(
      "Historical workbook does not exist: ",
      workbook_path,
      "\nBaseline template was not found: ",
      template_path,
      call. = FALSE
    )
  }

  dir.create(dirname(workbook_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(template_path, workbook_path, overwrite = FALSE)
  message("Baseline workbook copied to: ", workbook_path)
  workbook_path
}

update_monthly_history <- function(
  distribution_paths,
  baseline_workbook_path,
  baseline_template_path,
  backup_directory,
  backup_existing = TRUE,
  copy_template = TRUE
) {
  baseline_workbook_path <- ensure_baseline_workbook(
    workbook_path = baseline_workbook_path,
    template_path = baseline_template_path,
    copy_if_missing = copy_template
  )
  assert_workbook_available(baseline_workbook_path)

  workbook_metadata <- read_workbook_metadata(baseline_workbook_path)
  existing_history <- read_existing_history(baseline_workbook_path)
  monthly_distribution <- map_dfr(distribution_paths, read_monthly_distribution)

  # Re-running the script will not duplicate existing data. Rows are unique at the
  # table, month, province, occupation, and variable grain; if a repeated row is
  # found, the last version read from the distribution workbooks is kept.
  updated_history <- bind_rows(existing_history, monthly_distribution) |>
    deduplicate_history()

  if (backup_existing && file.exists(baseline_workbook_path)) {
    backup_file(baseline_workbook_path, backup_directory)
  }

  written_workbook_path <- write_history_workbook(
    data = updated_history,
    workbook_path = baseline_workbook_path,
    metadata = workbook_metadata
  )

  list(
    distribution_paths = distribution_paths,
    baseline_workbook_path = written_workbook_path,
    existing_rows = nrow(existing_history),
    monthly_rows = nrow(monthly_distribution),
    output_rows = nrow(updated_history),
    reference_periods_added = sort(unique(monthly_distribution$reference_period)),
    output_rows_by_source_table = updated_history |>
      count(source_table, series_type, moving_average_months, name = "rows")
  )
}

# ---------------------------------------------------
# Run
# ---------------------------------------------------

monthly_distribution_paths <- find_distribution_files(
  folder = distribution_folder,
  pattern = distribution_file_pattern
)

result <- update_monthly_history(
  distribution_paths = monthly_distribution_paths,
  baseline_workbook_path = excel_workbook_path,
  baseline_template_path = baseline_template_workbook_path,
  backup_directory = backup_folder,
  backup_existing = backup_existing_file,
  copy_template = copy_template_if_missing
)

message("Distribution folder: ", distribution_folder)
message("Distribution workbooks read: ", length(result$distribution_paths))
message("Distribution workbook names:")
for (path in result$distribution_paths) {
  message("  - ", basename(path))
}
message("Historical workbook updated: ", result$baseline_workbook_path)
message("Backup folder: ", backup_folder)
message("Existing rows read: ", result$existing_rows)
message("Monthly rows read: ", result$monthly_rows)
message("Output rows after dedupe: ", result$output_rows)
message("Output rows by source table:")
for (row_index in seq_len(nrow(result$output_rows_by_source_table))) {
  row <- result$output_rows_by_source_table[row_index, ]
  message(
    "  - ",
    row$source_table,
    " / ",
    row$series_type,
    " / ",
    row$moving_average_months,
    " months: ",
    row$rows
  )
}
message(
  "Reference periods loaded from workbook: ",
  paste(result$reference_periods_added, collapse = ", ")
)
