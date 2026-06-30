library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)

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

distribution_folder <- env_value(
  "ECE_DISTRIBUTION_FOLDER",
  file.path(pipeline_root, "02_Input_Monthly_Distributions")
)
distribution_file_pattern <- env_value(
  "ECE_DISTRIBUTION_FILE_PATTERN",
  "^(ChildCareOcc_ESDC_LFS_Tables_\\d{6}|Monthly Distribution.*)\\.(xlsx|xls)$"
)

historical_csv_path <- env_value(
  "ECE_HISTORICAL_CSV",
  file.path(pipeline_root, "03_Output_PowerBI", "Monthly Historical ECE data.csv")
)
output_csv_path <- env_value(
  "ECE_OUTPUT_CSV",
  file.path(pipeline_root, "03_Output_PowerBI", "Monthly Historical ECE data - full.csv")
)
backup_folder <- env_value(
  "ECE_BACKUP_FOLDER",
  file.path(pipeline_root, "04_Backups")
)

# Set to TRUE after validating the new full output in Power BI.
# When TRUE, the script writes back to historical_csv_path and creates a backup first.
overwrite_historical_file <- env_flag("ECE_OVERWRITE_HISTORICAL_FILE", FALSE)
backup_existing_file <- env_flag("ECE_BACKUP_EXISTING_FILE", TRUE)

required_packages <- c(
  "readxl",
  "readr",
  "dplyr",
  "purrr",
  "stringr",
  "lubridate",
  "tibble"
)

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

canonicalize_header <- function(header) {
  header |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
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
    reference_month = c("reference_month", "Reference Month"),
    province = c("province", "Province"),
    occupation_5_digit_noc = c(
      "occupation_5_digit_noc",
      "Occupation (5-digit NOC)"
    ),
    variable = c("variable", "Variable"),
    estimate_rounded = c("estimate_rounded", "Estimate (rounded)"),
    coefficient_of_variation_pct = c(
      "coefficient_of_variation_pct",
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
  as.integer(str_extract(as.character(value), "\\d{6}"))
}

reference_period_to_month <- function(reference_period) {
  year_value <- reference_period %/% 100
  month_value <- reference_period %% 100

  ymd(sprintf("%04d-%02d-01", year_value, month_value))
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

# ---------------------------------------------------
# Read the StatCan distribution workbook
# ---------------------------------------------------

table_definitions <- tribble(
  ~sheet_pattern, ~source_table, ~series_type, ~moving_average_months, ~default_occupation,
  "^Table 1", "Table 1 - monthly combined N", "Monthly combined NOCs", 1L, "Combined NOCs 42202 and 44100",
  "^Table 2", "Table 2 - 3MMA separate NOCs", "3MMA separate NOCs", 3L, NA_character_,
  "^Table 3", "Table 3 - monthly separate N", "Monthly separate NOCs", 1L, NA_character_
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
    .name_repair = "minimal"
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
      reference_period = parse_reference_period(reference_period),
      reference_month = reference_period_to_month(reference_period)
    ) |>
    standardize_numeric_columns() |>
    filter(!is.na(reference_period)) |>
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
# Read and standardize the existing historical file
# ---------------------------------------------------

read_existing_history <- function(csv_path) {
  if (!file.exists(csv_path)) {
    return(select_output_columns(tibble()))
  }

  read_csv(
    file = csv_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    drop_empty_columns() |>
    rename_to_internal_names() |>
    add_missing_column("reference_month") |>
    add_missing_column("source_table") |>
    add_missing_column("series_type") |>
    add_missing_column("moving_average_months") |>
    add_missing_column("source_file") |>
    add_missing_column("loaded_at") |>
    mutate(
      source_table = coalesce(source_table, "Table 2 - 3MMA separate NOCs"),
      series_type = coalesce(series_type, "3MMA separate NOCs"),
      moving_average_months = coalesce(moving_average_months, "3"),
      source_file = coalesce(source_file, basename(csv_path)),
      loaded_at = coalesce(loaded_at, NA_character_),
      reference_period = parse_reference_period(reference_period)
    ) |>
    mutate(
      reference_month = if_else(
        is.na(reference_month) | str_trim(as.character(reference_month)) == "",
        as.character(reference_period_to_month(reference_period)),
        as.character(reference_month)
      )
    ) |>
    standardize_numeric_columns() |>
    mutate(reference_month = ymd(reference_month)) |>
    filter(!is.na(reference_period)) |>
    select_output_columns()
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

format_for_power_bi_csv <- function(data) {
  output <- data |>
    arrange(
      reference_period,
      source_table,
      province,
      occupation_5_digit_noc,
      variable
    ) |>
    select_output_columns()

  names(output) <- c(
    "Reference Period",
    "Reference Month",
    "Province",
    "Occupation (5-digit NOC)",
    "Variable",
    "Estimate (rounded)",
    "Coefficient of variation (%) of estimate",
    "Standard error of estimate",
    "Lower bound (95% CI) of estimate",
    "Upper bound (95% CI) of estimate",
    "Source Table",
    "Series Type",
    "Moving Average Months",
    "Source File",
    "Loaded At"
  )

  output
}

backup_file <- function(file_path, backup_directory = dirname(file_path)) {
  if (!file.exists(file_path)) {
    return(invisible(NULL))
  }

  dir.create(backup_directory, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(
    backup_directory,
    paste0(tools::file_path_sans_ext(basename(file_path)), " backup ", timestamp, ".csv")
  )

  file.copy(file_path, backup_path, overwrite = FALSE)
  message("Backup created: ", backup_path)
}

update_monthly_history <- function(
  distribution_paths,
  historical_path,
  output_path,
  backup_directory,
  backup_existing = TRUE
) {
  existing_history <- read_existing_history(historical_path)
  monthly_distribution <- map_dfr(distribution_paths, read_monthly_distribution)

  # Re-running the script will not duplicate existing data. Rows are unique at the
  # table, month, province, occupation, and variable grain; if a repeated row is
  # found, the last version read from the distribution workbooks is kept.
  updated_history <- bind_rows(existing_history, monthly_distribution) |>
    deduplicate_history()

  if (backup_existing && file.exists(output_path)) {
    backup_file(output_path, backup_directory)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  write_excel_csv(
    format_for_power_bi_csv(updated_history),
    output_path,
    na = ""
  )

  list(
    distribution_paths = distribution_paths,
    historical_path = historical_path,
    output_path = output_path,
    existing_rows = nrow(existing_history),
    monthly_rows = nrow(monthly_distribution),
    output_rows = nrow(updated_history),
    reference_periods_added = sort(unique(monthly_distribution$reference_period))
  )
}

# ---------------------------------------------------
# Run
# ---------------------------------------------------

monthly_distribution_paths <- find_distribution_files(
  folder = distribution_folder,
  pattern = distribution_file_pattern
)

final_output_path <- if (isTRUE(overwrite_historical_file)) {
  historical_csv_path
} else {
  output_csv_path
}

result <- update_monthly_history(
  distribution_paths = monthly_distribution_paths,
  historical_path = historical_csv_path,
  output_path = final_output_path,
  backup_directory = backup_folder,
  backup_existing = backup_existing_file
)

message("Distribution folder: ", distribution_folder)
message("Distribution workbooks read: ", length(result$distribution_paths))
message("Distribution workbook names:")
for (path in result$distribution_paths) {
  message("  - ", basename(path))
}
message("Historical source: ", result$historical_path)
message("Output written: ", result$output_path)
message("Backup folder: ", backup_folder)
message("Existing rows read: ", result$existing_rows)
message("Monthly rows read: ", result$monthly_rows)
message("Output rows after dedupe: ", result$output_rows)
message(
  "Reference periods loaded from workbook: ",
  paste(result$reference_periods_added, collapse = ", ")
)
