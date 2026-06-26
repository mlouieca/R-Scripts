# ===================================================
# U.S. CPS Basic Monthly Labour Summary Pipeline
# ===================================================
#
# Purpose:
#   Pull U.S. Census CPS Basic Monthly microdata,
#   identify co-resident parents using CPS parent pointers,
#   and create six labour-market summary rows per month:
#
#   Women / Men ×
#     - All core-aged adults
#     - Co-resident child aged 0–5
#     - No co-resident child under 18
#
# Output:
#   A monthly summary dataset suitable for Power BI
#   and a monthly load log for validation/audit purposes.
#
# ===================================================


# ===================================================
# 1. Packages
# ===================================================

# Run this once only if any package is missing:
#
# install.packages(c(
#   "httr2",
#   "jsonlite",
#   "dplyr",
#   "tidyr",
#   "lubridate",
#   "tibble"
# ))

library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(tibble)


# ===================================================
# 2. Configuration
# ===================================================

# Option A:
# Paste your personal Census API key below.
#
# Do not use the API key contained in the old team
# documentation. Treat that key as exposed.
#
census_api_key <- "1a8e3aa3d88e871469e4ee867856fd6b9dc2a672"


# Option B:
# A more secure approach later is to store the key as a
# Windows environment variable and use:
#
# census_api_key <- Sys.getenv("CENSUS_API_KEY")
#
# For now, Option A is simpler while testing.


# Folder where completed CSV files will be saved.
#
# "~" usually means your user home folder.
# This will typically create a CPS_Output folder in your
# user directory.
#
output_folder <- "~/CPS_Output"


# ===================================================
# 3. Download one CPS Basic Monthly file
# ===================================================

get_cps_month <- function(
    reference_month,
    api_key = census_api_key
) {
  
  reference_month <- as.Date(reference_month)
  
  year_text <- format(reference_month, "%Y")
  month_text <- tolower(format(reference_month, "%b"))
  
  base_url <- paste0(
    "https://api.census.gov/data/",
    year_text,
    "/cps/basic/",
    month_text
  )
  
  # CPS parent-pointer variables changed after 2019.
  #
  # 2019:
  #   PELNDAD = father person-line pointer
  #   PELNMOM = mother person-line pointer
  #
  # 2020 onward:
  #   PEPAR1 / PEPAR2 = parent person-line pointers
  #
  is_legacy_schema <- year(reference_month) < 2020
  
  core_variables <- c(
    "HRHHID",
    "HRHHID2",
    "HRYEAR4",
    "HRMONTH",
    "PULINENO",
    "PRTAGE",
    "PESEX",
    "PEMLR",
    "PWSSWGT",
    "PWCMPWGT"
  )
  
  parent_variables <- if (is_legacy_schema) {
    c("PELNDAD", "PELNMOM")
  } else {
    c("PEPAR1", "PEPAR2")
  }
  
  requested_variables <- c(
    core_variables,
    parent_variables
  )
  
  response <- request(base_url) |>
    req_url_query(
      get = paste(requested_variables, collapse = ","),
      key = api_key
    ) |>
    req_perform()
  
  raw_json <- resp_body_string(response)
  
  raw_data <- fromJSON(raw_json)
  
  cps_data <- as.data.frame(
    raw_data[-1, , drop = FALSE],
    stringsAsFactors = FALSE
  )
  
  names(cps_data) <- tolower(raw_data[1, ])
  
  # Standardize legacy 2019 parent pointer names to the
  # modern names used by the rest of the script.
  if (is_legacy_schema) {
    cps_data <- cps_data |>
      rename(
        pepar1 = pelndad,
        pepar2 = pelnmom
      )
  }
  
  cps_data <- cps_data |>
    mutate(
      ReferenceMonth = reference_month,
      
      SourceFile = paste0(
        month_text,
        substr(year_text, 3, 4),
        " CPS API"
      ),
      
      across(
        c(
          hryear4,
          hrmonth,
          pulineno,
          prtage,
          pesex,
          pemlr,
          pwsswgt,
          pwcmpwgt,
          pepar1,
          pepar2
        ),
        ~ suppressWarnings(as.numeric(.x))
      ),
      
      across(
        c(
          hrhhid,
          hrhhid2
        ),
        as.character
      )
    ) |>
    select(
      ReferenceMonth,
      SourceFile,
      hrhhid,
      hrhhid2,
      hryear4,
      hrmonth,
      pulineno,
      prtage,
      pesex,
      pemlr,
      pwsswgt,
      pwcmpwgt,
      pepar1,
      pepar2
    )
  
  return(cps_data)
}


# ===================================================
# 4. Convert one CPS month into six dashboard rows
# ===================================================

summarise_cps_month <- function(cps_data) {
  
  # -------------------------------------------------
  # A. Valid person records and household identifiers
  # -------------------------------------------------
  
  persons <- cps_data |>
    filter(
      !is.na(hrhhid),
      !is.na(hrhhid2),
      !is.na(pulineno),
      pulineno > 0,
      !is.na(prtage)
    ) |>
    mutate(
      HouseholdID = paste(
        hrhhid,
        hrhhid2,
        sep = "_"
      ),
      
      PersonLine = as.numeric(pulineno)
    )
  
  
  # -------------------------------------------------
  # B. Parent flags based on children aged 0–17
  # -------------------------------------------------
  #
  # Each child may have one or two parent pointers.
  # We pivot those pointer fields into a long format,
  # then identify adults linked to at least one
  # co-resident own child.
  #
  parent_flags <- persons |>
    filter(
      prtage >= 0,
      prtage <= 17
    ) |>
    select(
      ReferenceMonth,
      HouseholdID,
      ChildLine = PersonLine,
      ChildAge = prtage,
      pepar1,
      pepar2
    ) |>
    pivot_longer(
      cols = c(pepar1, pepar2),
      names_to = "ParentPointerField",
      values_to = "ParentLine"
    ) |>
    filter(
      !is.na(ParentLine),
      ParentLine > 0
    ) |>
    distinct(
      ReferenceMonth,
      HouseholdID,
      ChildLine,
      ChildAge,
      ParentLine
    ) |>
    group_by(
      ReferenceMonth,
      HouseholdID,
      ParentLine
    ) |>
    summarise(
      HasCoResidentChildUnder18 = TRUE,
      
      HasCoResidentChildAge0to5 = any(
        ChildAge >= 0 &
          ChildAge <= 5
      ),
      
      LinkedChildrenUnder18 = n_distinct(ChildLine),
      
      .groups = "drop"
    )
  
  
  # -------------------------------------------------
  # C. Core-aged adults with usable labour data
  # -------------------------------------------------
  #
  # Age:
  #   25–54
  #
  # Sex:
  #   1 = Men
  #   2 = Women
  #
  # Labour force status:
  #   1–7
  #
  # Weight:
  #   PWCMPWGT
  #
  core_adults <- persons |>
    filter(
      prtage >= 25,
      prtage <= 54,
      
      pesex %in% c(1, 2),
      
      pemlr %in% 1:7,
      
      !is.na(pwcmpwgt),
      pwcmpwgt > 0
    ) |>
    left_join(
      parent_flags,
      by = c(
        "ReferenceMonth",
        "HouseholdID",
        "PersonLine" = "ParentLine"
      )
    ) |>
    mutate(
      HasCoResidentChildUnder18 =
        coalesce(
          HasCoResidentChildUnder18,
          FALSE
        ),
      
      HasCoResidentChildAge0to5 =
        coalesce(
          HasCoResidentChildAge0to5,
          FALSE
        ),
      
      LinkedChildrenUnder18 =
        coalesce(
          LinkedChildrenUnder18,
          0
        ),
      
      Sex = case_when(
        pesex == 1 ~ "Men",
        pesex == 2 ~ "Women",
        TRUE ~ NA_character_
      ),
      
      ChildStatus = case_when(
        HasCoResidentChildAge0to5 ~
          "Co-resident child aged 0–5",
        
        !HasCoResidentChildUnder18 ~
          "No co-resident child under 18",
        
        TRUE ~
          "Co-resident child aged 6–17 only"
      ),
      
      # API PWCMPWGT is already in person-weight units.
      LabourForceWeight = pwcmpwgt
    )
  
  
  # -------------------------------------------------
  # D. Create dashboard populations
  # -------------------------------------------------
  
  # Overall benchmark.
  all_core_aged_adults <- core_adults |>
    mutate(
      ChildStatus = "All core-aged adults"
    )
  
  # Two core comparison groups.
  comparison_groups <- core_adults |>
    filter(
      ChildStatus %in% c(
        "Co-resident child aged 0–5",
        "No co-resident child under 18"
      )
    )
  
  dashboard_population <- bind_rows(
    all_core_aged_adults,
    comparison_groups
  ) |>
    mutate(
      EmploymentWeight = if_else(
        pemlr %in% c(1, 2),
        LabourForceWeight,
        0
      ),
      
      UnemploymentWeight = if_else(
        pemlr %in% c(3, 4),
        LabourForceWeight,
        0
      ),
      
      NILFWeight = if_else(
        pemlr %in% c(5, 6, 7),
        LabourForceWeight,
        0
      ),
      
      SexSort = case_when(
        Sex == "Women" ~ 1,
        Sex == "Men" ~ 2,
        TRUE ~ 99
      ),
      
      ChildStatusSort = case_when(
        ChildStatus == "All core-aged adults" ~ 1,
        
        ChildStatus == "Co-resident child aged 0–5" ~ 2,
        
        ChildStatus == "No co-resident child under 18" ~ 3,
        
        TRUE ~ 99
      )
    )
  
  
  # -------------------------------------------------
  # E. Aggregate to final monthly summary
  # -------------------------------------------------
  
  monthly_summary <- dashboard_population |>
    group_by(
      ReferenceMonth,
      Sex,
      ChildStatus,
      SexSort,
      ChildStatusSort
    ) |>
    summarise(
      UnweightedN = n(),
      
      Population = sum(
        LabourForceWeight,
        na.rm = TRUE
      ),
      
      Employment = sum(
        EmploymentWeight,
        na.rm = TRUE
      ),
      
      Unemployment = sum(
        UnemploymentWeight,
        na.rm = TRUE
      ),
      
      NotInLabourForce = sum(
        NILFWeight,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) |>
    mutate(
      LabourForce =
        Employment +
        Unemployment,
      
      EmploymentRate = if_else(
        Population == 0,
        NA_real_,
        Employment / Population
      ),
      
      ParticipationRate = if_else(
        Population == 0,
        NA_real_,
        LabourForce / Population
      ),
      
      UnemploymentRate = if_else(
        LabourForce == 0,
        NA_real_,
        Unemployment / LabourForce
      ),
      
      GroupLabel = paste(
        Sex,
        ChildStatus,
        sep = " — "
      ),
      
      CheckDifference =
        Population -
        Employment -
        Unemployment -
        NotInLabourForce
    ) |>
    select(
      ReferenceMonth,
      Sex,
      ChildStatus,
      GroupLabel,
      SexSort,
      ChildStatusSort,
      UnweightedN,
      Population,
      Employment,
      Unemployment,
      NotInLabourForce,
      LabourForce,
      EmploymentRate,
      ParticipationRate,
      UnemploymentRate,
      CheckDifference
    ) |>
    arrange(
      ReferenceMonth,
      SexSort,
      ChildStatusSort
    )
  
  return(monthly_summary)
}


# ===================================================
# 5. Safely process one month
# ===================================================

process_cps_month_safely <- function(
    reference_month,
    api_key = census_api_key
) {
  
  tryCatch(
    {
      
      cps_month <- get_cps_month(
        reference_month = reference_month,
        api_key = api_key
      )
      
      monthly_summary <- summarise_cps_month(
        cps_month
      )
      
      list(
        Status = "Loaded",
        ReferenceMonth = as.Date(reference_month),
        ErrorMessage = NA_character_,
        Data = monthly_summary
      )
    },
    
    error = function(e) {
      
      list(
        Status = "Failed",
        ReferenceMonth = as.Date(reference_month),
        ErrorMessage = conditionMessage(e),
        Data = NULL
      )
    }
  )
}


# ===================================================
# 6. Build a summary across a range of months
# ===================================================

build_cps_summary_range <- function(
    start_month,
    end_month,
    api_key = census_api_key
) {
  
  start_month <- floor_date(
    as.Date(start_month),
    unit = "month"
  )
  
  end_month <- floor_date(
    as.Date(end_month),
    unit = "month"
  )
  
  month_list <- seq(
    from = start_month,
    to = end_month,
    by = "month"
  )
  
  monthly_results <- vector(
    mode = "list",
    length = length(month_list)
  )
  
  for (i in seq_along(month_list)) {
    
    current_month <- month_list[i]
    
    message(
      "Processing ",
      format(current_month, "%B %Y"),
      " (",
      i,
      " of ",
      length(month_list),
      ")"
    )
    
    monthly_results[[i]] <- process_cps_month_safely(
      reference_month = current_month,
      api_key = api_key
    )
  }
  
  
  # -------------------------------------------------
  # Create load log
  # -------------------------------------------------
  
  load_log <- tibble(
    ReferenceMonth = as.Date(
      vapply(
        monthly_results,
        function(x) {
          as.character(x$ReferenceMonth)
        },
        character(1)
      )
    ),
    
    Status = vapply(
      monthly_results,
      function(x) {
        x$Status
      },
      character(1)
    ),
    
    ErrorMessage = vapply(
      monthly_results,
      function(x) {
        if (is.null(x$ErrorMessage)) {
          NA_character_
        } else {
          x$ErrorMessage
        }
      },
      character(1)
    )
  ) |>
    arrange(ReferenceMonth)
  
  
  # -------------------------------------------------
  # Combine successful monthly summary tables
  # -------------------------------------------------
  
  successful_summaries <- lapply(
    monthly_results,
    function(x) x$Data
  )
  
  successful_summaries <- Filter(
    Negate(is.null),
    successful_summaries
  )
  
  if (length(successful_summaries) == 0) {
    
    summary_data <- tibble(
      ReferenceMonth = as.Date(character()),
      Sex = character(),
      ChildStatus = character(),
      GroupLabel = character(),
      SexSort = numeric(),
      ChildStatusSort = numeric(),
      UnweightedN = numeric(),
      Population = numeric(),
      Employment = numeric(),
      Unemployment = numeric(),
      NotInLabourForce = numeric(),
      LabourForce = numeric(),
      EmploymentRate = numeric(),
      ParticipationRate = numeric(),
      UnemploymentRate = numeric(),
      CheckDifference = numeric()
    )
    
  } else {
    
    summary_data <- bind_rows(
      successful_summaries
    ) |>
      arrange(
        ReferenceMonth,
        SexSort,
        ChildStatusSort
      )
  }
  
  return(
    list(
      SummaryData = summary_data,
      LoadLog = load_log
    )
  )
}


# ===================================================
# 7. Export completed results
# ===================================================

export_cps_results <- function(
    result,
    output_folder,
    output_label
) {
  
  if (!dir.exists(output_folder)) {
    dir.create(
      output_folder,
      recursive = TRUE
    )
  }
  
  summary_file <- file.path(
    output_folder,
    paste0(
      "CPS_Monthly_Labour_Summary_",
      output_label,
      ".csv"
    )
  )
  
  log_file <- file.path(
    output_folder,
    paste0(
      "CPS_Monthly_Labour_Load_Log_",
      output_label,
      ".csv"
    )
  )
  
  write.csv(
    result$SummaryData,
    summary_file,
    row.names = FALSE
  )
  
  write.csv(
    result$LoadLog,
    log_file,
    row.names = FALSE
  )
  
  message(
    "Summary export: ",
    summary_file
  )
  
  message(
    "Load-log export: ",
    log_file
  )
}


# ===================================================
# 8. Validation helper
# ===================================================

validate_cps_result <- function(result) {
  
  summary_data <- result$SummaryData
  load_log <- result$LoadLog
  
  cat("\n==============================\n")
  cat("CPS SUMMARY VALIDATION\n")
  cat("==============================\n\n")
  
  cat(
    "Summary rows: ",
    nrow(summary_data),
    "\n",
    sep = ""
  )
  
  cat("\nLoad status:\n")
  print(table(load_log$Status))
  
  cat("\nFailed months, if any:\n")
  print(
    load_log |>
      filter(Status != "Loaded")
  )
  
  cat("\nRows per loaded month:\n")
  print(
    summary_data |>
      count(ReferenceMonth) |>
      arrange(ReferenceMonth)
  )
  
  if (nrow(summary_data) > 0) {
    
    max_check_difference <- max(
      abs(summary_data$CheckDifference),
      na.rm = TRUE
    )
    
    cat(
      "\nMaximum absolute CheckDifference: ",
      max_check_difference,
      "\n",
      sep = ""
    )
  }
}
# ===================================================
# 9. Active run block
# ===================================================
#
# Full historical CPS run:
# January 2010 through the latest completed month.
#
# ===================================================

historical_start_month <- as.Date("2010-01-01")

latest_completed_month <- floor_date(
  Sys.Date(),
  unit = "month"
) %m-% months(1)

output_label <- paste0(
  "2010_to_",
  format(
    latest_completed_month,
    "%Y_%m"
  )
)

all_history_result <- build_cps_summary_range(
  start_month = historical_start_month,
  end_month = latest_completed_month
)

validate_cps_result(
  all_history_result
)

export_cps_results(
  result = all_history_result,
  output_folder = output_folder,
  output_label = output_label
)

View(all_history_result$SummaryData)

View(all_history_result$LoadLog)
