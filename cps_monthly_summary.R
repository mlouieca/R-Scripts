library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)

# ---------------------------------------------------
# Configuration
# ---------------------------------------------------

census_api_key <- "1a8e3aa3d88e871469e4ee867856fd6b9dc2a672"

# ---------------------------------------------------
# Download one CPS Basic Monthly file
# ---------------------------------------------------

get_cps_month <- function(reference_month, api_key = census_api_key) {
  
  reference_month <- as.Date(reference_month)
  
  year_text <- format(reference_month, "%Y")
  month_text <- tolower(format(reference_month, "%b"))
  
  base_url <- paste0(
    "https://api.census.gov/data/",
    year_text,
    "/cps/basic/",
    month_text
  )
  
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
  
  requested_variables <- c(core_variables, parent_variables)
  
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
      SourceFile = paste0(month_text, substr(year_text, 3, 4), " CPS API"),
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
        c(hrhhid, hrhhid2),
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

# ---------------------------------------------------
# Test: January 2019
# ---------------------------------------------------

cps_jan_2019 <- get_cps_month("2019-01-01")

glimpse(cps_jan_2019)

head(cps_jan_2019)



cps_jan_2019 |>
  summarise(
    total_records = n(),
    child_records = sum(prtage >= 0 & prtage <= 17, na.rm = TRUE),
    linked_parent_pointer_1 = sum(pepar1 > 0, na.rm = TRUE),
    linked_parent_pointer_2 = sum(pepar2 > 0, na.rm = TRUE)
  )

cps_jan_2019 |>
  filter(prtage >= 0, prtage <= 17, pepar1 > 0 | pepar2 > 0) |>
  select(
    hrhhid,
    hrhhid2,
    pulineno,
    prtage,
    pepar1,
    pepar2
  ) |>
  head(10)


# ---------------------------------------------------
# Convert one CPS month into the six dashboard groups
# ---------------------------------------------------

summarise_cps_month <- function(cps_data) {
  
  # Keep valid person-level records and create identifiers
  persons <- cps_data |>
    filter(
      !is.na(hrhhid),
      !is.na(hrhhid2),
      !is.na(pulineno),
      pulineno > 0,
      !is.na(prtage)
    ) |>
    mutate(
      HouseholdID = paste(hrhhid, hrhhid2, sep = "_"),
      PersonLine = as.numeric(pulineno)
    )
  
  # Identify children aged 0–17 and link each child to parent line(s).
  parent_flags <- persons |>
    filter(prtage >= 0, prtage <= 17) |>
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
        ChildAge >= 0 & ChildAge <= 5
      ),
      LinkedChildrenUnder18 = n_distinct(ChildLine),
      .groups = "drop"
    )
  
  # Restrict to core-aged adults with a usable labour-force status and weight.
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
        coalesce(HasCoResidentChildUnder18, FALSE),
      
      HasCoResidentChildAge0to5 =
        coalesce(HasCoResidentChildAge0to5, FALSE),
      
      LinkedChildrenUnder18 =
        coalesce(LinkedChildrenUnder18, 0),
      
      Sex = case_when(
        pesex == 1 ~ "Men",
        pesex == 2 ~ "Women"
      ),
      
      ChildStatus = case_when(
        HasCoResidentChildAge0to5 ~
          "Co-resident child aged 0–5",
        
        !HasCoResidentChildUnder18 ~
          "No co-resident child under 18",
        
        TRUE ~
          "Co-resident child aged 6–17 only"
      ),
      
      LabourForceWeight = pwcmpwgt
    )
  
  # Make the overall benchmark group.
  all_core_aged_adults <- core_adults |>
    mutate(
      ChildStatus = "All core-aged adults"
    )
  
  # Keep the two comparison populations for the dashboard.
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
      Population = sum(LabourForceWeight),
      Employment = sum(EmploymentWeight),
      Unemployment = sum(UnemploymentWeight),
      NotInLabourForce = sum(NILFWeight),
      .groups = "drop"
    ) |>
    mutate(
      LabourForce = Employment + Unemployment,
      
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
# ---------------------------------------------------
# Build summary rows for a range of CPS months
# ---------------------------------------------------

build_cps_summary_range <- function(
    start_month,
    end_month,
    api_key = census_api_key
) {
  
  start_month <- floor_date(as.Date(start_month), unit = "month")
  end_month <- floor_date(as.Date(end_month), unit = "month")
  
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
  
  load_log <- tibble(
    ReferenceMonth = as.Date(
      vapply(
        monthly_results,
        function(x) as.character(x$ReferenceMonth),
        character(1)
      )
    ),
    Status = vapply(
      monthly_results,
      function(x) x$Status,
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
  
  successful_summaries <- monthly_results |>
    lapply(function(x) x$Data) |>
    Filter(Negate(is.null), x = _)
  
  summary_data <- bind_rows(successful_summaries) |>
    arrange(
      ReferenceMonth,
      SexSort,
      ChildStatusSort
    )
  
  list(
    SummaryData = summary_data,
    LoadLog = load_log
  )
}
# ---------------------------------------------------
# Export a completed summary range and load log
# ---------------------------------------------------

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
  
  message("Summary export: ", summary_file)
  message("Load-log export: ", log_file)
}
# ---------------------------------------------------
# Safely process one CPS month
# ---------------------------------------------------

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
      
      monthly_summary <- summarise_cps_month(cps_month)
      
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
# ---------------------------------------------------
# Full historical CPS backfill: 2019 to latest completed month
# ---------------------------------------------------

latest_completed_month <- floor_date(
  Sys.Date(),
  unit = "month"
) %m-% months(1)

output_label <- paste0(
  "2019_to_",
  format(latest_completed_month, "%Y_%m")
)

all_history_result <- build_cps_summary_range(
  start_month = "2019-01-01",
  end_month = latest_completed_month
)

export_cps_results(
  result = all_history_result,
  output_folder = "~/CPS_Output",
  output_label = output_label
)

View(all_history_result$SummaryData)

View(all_history_result$LoadLog)
