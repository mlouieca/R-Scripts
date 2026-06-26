# ============================================================
# R PACKAGE SETUP — ELCC / LFS / CPS / STATCAN / SQL WORKFLOW
# ============================================================

# Use a reliable CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ------------------------------------------------------------
# 1. Core data manipulation and file handling
# ------------------------------------------------------------

core_packages <- c(
  # Data wrangling
  "data.table",      # Fast processing of large microdata files
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "forcats",
  "tibble",
  "tidyselect",
  
  # Import/export
  "readr",           # CSV reading/writing
  "vroom",           # Very fast CSV reading for large files
  "readxl",          # Read Excel
  "writexl",         # Write simple Excel files
  "openxlsx",        # More formatted Excel output
  "haven",           # Read SAS, SPSS, Stata files
  "arrow",           # Parquet files; useful for large historical datasets
  
  # File paths and project organization
  "here",            # Reliable project-relative paths
  "fs",              # File-system operations
  "zip",             # ZIP file utilities
  
  # Cleaning and labelling
  "janitor",         # clean_names(), tabyl(), etc.
  "snakecase",
  "labelled",        # Manage labelled variables from survey files
  
  # Dates and text
  "lubridate",
  "stringi",
  "glue",
  
  # Helpful console / error messages
  "cli",
  "crayon",
  "progress"
)

# ------------------------------------------------------------
# 2. Survey and microdata analysis
# ------------------------------------------------------------

survey_packages <- c(
  "survey",          # Core weighted-survey analysis package
  "srvyr",           # dplyr-style interface to survey package
  "sampling",        # Sampling-related tools
  "ipumsr"           # Useful if you ever use IPUMS extracts
)

# ------------------------------------------------------------
# 3. Statistics Canada / API / web data retrieval
# ------------------------------------------------------------

statcan_packages <- c(
  "cansim",          # Download StatCan tables programmatically
  "httr2",           # Modern web/API requests
  "jsonlite",        # Parse JSON API responses
  "curl",            # Download utilities
  "rvest",           # Web scraping where appropriate
  "xml2"             # HTML/XML parsing support for rvest
)

# ------------------------------------------------------------
# 4. SQL Server and database work
# ------------------------------------------------------------

database_packages <- c(
  "DBI",             # Database interface
  "odbc",            # SQL Server / ODBC connections
  "duckdb",          # Local analytical SQL database
  "dbplyr",          # dplyr syntax translated into SQL
  "RSQLite"          # Lightweight local database option
)

# ------------------------------------------------------------
# 5. Analysis, modelling, and time series
# ------------------------------------------------------------

analysis_packages <- c(
  "broom",           # Tidy regression/model outputs
  "fixest",          # Fast fixed-effects regressions
  "modelsummary",    # Publication-ready regression tables
  "sandwich",        # Robust standard errors
  "lmtest",          # Model tests
  "forecast",        # Time-series tools
  "tsibble",         # Tidy time-series structure
  "fable",           # Forecasting workflows
  "slider",          # Rolling averages, moving windows
  "zoo",             # Time-series and rolling calculations
  "scales"           # Formatting percentages, dollars, axes
)

# ------------------------------------------------------------
# 6. Charts and reporting
# ------------------------------------------------------------

reporting_packages <- c(
  "ggplot2",
  "patchwork",       # Combine charts
  "ggrepel",         # Better chart labels
  "ggtext",          # Rich text in ggplot labels
  "knitr",
  "rmarkdown",
  "quarto",
  "kableExtra",
  "gt",
  "flextable"
)

# ------------------------------------------------------------
# 7. Optional quality-of-life packages
# ------------------------------------------------------------

optional_packages <- c(
  "conflicted",      # Makes package-function conflicts explicit
  "skimr",           # Quick dataset summaries
  "naniar",          # Missing-data exploration
  "checkmate",       # Data validation
  "pointblank",      # More formal data-quality checks
  "qs",              # Very fast R object storage
  "pins"             # Versioned data storage/caching
)

# ------------------------------------------------------------
# Combine all package groups
# ------------------------------------------------------------

all_packages <- unique(c(
  core_packages,
  survey_packages,
  statcan_packages,
  database_packages,
  analysis_packages,
  reporting_packages,
  optional_packages
))

# ------------------------------------------------------------
# Install only packages that are not already installed
# ------------------------------------------------------------

installed <- rownames(installed.packages())
missing_packages <- setdiff(all_packages, installed)

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
} else {
  message("All requested packages are already installed.")
}

# ------------------------------------------------------------
# Optional: update existing installed packages
# Uncomment when needed.
# ------------------------------------------------------------

# update.packages(
#   ask = FALSE,
#   checkBuilt = TRUE
# )

message("Package setup complete.")