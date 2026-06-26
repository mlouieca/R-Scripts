# ============================================================
# R PACKAGE SETUP - QUANTS MINI
# Core tools for ELCC / LFS / CPS / StatCan / SQL workflows
# ============================================================

# Use a reliable CRAN mirror.
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Keep this list small: data work, survey microdata, StatCan,
# SQL Server connections, and basic charts.
mini_packages <- c(
  "data.table",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "janitor",
  "lubridate",
  "survey",
  "srvyr",
  "cansim",
  "DBI",
  "odbc",
  "ggplot2"
)

installed <- rownames(installed.packages())
missing_packages <- setdiff(mini_packages, installed)

if (length(missing_packages) > 0) {
  message("Installing: ", paste(missing_packages, collapse = ", "))

  # NA installs required dependencies but skips bulky suggested packages.
  install.packages(missing_packages, dependencies = NA)
} else {
  message("All mini setup packages are already installed.")
}

message("Mini package setup complete.")
