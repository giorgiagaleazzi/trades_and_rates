# ----------------------------------------------------------
# main.R
#
# Project: Why Do Exchange Rate Movements Often
#  Fail to Generate Trade Adjustment?
# Author : Giorgia Galeazzi
#
# Purpose:
#   Master script for the complete research pipeline.
#
# Pipeline:
#   1. Load configuration
#   2. Download raw data
#   3. Build macro dataset
#   4. Decompose exchange rates
#   5. Construct gravity panel
#   6. Estimate trade models
#   7. Produce figures and tables
#
# Usage:
#   source("main.R")
# ----------------------------------------------------------

keep <- "dots_raw"   # or whatever you want to keep
rm(list = setdiff(ls(), keep))

# ----------------------------------------------------------
# 1. Load packages
# ----------------------------------------------------------

required_packages <- c("httr2", "readr", "readxl", "haven", "mFilter",
                       "dplyr", "tidyr", "purrr", "lubridate","yaml",
                       "stringr", "countrycode", "fixest",  "tidyverse",
                       "sandwich", "broom", "deseats", "readr")

new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("  Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
}
invisible(lapply(required_packages, library, character.only = TRUE))
cat("  Packages loaded.\n")

# ----------------------------------------------------------
# 2. Load configuration
# ----------------------------------------------------------

cfg <- yaml::read_yaml("config/pipeline_config.yml")

source("config/config.R")
source("config/helpers.R")

log_message("Pipeline started")

# ----------------------------------------------------------
# 3. Load pre-built panel data
# ----------------------------------------------------------

#  01_build_panel2.R takes ~2 hours to run and only needs to be
#  re-run when raw data changes. Run it once manually to produce
#  panel_final.rds, then load it here.

source("R/01_build_panel_G10.R")

#log_message("Step 1: Loading pre-built panel data")
#panel_clean <- readRDS(file.path(cfg$paths$clean_data, "panel_final.rds"))
#log_message(sprintf("Step 1 completed (%d obs loaded)", nrow(panel_clean)))

# ----------------------------------------------------------
# 4. Baseline + GPR interaction estimation
# ----------------------------------------------------------
#
#  Eq. 5.2 (H1): trade response to persistent vs transitory
#                exchange rate movements
#  Eq. 5.3 (H2): GPR interaction with persistent movements,
#                plus GPR subsample splits

log_message("Step 2: Estimation (baseline + GPR interaction)")

source("R/02_estimation.R")

log_message("Step 2 completed")

# ----------------------------------------------------------
# 5. Crisis-period robustness analysis
# ----------------------------------------------------------

log_message("Step 3: Crisis-period analysis")

source("R/03_crisis_analysis.R")

log_message("Step 3 completed")

# ----------------------------------------------------------
# 6. Figures and tables
# ----------------------------------------------------------

log_message("Step 4: Producing figures and tables")

source("R/04_figures_tables.R")

log_message("Step 4 completed")

# ----------------------------------------------------------
# 7. Appendix / robustness results
# ----------------------------------------------------------

log_message("Step 5: Appendix and robustness results")

source("R/05_appendix_results.R")

log_message("Step 5 completed")

# ----------------------------------------------------------
# 8. Final report
# ----------------------------------------------------------

log_message("Pipeline finished successfully")

cat("\n")
cat("========================================\n")
cat(" Exchange Rate Persistence and Trade\n")
cat(" Pipeline completed successfully\n")
cat("========================================\n")
cat("\n")

cat(
  "Sample period: ",
  format(SAMPLE_START, "%Y-%m"),
  " to ",
  format(SAMPLE_END, "%Y-%m"),
  "\n",
  sep = ""
)

cat(
  "Countries: ",
  length(COUNTRIES),
  "\n",
  sep = ""
)

cat(
  "Estimator: ",
  ESTIMATOR,
  "\n",
  sep = ""
)

cat("\n")