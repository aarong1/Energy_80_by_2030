#libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(xgboost)
library(ggplot2)
library(shiny)
library(readxl)
library(stringr)
library(scales)
library(shinydashboard)
library(DT)
library(rlang)
library(timevis)
library(bslib)
library(echarts4r)
library(reactable)
library(reactablefmtr)
library(gt)


nice_names <- c(
  sum_import    = "Imports",
  sum_export    = "Exports",
  sum_demand    = "Demand",
  sum_avai_solar = "Available Solar",
  sum_solar = "Generated Solar",
  sum_avai_wind = "Available Wind",
  sum_wind = "Generated Wind",
  sum_system_gen.y = "Fossil Fuel"
)

#load data
load("./data/inputs.rda")

#ensure date type
stopifnot(exists("combined_df"))
combined_df$date <- as.Date(combined_df$date)

if (!"median_SNSP.x" %in% names(combined_df) && "median_SNSP" %in% names(combined_df)) {
  combined_df <- dplyr::rename(combined_df, median_SNSP.x = median_SNSP)
}

#variables to forecast
candidate_vars <- c("sum_import", "sum_export", "sum_demand", "sum_avai_solar","sum_solar", "sum_avai_wind","sum_wind", "sum_system_gen.y")
vars <- intersect(candidate_vars, names(combined_df))
missing_vars <- setdiff(candidate_vars, vars)
if (length(missing_vars)) {
  warning("Skipping missing variables: ", paste(missing_vars, collapse = ", "))
}
if (length(vars) == 0) stop("None of the expected variables were found in combined_df.")

#parameters
agg_fun      <- sum        #it can change to sum or median
val_year     <- 2025        #validation data
h            <- 61          #forecast horizon
max_lag      <- 12          #12 months of lags
roll_windows <- c(3, 6, 12) #rolling mean windows
seed         <- 2026

set.seed(seed)

