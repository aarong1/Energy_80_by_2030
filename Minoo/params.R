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


nice_names <- c(
  sum_import    = "Imports",
  sum_export    = "Exports",
  sum_demand    = "Demand",
  median_SNSP.x = "SNSP",
  sum_res       = "RES (Solar + Wind)"
)

#load data
load("C:/Users/YA775WS/OneDrive - EY/Desktop/SIB/whole_picture_2/data/inputs.rda")

#ensure date type
stopifnot(exists("combined_df"))
combined_df$date <- as.Date(combined_df$date)

if (!"median_SNSP.x" %in% names(combined_df) && "median_SNSP" %in% names(combined_df)) {
  combined_df <- dplyr::rename(combined_df, median_SNSP.x = median_SNSP)
}

#variables to forecast
candidate_vars <- c("sum_import", "sum_export", "sum_demand", "median_SNSP.x", "sum_res")
vars <- intersect(candidate_vars, names(combined_df))
missing_vars <- setdiff(candidate_vars, vars)
if (length(missing_vars)) {
  warning("Skipping missing variables: ", paste(missing_vars, collapse = ", "))
}
if (length(vars) == 0) stop("None of the expected variables were found in combined_df.")

#parameters
agg_fun      <- mean        #it can change to sum or median
val_year     <- 2025        #validation data
h            <- 50          #forecast horizon
max_lag      <- 12          #12 months of lags
roll_windows <- c(3, 6, 12) #rolling mean windows
seed         <- 2026

set.seed(seed)

