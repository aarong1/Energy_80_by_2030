
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(xgboost)
library(ggplot2)

nice_names <- c(
  sum_import    = "Imports",
  sum_export    = "Exports",
  sum_demand    = "Demand",
  median_SNSP.x = "SNSP",
  sum_solar = "Generated Solar",
  sum_wind = "Generated Wind",
  sum_avai_solar = "Available Solar",
  sum_avai_wind = "Available Wind",
  sum_system_gen.y = "Fossil Fuel"
)

get_train_label <- function(var, val_year, min_date) {
  if (!is.null(min_date) && var %in% c("sum_import", "sum_export", "sum_solar", "sum_avai_solar")) {
    paste0("Train: ", substr(min_date, 1, 4), "–", val_year - 1)
  } else {
    paste0("Train: < ", val_year)
  }
}

load("./data/inputs.rda")
stopifnot(exists("combined_df"))
combined_df$date <- as.Date(combined_df$date)

if (!"median_SNSP.x" %in% names(combined_df) && "median_SNSP" %in% names(combined_df)) {
  combined_df <- dplyr::rename(combined_df, median_SNSP.x = median_SNSP)
}

candidate_vars <- c("sum_import", "sum_export", "sum_demand", "sum_solar",
                    "sum_wind", "sum_avai_solar", "sum_avai_wind", "sum_system_gen.y")
vars <- intersect(candidate_vars, names(combined_df))
missing_vars <- setdiff(candidate_vars, vars)
if (length(missing_vars)) warning("Skipping missing: ", paste(missing_vars, collapse = ", "))

agg_fun      <- sum
val_year     <- 2025
h            <- 61
max_lag      <- 12
roll_windows <- c(3, 6, 12)
seed         <- 2026
set.seed(seed)

#monthly aggregation
monthly_df <- combined_df %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    sum_import = if ("sum_import" %in% vars) agg_fun(sum_import, na.rm = TRUE),
    sum_export = if ("sum_export" %in% vars) agg_fun(sum_export, na.rm = TRUE),
    sum_demand = if ("sum_demand" %in% vars) agg_fun(sum_demand, na.rm = TRUE),
    sum_solar  = if ("sum_solar" %in% vars)  agg_fun(sum_solar,  na.rm = TRUE),
    sum_wind   = if ("sum_wind" %in% vars)   agg_fun(sum_wind,   na.rm = TRUE),
    sum_avai_solar = if ("sum_avai_solar" %in% vars) agg_fun(sum_avai_solar, na.rm = TRUE),
    sum_avai_wind  = if ("sum_avai_wind" %in% vars) agg_fun(sum_avai_wind,  na.rm = TRUE),
    sum_system_gen.y  = if ("sum_system_gen.y" %in% vars) agg_fun(sum_system_gen.y,  na.rm = TRUE),
    .groups="drop"
  ) %>%
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
  arrange(date) %>%
  select(date, any_of(vars))

#time features  
add_time_features <- function(df) {
  df %>%
    mutate(
      month = month(date),
      sin12 = sin(2 * pi * month / 12),
      cos12 = cos(2 * pi * month / 12)
    )
}

vars_zero_to_na <- c("sum_export", "sum_import", "sum_solar", "sum_avai_solar")

for (v in vars_zero_to_na) {
  if (v %in% names(monthly_df)) {
    monthly_df[[v]] <- ifelse(monthly_df[[v]] == 0, NA, monthly_df[[v]])
  }
}


#supervised dataset
make_supervised <- function(df, target, max_lag = 12, roll_windows = c(3,6,12)) {
  out <- df
  for (L in 1:max_lag) {
    out[[paste0("lag_", L)]] <- lag(out[[target]], n = L)
  }
  lag1 <- lag(out[[target]], n = 1)
  for (w in roll_windows) {
    out[[paste0("rollmean_", w)]] <- zoo::rollapply(
      lag1, width = w, FUN = mean, align = "right", fill = NA
    )
  }
  out
}

#train/validation split
build_matrices <- function(df_sup, target, val_year) {
  df_sup <- df_sup %>% filter(complete.cases(.))
  idx_train <- which(year(df_sup$date) < val_year)
  idx_valid <- which(year(df_sup$date) == val_year)
  
  feats <- grep("^(month|sin12|cos12|lag_|rollmean_)", names(df_sup), value = TRUE)
  
  dtrain <- xgb.DMatrix(as.matrix(df_sup[idx_train, feats]), label=df_sup[[target]][idx_train])
  dvalid <- xgb.DMatrix(as.matrix(df_sup[idx_valid, feats]), label=df_sup[[target]][idx_valid])
  
  list(dtrain=dtrain, dvalid=dvalid, feats=feats,
       train_df=df_sup[idx_train,], valid_df=df_sup[idx_valid,])
}

#fit XGB
fit_xgb <- function(dtrain, dvalid, params=list(), nrounds=5000, early_stopping_rounds=100) {
  default_params <- list(
    objective="reg:squarederror", eval_metric="rmse",
    max_depth=6, eta=0.03, subsample=0.8, colsample_bytree=0.8, min_child_weight=5
  )
  params <- modifyList(default_params, params)
  xgb.train(
    params=params, data=dtrain, nrounds=nrounds,
    watchlist=list(train=dtrain, valid=dvalid),
    early_stopping_rounds=early_stopping_rounds
  )
}

#iterative forecasting
forecast_iterative <- function(model, last_hist_df, target, feats, horizon, max_lag, roll_windows) {
  hist <- last_hist_df %>% arrange(date)
  last_date <- max(hist$date)
  future <- data.frame()
  preds <- numeric(horizon)
  work <- hist
  
  for (i in 1:horizon) {
    next_date <- seq(last_date, by="month", length.out=2)[2]
    
    base_row <- data.frame(date=next_date) %>%
      add_time_features() %>%
      mutate(!!target := NA_real_) %>%
      select(date, month, sin12, cos12, all_of(target))
    
    work_min <- work %>%
      select(date, month, sin12, cos12, all_of(target), starts_with("lag_"), starts_with("rollmean_"))
    tmp <- bind_rows(work_min, base_row)
    
    for (L in 1:max_lag) tmp[[paste0("lag_", L)]] <- lag(tmp[[target]], n = L)
    
    lag1 <- lag(tmp[[target]], 1)
    for (w in roll_windows)
      tmp[[paste0("rollmean_", w)]] <- zoo::rollapply(lag1, w, mean, align="right", fill=NA)
    
    xrow <- tmp %>% slice(n()) %>% select(all_of(feats))
    if (anyNA(xrow)) {
      last_non_na <- work %>% slice(n()) %>% select(all_of(feats))
      nas <- which(is.na(xrow))
      xrow[nas] <- last_non_na[nas]
    }
    
    yhat <- as.numeric(predict(model, xgb.DMatrix(as.matrix(xrow))))
    preds[i] <- yhat
    
    tmp[nrow(tmp), target] <- yhat
    work <- tmp
    
    future <- bind_rows(future, data.frame(date=next_date, pred=yhat))
    last_date <- next_date
  }
  future
}

#bootstrap PIs
bootstrap_intervals <- function(point_fc, residuals, B=1000, levels=c(0.80,0.95)) {
  set.seed(seed + 1)
  n_h <- nrow(point_fc)
  mat <- matrix(NA, n_h, B)
  res_pool <- residuals[is.finite(residuals)]
  
  for (b in 1:B) mat[,b] <- point_fc$pred + sample(res_pool, n_h, replace=TRUE)
  
  out <- point_fc
  for (lvl in levels) {
    lo <- apply(mat, 1, quantile, probs = (1-lvl)/2)
    hi <- apply(mat, 1, quantile, probs = 1-(1-lvl)/2)
    out[[paste0("lo_", round(lvl*100))]] <- lo
    out[[paste0("hi_", round(lvl*100))]] <- hi
  }
  out
}

#master routine
run_xgb_forecast <- function(monthly_df, target, val_year, h, max_lag, roll_windows,
                             params=list(), min_train_date=NULL) {
  base_full <- monthly_df %>%
    add_time_features() %>%
    select(date, month, sin12, cos12, all_of(target))
  
  base <- if (!is.null(min_train_date)) base_full %>% filter(date >= as.Date(min_train_date)) else base_full
  
  sup <- make_supervised(base, target, max_lag=max_lag, roll_windows=roll_windows)
  mats <- build_matrices(sup, target, val_year)
  model <- fit_xgb(mats$dtrain, mats$dvalid, params=params)
  
  valid_pred <- predict(model, mats$dvalid)
  valid_df <- mats$valid_df %>% mutate(pred = valid_pred, resid = .data[[target]] - pred)
  
  last_hist <- sup %>% filter(complete.cases(.))
  future_pts <- forecast_iterative(model, last_hist, target, mats$feats, h, max_lag, roll_windows)
  fc_with_pi <- bootstrap_intervals(future_pts, valid_df$resid, B=1000)
  
  list(
    model=model, valid=valid_df, forecast=fc_with_pi,
    history=monthly_df %>% select(date, all_of(target))
  )
}



plot_fc <- function(res, target) {
  hist <- res$history %>% rename(value=all_of(target))
  fc <- res$forecast
  
  ggplot() +
    geom_line(data=hist, aes(date,value), color="grey25", linewidth=0.7) +
    geom_ribbon(data=fc, aes(date, ymin=lo_95, ymax=hi_95),
                fill="#9ecae1", alpha=0.35) +
    geom_ribbon(data=fc, aes(date, ymin=lo_80, ymax=hi_80),
                fill="#3182bd", alpha=0.35) +
    geom_line(data=fc, aes(date,pred), color="#08519c", linewidth=1.2) +
    labs(title=paste("XGBoost Forecast:", nice_names[[target]]),
         subtitle=paste("Training: 2014 - 2025"),
         x="Date", y=nice_names[[target]]) +
    theme_minimal(14)
}

plot_fc_last3 <- function(res, target) {
  hist <- res$history %>% rename(value=all_of(target))
  fc <- res$forecast
  
  ggplot() +
    geom_line(data=hist, aes(date,value), color="grey25", linewidth=0.7) +
    geom_ribbon(data=fc, aes(date, ymin=lo_95, ymax=hi_95),
                fill="#e5989b", alpha=0.35) +
    geom_ribbon(data=fc, aes(date, ymin=lo_80, ymax=hi_80),
                fill="#b56576", alpha=0.35) +
    geom_line(data=fc, aes(date,pred), color="#6d597a", linewidth=1.2) +
    labs(title=paste("XGBoost Forecast:", nice_names[[target]]),
         subtitle=paste("Training: 2023 - 2025"),
         x="Date", y=nice_names[[target]]) +
    theme_minimal(14)
}

results_full <- list()   
results_last3 <- list()  #training only on 2023–2025

for (v in vars) {
  message("Training full model: ", v)
  
  min_date <- NULL
  if (v %in% c("sum_import", "sum_export")) min_date <- "2022-01-01"
  if (v %in% c("sum_solar", "sum_avai_solar")) min_date <- "2018-01-01"
  
  results_full[[v]] <- run_xgb_forecast(
    monthly_df, target=v, val_year=val_year, h=h,
    max_lag=max_lag, roll_windows=roll_windows,
    params=list(eta=0.03, max_depth=6, subsample=0.8, colsample_bytree=0.8),
    min_train_date=min_date
  )
  
  message("Training last-3-years model: ", v)
  
  results_last3[[v]] <- run_xgb_forecast(
    monthly_df, target=v, val_year=val_year, h=h,
    max_lag=max_lag, roll_windows=roll_windows,
    params=list(eta=0.03, max_depth=6, subsample=0.8, colsample_bytree=0.8),
    min_train_date="2023-01-01"
  )
  
  
  print(plot_fc(results_full[[v]], v))            
  print(plot_fc_last3(results_last3[[v]], v))           
}

#save
output_path <- "./data"
save(results_full, results_last3, file=file.path(output_path, "results_xgb_all.rda"))
