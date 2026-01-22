#functions
plot_xgb_forecast <- function(res, target) {
   hist <- res$history %>% rename(value = all_of(target))
   fc   <- res$forecast

   ggplot() +
     geom_line(data = hist, aes(x = date, y = value), color = "grey25", linewidth = 0.7) +
     geom_ribbon(data = fc, aes(x = date, ymin = lo_95, ymax = hi_95),
                 fill = "#9ecae1", alpha = 0.35) +
     geom_ribbon(data = fc, aes(x = date, ymin = lo_80, ymax = hi_80),
                 fill = "#3182bd", alpha = 0.35) +
     geom_line(data = fc, aes(x = date, y = pred), color = "#08519c", linewidth = 1.2) +
     labs(
       title = paste("XGBoost Forecast —", nice_names[[target]]),
       x = "Date",
       y = nice_names[[target]]
     ) +
     theme_minimal(base_size = 14)
 }

get_train_label <- function(var, val_year, min_date) {
   if (!is.null(min_date) && var %in% c("sum_import", "sum_export")) {
     paste0("Train: ", substr(min_date, 1, 4), "–", val_year - 1)
   } else {
     paste0("Train: < ", val_year)
   }
 }

plot_fc <- function(res, target) {
  hist <- res$history %>% rename(value = all_of(target))
  fc   <- res$forecast
  
  ggplot() +
    geom_line(data = hist, aes(x = date, y = value), color = "grey25", linewidth = 0.7) +
    geom_ribbon(data = fc, aes(x = date, ymin = lo_95, ymax = hi_95),
                fill = "#9ecae1", alpha = 0.35) +
    geom_ribbon(data = fc, aes(x = date, ymin = lo_80, ymax = hi_80),
                fill = "#3182bd", alpha = 0.35) +
    geom_line(data = fc, aes(x = date, y = pred), color = "#08519c", linewidth = 1.2) +
    labs(
      title = paste("XGBoost Forecast —", nice_names[[target]]),
      subtitle = paste0(
        get_train_label(target, val_year, min_date = if (target %in% c("sum_import","sum_export")) "2022-01-01" else NULL),
        " | Valid: ", val_year,
        " | Horizon: ", h, " months"
      ),
      x = "Date", y = nice_names[[target]],
      caption = "Bands: 80% (inner) and 95% (outer) via residual bootstrap"
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())
}

run_lm_forecast <- function(monthly_df, target, val_year, h, min_train_date = NULL) {
  base_full <- monthly_df %>%
    select(date, all_of(target)) %>%
    mutate(
      month = lubridate::month(date),
      sin12 = sin(2 * pi * month / 12),
      cos12 = cos(2 * pi * month / 12)
    ) %>%
    arrange(date)
  base <- if (!is.null(min_train_date)) {
    base_full %>% filter(date >= as.Date(min_train_date))
  } else {
    base_full
  }
  base <- base %>% mutate(t = dplyr::row_number())

  idx_train <- which(lubridate::year(base$date) <  val_year)
  idx_valid <- which(lubridate::year(base$date) == val_year)

  if (length(idx_train) == 0) stop("No training rows before val_year = ", val_year)
  if (length(idx_valid) == 0) stop("No validation rows in val_year = ", val_year,
                                   ". Pick a different val_year or ensure data covers it.")

  train_df <- base[idx_train, , drop = FALSE]
  valid_df <- base[idx_valid, , drop = FALSE]

  f <- stats::as.formula(paste(target, "~ t + sin12 + cos12"))
  fit <- stats::lm(f, data = train_df)

  valid_df <- valid_df %>%
    mutate(pred = as.numeric(stats::predict(fit, newdata = valid_df)),
           resid = .data[[target]] - pred)

  last_date <- max(base$date)
  future_dates <- seq.Date(last_date, by = "month", length.out = h + 1)[-1]

  future <- tibble::tibble(date = future_dates) %>%
    mutate(
      month = lubridate::month(date),
      sin12 = sin(2 * pi * month / 12),
      cos12 = cos(2 * pi * month / 12),
      t = max(base$t) + dplyr::row_number()
    ) %>%
    mutate(pred = as.numeric(stats::predict(fit, newdata = .)))

  fc_with_pi <- bootstrap_intervals(
    point_fc  = future %>% select(date, pred) %>% as.data.frame(),
    residuals = valid_df$resid,
    B = 1000, levels = c(0.80, 0.95)
  )

  list(
    model    = fit,
    valid    = valid_df,
    forecast = fc_with_pi,
    history  = monthly_df %>% select(date, all_of(target))
  )
}


plot_fc_lm <- function(res_lm, target) {
  hist  <- res_lm$history %>% dplyr::rename(value = dplyr::all_of(target))
  fc_lm <- res_lm$forecast
  ggplot() +
    geom_line(data = hist, aes(x = date, y = value), color = "grey25", linewidth = 0.7) +
    geom_ribbon(data = fc_lm, aes(x = date, ymin = lo_95, ymax = hi_95),
                fill = "#fdd0a2", alpha = 0.30) +
    geom_ribbon(data = fc_lm, aes(x = date, ymin = lo_80, ymax = hi_80),
                fill = "#f16913", alpha = 0.25) +
    geom_line(data = fc_lm, aes(x = date, y = pred),
              color = "#d94801", linewidth = 1.2, linetype = "dashed") +
    labs(
      title    = paste("Linear Model Forecast —", nice_names[[target]]),
      
      subtitle = paste0(
        get_train_label(target, val_year, min_date = if (target %in% c("sum_import","sum_export")) "2022-01-01" else NULL),
        " | Valid: ", val_year,
        " | Horizon: ", h, " months"
      ),
      x = "Date", y = nice_names[[target]],
      caption = "80% and 95% forecast intervals"
    ) +
    theme_minimal(base_size = 14)
}

monthly_df <- combined_df %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
  group_by(year, month) %>%
  summarise(
    sum_import    = if ("sum_import" %in% vars)    agg_fun(sum_import,    na.rm = TRUE) else NULL,
    sum_export    = if ("sum_export" %in% vars)    agg_fun(sum_export,    na.rm = TRUE) else NULL,
    sum_demand    = if ("sum_demand" %in% vars)    agg_fun(sum_demand,    na.rm = TRUE) else NULL,
    median_SNSP.x = if ("median_SNSP.x" %in% vars) agg_fun(median_SNSP.x, na.rm = TRUE) else NULL,
    sum_res = if ("sum_res" %in% vars) agg_fun(sum_res, na.rm = TRUE) else NULL,
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
  arrange(date) %>%
  select(date, any_of(vars))


add_time_features <- function(df) {
  df %>%
    mutate(
      month = lubridate::month(date),
      #cyclical encodings for seasonality
      sin12 = sin(2 * pi * month / 12),
      cos12 = cos(2 * pi * month / 12)
    )
}

make_supervised <- function(df, target, max_lag = 12, roll_windows = c(3,6,12)) {
  out <- df
  for (L in 1:max_lag) {
    out[[paste0("lag_", L)]] <- dplyr::lag(out[[target]], n = L)
  }

  lag1 <- dplyr::lag(out[[target]], n = 1)
  for (w in roll_windows) {
    out[[paste0("rollmean_", w)]] <- zoo::rollapply(
      lag1, width = w, FUN = mean, align = "right", fill = NA, partial = FALSE
    )
  }
  out
}

build_matrices <- function(df_sup, target, val_year) {
  df_sup <- df_sup %>% filter(complete.cases(.))

  idx_train <- which(lubridate::year(df_sup$date) <  val_year)
  idx_valid <- which(lubridate::year(df_sup$date) == val_year)

  if (length(idx_train) == 0) stop("No training rows before val_year = ", val_year)
  if (length(idx_valid) == 0) stop("No validation rows in val_year = ", val_year,
                                   ". Pick a different val_year or ensure data covers it.")

  feats <- grep("^(month|sin12|cos12|lag_|rollmean_)", names(df_sup), value = TRUE)

  dtrain <- xgb.DMatrix(
    data = as.matrix(df_sup[idx_train, feats, drop = FALSE]),
    label = df_sup[[target]][idx_train]
  )

  dvalid <- xgb.DMatrix(
    data = as.matrix(df_sup[idx_valid, feats, drop = FALSE]),
    label = df_sup[[target]][idx_valid]
  )

  list(
    dtrain = dtrain,
    dvalid = dvalid,
    feats  = feats,
    train_df = df_sup[idx_train, , drop = FALSE],
    valid_df = df_sup[idx_valid, , drop = FALSE]
  )
}

fit_xgb <- function(dtrain, dvalid, params = list(), nrounds = 5000, early_stopping_rounds = 100) {
  watchlist <- list(train = dtrain, valid = dvalid)

  default_params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.03,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 5,
    nthread = 0
  )

  params <- modifyList(default_params, params)

  xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    watchlist = watchlist,
    early_stopping_rounds = early_stopping_rounds,
    verbose = 1
  )
}

forecast_iterative <- function(model, last_hist_df, target, feats, horizon, max_lag, roll_windows) {
  hist <- last_hist_df %>% arrange(date)
  last_date <- max(hist$date)

  future <- data.frame()
  preds  <- numeric(horizon)

  work <- hist

  for (i in 1:horizon) {  
    next_date <- as.Date(seq(last_date, by = "month", length.out = 2)[2])

    base_row <- data.frame(date = next_date) %>% add_time_features() %>%
      mutate(!!target := NA_real_) %>%
      select(date, month, sin12, cos12, all_of(target))

    work_min <- work %>%
      select(date, month, sin12, cos12, all_of(target),
             starts_with("lag_"), starts_with("rollmean_"))

    tmp <- dplyr::bind_rows(work_min, base_row)

    for (L in 1:max_lag) {
      tmp[[paste0("lag_", L)]] <- dplyr::lag(tmp[[target]], n = L)
    }

    lag1 <- dplyr::lag(tmp[[target]], n = 1)
    for (w in roll_windows) {
      tmp[[paste0("rollmean_", w)]] <- zoo::rollapply(
        lag1, width = w, FUN = mean, align = "right", fill = NA, partial = FALSE
      )
    }

    xrow <- tmp %>% slice(dplyr::n()) %>% select(all_of(feats))

    if (anyNA(xrow)) {
      last_non_na <- work %>% slice(dplyr::n()) %>% select(all_of(feats))
      nas <- which(is.na(xrow))
      xrow[nas] <- last_non_na[nas]
    }

    dmat <- xgb.DMatrix(as.matrix(xrow))
    yhat <- as.numeric(predict(model, dmat))
    preds[i] <- yhat

    tmp[nrow(tmp), target] <- yhat
    work <- tmp

    future <- dplyr::bind_rows(future, data.frame(date = next_date, pred = yhat))
    last_date <- next_date
  }

  future
}

bootstrap_intervals <- function(point_fc, residuals, B = 1000, levels = c(0.80, 0.95)) {
  set.seed(seed + 1)
  n_h <- nrow(point_fc)
  mat <- matrix(NA_real_, nrow = n_h, ncol = B)
  res_pool <- residuals[is.finite(residuals)]

  for (b in 1:B) {
    draws <- sample(res_pool, size = n_h, replace = TRUE)
    mat[, b] <- point_fc$pred + draws
  }

  out <- point_fc
  for (lvl in levels) {
    lo <- apply(mat, 1, quantile, probs = (1 - lvl)/2, na.rm = TRUE)
    hi <- apply(mat, 1, quantile, probs = 1 - (1 - lvl)/2, na.rm = TRUE)
    out[[paste0("lo_", round(lvl*100))]] <- lo
    out[[paste0("hi_", round(lvl*100))]] <- hi
  }
  out
}

run_xgb_forecast <- function(monthly_df, target, val_year, h, max_lag, roll_windows,
                             params = list(),
                             min_train_date = NULL) {   

  base_full <- monthly_df %>%
    add_time_features() %>%
    select(date, month, sin12, cos12, all_of(target))

  if (!is.null(min_train_date)) {
    min_train_date <- as.Date(min_train_date)
    base <- base_full %>% filter(date >= min_train_date)
  } else {
    base <- base_full
  }

  sup <- make_supervised(base, target, max_lag = max_lag, roll_windows = roll_windows)
  mats <- build_matrices(sup, target, val_year = val_year)
  model <- fit_xgb(mats$dtrain, mats$dvalid, params = params)
  valid_pred <- predict(model, mats$dvalid)
  valid_df <- mats$valid_df %>%
    mutate(pred = as.numeric(valid_pred),
           resid = .data[[target]] - pred)

  last_hist <- sup %>% filter(complete.cases(.))
  future_pts <- forecast_iterative(model, last_hist, target, mats$feats, h, max_lag, roll_windows)
  fc_with_pi <- bootstrap_intervals(future_pts, valid_df$resid, B = 1000, levels = c(0.80, 0.95))

  list(
    model = model,
    valid = valid_df,
    forecast = fc_with_pi,
    history = monthly_df %>% select(date, all_of(target))
  )
}
