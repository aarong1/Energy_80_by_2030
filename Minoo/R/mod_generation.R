#ui
generation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Generation Dashboard", 
       style = "color:#2a2a2a; margin-bottom:25px;"),
    
    tabsetPanel(
      id = ns("tabs"),
      
      tabPanel(
        title = "Assumptions & Current Data",
        
#-------daily and monthly SNSP (historical data)
        h2("Tier 1: A11 – SNSP, inertia, RoCoF & minimum conventional generation limits",
           style = "color: #355070; margin-bottom : 70px; text-align: left;"),
        
        
        h4(HTML(
          'According to the
     <a href="https://cms.eirgrid.ie/operational-policy-roadmap-2025-2035" target="_blank">
       Operational Policy Roadmap 2025-2035 </a>, 
     the assumed SNSP is 75%. The 
     <a href="https://www.soni.ltd.uk/grid/system-and-renewable-data-reports" target="_blank">
       Eirgrid Live Dashboard </a> indicates daily and monthly median values of SNSP.')),
        
        div(id = ns("content"),
            plotOutput(ns("snsp_plot"), height = "420px")
        ),
        
        tags$hr(style="margin-top:60px;"),
        
#-------calculate curtailed RES for any specific date
        h2("Tier 1: A1 – Single weather year (e.g. 2020) used for wind & solar.",
           style = "color: #355070; margin-bottom : 70px; text-align: left;"),
           
        h4("If 2020 was windier / sunnier / milder than average, then RES output and adequacy look better than over a decade.
        In a “bad” year (low wind, cold snaps) you might need a lot more fossil generation than the model shows.
        You need to know how robust 80% is across a range of weather years, not just one."),
        
        h4("RES (wind + solar) output is curtailed by SNSP limits. Selecting a date shows the actual RES generated, 
           the RES curtailed, and the gap between the achieved and the potential target."),

        
        h4(HTML("$$\\text{SNSP} = \\frac{\\text{Curtailed RES} + \\text{Import}} {\\text{Demand} + \\text{Export}}$$")),
        h4(HTML("$$\\text{Curtailed RES} = \\text{SNSP} * (\\text{Demand} + \\text{Export}) - \\text{Import}$$")),
        
        
        dateInput(
          ns("selected_date"),
          "Choose a date:",
          value = as.Date("2025-11-30"),
          min   = as.Date("2014-01-01"),
          max   = as.Date("2025-11-30")
        ),
        
        tableOutput(ns("daily_import_export")),
        actionButton(ns("reset_daily_import_export"), "Reset table",
                     style = "background-color:#eaac8b; border-color:#eaac8b; color:#fff;"),
        downloadButton(ns("daily_download"), "Download table (CSV)"),
        
        tags$hr(style="margin-top:60px;"),
        
#-------curtailed vs potential RES
        plotOutput(ns("res_ts"), height = 280),
        plotOutput(ns("res_monthly_stack"), height = 280),
        
#-------RES and demand distributions
        tags$hr(style="margin-top:60px;"),
        h2("Tier 1: A4 – Single “median” demand forecast used (no high/low)",
           style = "color: #355070; margin-bottom : 70px; text-align: left;"),
        
        h4("80% is a ratio: RES generation / total demand. If demand ends up significantly higher than assumed and RES build-out doesn’t keep pace, you miss 80%.
           If demand shape (winter peaks, electrification timing) is different, you can hit system security limits sooner, increasing curtailment."),
        
        h4("RES (Solar + Wind) vs Total Demand – Percentile Selector"),
        
        fluidRow(
          column(3,
                 selectInput(
                   ns("q_choice_res"), "Percentile (RES)",
                   choices = c("p01", "p05", "p10", "p25", "median", "p75", "p90", "p95", "p99"),
                   selected = "median")
          ),
          column(3,
                 selectInput(
                   ns("q_choice_dem"), "Percentile (Demand)",
                   choices = c("p01", "p05", "p10", "p25", "median", "p75", "p90", "p95", "p99"),
                   selected = "median")
          ),
          column(6,
                 uiOutput(ns("res_demand_ratio_text"))
          )
        ),
        
        fluidRow(
          column(6, plotOutput(ns("res_plot"), height = 400)),
          column(6, plotOutput(ns("demand_plot"), height = 400))
        ),
        
        downloadButton(ns("download_res_plot"), "Download RES (PNG)"),
        downloadButton(ns("download_demand_plot"), "Download Demand (PNG)"),
        tags$hr(style="margin-top:60px;")
        ),
      
      tabPanel(
        title = "Model‑Based Predictions",
        
#-------model-based predictions
        h2("Model-Based Predictions", style = "color:#355070; margin-bottom:40px;"),
        h3("Forecast Plots"),
        uiOutput(ns("all_forecast_plots")),
        tags$hr(),
        h3("XGBoost Forecast Table"),
        
#-------table based on predicted values
        tableOutput(ns("combined_forecast_table")),
        downloadButton(ns("download_combined_forecast"), "Download XGBoost Forecast (CSV)"),
        
        # h3("Combined Forecast Table_Linear"),
        # tableOutput(ns("combined_forecast_table_lm")),
        # downloadButton(ns("download_combined_forecast_lm"), "Download Combined Forecast (CSV)"),
        
#-------sliders for individual measurements
        h2("Curtailed RES Calculator",
           style = "color:#355070; margin-top:40px; margin-bottom:20px;"),
        p("Adjust parameters below to calculate Curtailed RES, Curtailed Target, and Potential Target."),
        
        fluidRow(
          column(
            6,
            sliderInput(ns("calc_import"), "Import:",
                        min = 0,
                        max = 50000,
                        value = 10000
            ),
            sliderInput(ns("calc_export"), "Export:",
                        min = 0,
                        max = 50000,
                        value = 10000
            ),
            sliderInput(ns("calc_demand"), "Demand:",
                        min = 50000,
                        max = 120000,
                        value = 70000
            ),
            sliderInput(ns("calc_snsp"), "SNSP:",
                        min = 0,
                        max = 1,
                        value = 0.2
            ),
            sliderInput(ns("calc_res"), "RES:",
                        min = 0,
                        max = 50000,
                        value = 10000
          )
        ),
        column(
          6, 
          h4("Results"),
          tableOutput(ns("calc_curtailed_res"))
          
        )),
        )
      )
    ) 
}

#server
generation_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {

#---daily and monthly SNSP (historical data)
    prediction_schedule <- tibble::tibble(
      start = as.POSIXct(c(
        "2014-01-01 00:00:00",
        "2016-10-01 00:00:00",
        "2017-10-01 00:00:00",
        "2018-07-01 00:00:00",
        "2022-01-01 00:00:00",
        "2025-01-01 00:00:00",
        "2027-01-01 00:00:00",
        "2029-01-01 00:00:00",
        "2030-01-01 00:00:00",
        "2035-01-01 00:00:00"
      ), tz = "Europe/Dublin"),
      end = as.POSIXct(c(
        "2016-09-30 23:59:59",
        "2017-09-30 23:59:59",
        "2018-06-30 23:59:59",
        "2021-12-31 23:59:59",
        "2024-12-31 23:59:59",
        "2026-12-31 23:59:59",
        "2028-12-31 23:59:59",
        "2029-12-31 23:59:59",
        "2034-12-31 23:59:59",
        "2035-12-31 23:59:59"
      ), tz = "Europe/Dublin"),
      value = c(50, 55, 65, 70, 75, 80, 85, 90, 95, 100)
    )

    output$snsp_plot <- renderPlot({
      ggplot2::ggplot() +
        ggplot2::geom_point(
          data = daily_median_snsp,
          ggplot2::aes(x = date, y = median_SNSP * 100),
          color = "#dedbd2",
          size = 1.8,
          alpha = 0.7
        ) +
        ggplot2::geom_point(
          data = monthly_median_snsp,
          ggplot2::aes(x = month_start, y = median_SNSP * 100),
          color = "#e56b6f",  
          size = 3.2,
          alpha = 0.9
        ) +
        ggplot2::geom_segment(
          data = prediction_schedule,
          ggplot2::aes(x = as.Date(start), xend = as.Date(end), y = value, yend = value),
          color = "#6d597a", linewidth = 1.1, linetype = "dashed"
        ) +
        ggplot2::scale_x_date(
          date_breaks = "1 year",
          date_labels = "%Y"
        ) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::labs(
          title = "Daily (Gray) and Monthly (Red) Median SNSP vs Predicted Trial Targets",
          x = "Date",
          y = "SNSP (%)",
          caption = "Gray dots: daily median | red dots: monthly median | purple dashed: trial targets"
        ) +
        ggplot2::theme_minimal(base_size = 12)})

#---calculate curtailed RES for any specific date
    rv <- reactiveValues(
      table = tibble::tibble(
        Date = character(),
        Import = numeric(),
        Export = numeric(),
        `System Generation` = numeric(),
        `Renewable (RES)` = numeric(),
        Wind = numeric(),
        Solar = numeric(),
        `Fossil Fuel` = numeric(),
        Demand = numeric(),
        SNSP = numeric(),
        `Curtailed RES` = numeric(),
        `Target` = numeric(),
        `Potential Target` = numeric()
      )
    )
    
    observeEvent(input$selected_date, {
      req(input$selected_date)
      new_row <- combined_df %>%
        dplyr::filter(.data$date == as.Date(input$selected_date)) %>%
        dplyr::transmute(
          Date   = format(.data$date, "%Y-%m-%d"),  
          Import = .data$sum_import,
          Export = .data$sum_export,
          `System Generation` = .data$sum_system_gen.x,
          `Renewable (RES)` = .data$sum_res, 
          Wind = .data$sum_wind, 
          Solar = .data$sum_solar, 
          `Fossil Fuel` = .data$sum_system_gen.y,
          Demand = .data$sum_demand, 
          SNSP = .data$median_SNSP.x, 
          `Curtailed RES` = (.data$median_SNSP.x * (.data$sum_demand + .data$sum_export))-.data$sum_import,
          `Target` = (((.data$median_SNSP.x * (.data$sum_demand + .data$sum_export))-.data$sum_import)/.data$sum_demand),
          `Potential Target` = (.data$sum_res/.data$sum_demand)
        ) %>%
        dplyr::slice_head(n = 1)   
      
      if (nrow(new_row) == 0) {
        return(NULL)
      }
      
      rv$table <- rv$table %>%
        dplyr::filter(.data$Date != new_row$Date) %>%
        dplyr::bind_rows(new_row) %>%
        dplyr::arrange(.data$Date)
    }, ignoreInit = TRUE)
    
   
    output$daily_import_export <- renderTable({
      rv$table
    }, digits = 2, rownames = FALSE)
    
    observeEvent(input$reset_daily_import_export, { #to reset the table
      rv$table <- tibble::tibble(
        Date = character(),
        Import = numeric(),
        Export = numeric(),
        `System Generation` = numeric(),
        `Renewable (RES)` = numeric(),
        Wind = numeric(),
        Solar = numeric(),
        `Fossil Fuel` = numeric(),
        Demand = numeric(),
        SNSP = numeric(),
        `Curtailed RES` = numeric(),
        `Target` = numeric(),
        `Potential Target` = numeric()
      )
    })
    
    output$daily_download <- downloadHandler( #to download the table
      filename = function() {
        paste0("selected_days_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(rv$table, file, row.names = FALSE)
      }
    )

#---curtailed vs potential RES
    output$res_ts <- renderPlot({
      ggplot() +
        geom_line(data = curtailment_res,
                  aes(x = date, y = median_SNSP, colour = "Curtailed RES"),
                  linewidth = 0.6, alpha = 0.3) +
        geom_smooth(data = curtailment_res,
                    aes(x = date, y = median_SNSP, colour = "Curtailed RES"),
                    method = "loess", span = 0.2, se = FALSE, linewidth = 1.1) +
        
        geom_line(data = daily_sum_res,
                  aes(x = date, y = sum_res, colour = "Potential RES"),
                  linewidth = 0.6, alpha = 0.3) +
        geom_smooth(data = daily_sum_res,
                    aes(x = date, y = sum_res, colour = "Potential RES"),
                    method = "loess", span = 0.2, se = FALSE, linewidth = 1.1) +
        
        scale_colour_manual(values = c("Curtailed RES" = "#eaac8b",
                                       "Potential RES" = "#6d597a")) +
        labs(title = "RES: Curtailed vs Potential (daily median + smooth)",
             x = NULL, y = "Ratio / Units",         x = NULL, y = "Ratio / Units", colour = NULL) +
        theme_minimal()
    })
    
    output$res_monthly_stack <- renderPlot({
      res_yearly_long <- monthly_sum_cur_res %>%
        dplyr::select(month_start, Curtailed = sum_cur_res) %>%
        dplyr::full_join(
          monthly_sum_pot_res %>% dplyr::select(month_start, Potential = sum_pot_res),
          by = "month_start"
        ) %>%
        tidyr::pivot_longer(
          cols = c(Curtailed, Potential),
          names_to = "Type",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Type = factor(Type, levels = c("Curtailed", "Potential")),
          Year = format(as.Date(month_start), "%Y")   
        ) %>%
        dplyr::group_by(Year, Type) %>%
        dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      
      ggplot(res_yearly_long, aes(x = Year, y = Value, fill = Type, group = Type)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.7) +  
        scale_fill_manual(
          values = c("Curtailed" = "#eaac8b",
                     "Potential" = "#6d597a")
        ) +
        labs(
          title = "RES: Curtailed vs Potential (Yearly sums, side-by-side)",
          x = NULL,
          y = "Units",
          fill = NULL
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    })
    
#---RES and demand distributions
    res_vec <- reactive({
      req(combined_df)
      v <- combined_df$sum_res
      validate(need(length(v) > 0, "RES: no data available."))
      pmax(v, 0)
    })
    
    demand_vec <- reactive({
      req(combined_df)
      v <- combined_df$sum_demand
      validate(need(length(v) > 0, "Demand: no data available."))
      pmax(v, 0)
    })
    
    q_probs <- c(p01 = 0.01, p05 = 0.05, p10 = 0.10, p25 = 0.25, median = 0.50, p75 = 0.75, 
                 p90 = 0.90, p95 = 0.95, p99 = 0.99)
    
    selected_prob_res <- reactive({ q_probs[[ req(input$q_choice_res) ]] })
    selected_prob_dem <- reactive({ q_probs[[ req(input$q_choice_dem) ]] })

    q_res    <- reactive({ stats::quantile(res_vec(),    probs = selected_prob_res(), na.rm = TRUE) })
    q_demand <- reactive({ stats::quantile(demand_vec(), probs = selected_prob_dem(), na.rm = TRUE) })

    output$res_demand_ratio_text <- renderUI({
      num <- as.numeric(q_res())
      den <- as.numeric(q_demand())
      rpct <- if (!is.finite(den) || den == 0) NA_real_ else 100 * num / den
      
      num_fmt <- scales::comma(num)
      den_fmt <- scales::comma(den)
      pct_fmt <- if (is.na(rpct)) "undefined" else sprintf("%.1f", rpct)
      
      formula <- if (is.na(rpct)) {
        sprintf("$$\\text{Ratio} = \\text{undefined (Demand = 0)}$$")
      } else {
        sprintf("$$\\text{Ratio} = \\frac{\\text{%s}}{\\text{%s}} \\times 100 = %s\\%%$$",
                num_fmt, den_fmt, pct_fmt)
      }
      withMathJax(HTML(formula))
    })
    
    output$res_plot <- renderPlot({
      options(scipen = 999)
      x <- res_vec()
      validate(need(length(x) > 1, "No RES data to plot."))

      d  <- density(x, from = 0, cut = 0)
      qx <- as.numeric(q_res())
      qy <- approx(d$x, d$y, xout = qx, rule = 2)$y
      dy <- 0.08 * max(d$y)

      plot(d, main = "RES (Solar + Wind)", xlab = "Value", ylab = "Density",
           col = "#e56b6f", lwd = 2, xlim = c(0, max(x)), xaxt = "n" )
      arrows(qx, qy + dy, qx, qy, col = "#b56576", lwd = 2, length = 0.08)
      text(qx, qy + dy * 1.2, labels = req(input$q_choice_res), col = "#b56576", cex = 0.9)
      axis(1, at = pretty(x), labels = format(pretty(x), big.mark = ",", scientific = FALSE))
    })

    output$demand_plot <- renderPlot({
      options(scipen = 999)
      x <- demand_vec()
      validate(need(length(x) > 1, "No Demand data to plot."))

      d  <- density(x)
      qx <- as.numeric(q_demand())
      qy <- approx(d$x, d$y, xout = qx, rule = 2)$y
      dy <- 0.08 * max(d$y)

      plot(d, main = "Total Demand", xlab = "Value", ylab = "Density",
           col = "#6d597a", lwd = 2, xaxt = "n")
      arrows(qx, qy + dy, qx, qy, col = "#b56576", lwd = 2, length = 0.08)
      text(qx, qy + dy * 1.2, labels = req(input$q_choice_dem), col = "#b56576", cex = 0.9)
      axis(1, at = pretty(x), labels = format(pretty(x), big.mark = ",", scientific = FALSE))
    })

    output$download_res_plot <- downloadHandler( #to download the plots
      filename = function() paste0("RES_Density_", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = 1000, height = 600, res = 150)
        x <- res_vec()
        d  <- density(x)
        qx <- as.numeric(q_res())
        qy <- approx(d$x, d$y, xout = qx, rule = 2)$y
        dy <- 0.08 * max(d$y)
        plot(d, main = "RES (Solar + Wind)", xlab = "Value", ylab = "Density",
             col = "#e56b6f", lwd = 2, xlim = c(0, ifelse(length(x)==0, 1, max(x))))
        arrows(qx, qy + dy, qx, qy, col = "#b56576", lwd = 2, length = 0.08)
        text(qx, qy + dy * 1.2, labels = req(input$q_choice_res), col = "#b56576", cex = 0.9)
        axis(1, at = pretty(x), labels = format(pretty(x), big.mark = ",", scientific = FALSE))
        dev.off()
      }
    )

    output$download_demand_plot <- downloadHandler(
      filename = function() paste0("Demand_Density_", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = 1000, height = 600, res = 150)
        x <- demand_vec()
        d  <- density(x)
        qx <- as.numeric(q_demand())
        qy <- approx(d$x, d$y, xout = qx, rule = 2)$y
        dy <- 0.08 * max(d$y)
        plot(d, main = "Total Demand", xlab = "Value", ylab = "Density",
             col = "#6d597a", lwd = 2, xlim = c(0, ifelse(length(x)==0, 1, max(x))))
        arrows(qx, qy + dy, qx, qy, col = "#b56576", lwd = 2, length = 0.08)
        text(qx, qy + dy * 1.2, labels = req(input$q_choice_dem), col = "#b56576", cex = 0.9)
        axis(1, at = pretty(x), labels = format(pretty(x), big.mark = ",", scientific = FALSE))
        dev.off()
      }
    )

#---model-based predictions
    results <- local({
      file <- "C:/Users/YA775WS/OneDrive - EY/Desktop/SIB/whole_picture_2/data/results_all_vars.rda"
      env <- new.env()
      load(file, envir = env)
      env$results
    })
    
    results_lm <- list()
    for (v in vars) {
      message("Fitting Linear model for: ", v)
      min_date <- if (v %in% c("sum_import", "sum_export")) "2022-01-01" else NULL
      
      results_lm[[v]] <- run_lm_forecast(
        monthly_df, target = v, val_year = val_year, h = h,
        min_train_date = min_date
      )
    }
    
    output$all_forecast_plots <- renderUI({
      plot_list <- lapply(names(results), function(varname) {
        tagList(
          h3(paste("Forecast:", nice_names[[varname]])),
          h4("XGBoost Forecast"),
          plotOutput(session$ns(paste0("plot_xgb_", varname)), height = "350px"),
          tags$br(),
          
          h4("Linear Model Forecast"),
          plotOutput(session$ns(paste0("plot_lm_", varname)), height = "350px"),
          tags$hr()
        )
      })
      
      for (v in names(results)) {
        local({
          vn <- v
          output[[paste0("plot_xgb_", vn)]] <- renderPlot({
            plot_fc(results[[vn]], vn)
          })
          
          output[[paste0("plot_lm_", vn)]] <- renderPlot({
            plot_fc_lm(results_lm[[vn]], vn)
          })
        })
      }
      
      do.call(tagList, plot_list)
    })

#---table based on predicted values
    combined_forecast <- reactive({
      tables <- lapply(names(results), function(varname) {
        df <- results[[varname]]$forecast

        if (is.numeric(df$date)) {
          df$date <- as.Date(df$date, origin = "1970-01-01")
        }

        df %>%
          select(date, pred) %>%
          rename(!!varname := pred)
      })

      df_all <- Reduce(function(x, y) full_join(x, y, by = "date"), tables) %>%
        arrange(date)

      df_all <- df_all %>%
        mutate(
          curtailed_res    = median_SNSP.x * (sum_demand + sum_export) - sum_import,
          curtailed_target = curtailed_res / sum_demand,
          potential_target = sum_res / sum_demand
        ) %>%
        rename_with(~ nice_names[.x], .cols = names(nice_names)) %>%
        mutate(date = format(date, "%Y-%m-%d"))
      df_all
    })
    
    output$combined_forecast_table <- renderTable({
      combined_forecast()
    }, digits = 3, rownames = FALSE)
    
    output$download_combined_forecast <- downloadHandler(
      filename = function() paste0("combined_forecast_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(combined_forecast(), file, row.names = FALSE)
      }
    )
    
#---sliders for individual measurements
    output$calc_curtailed_res <- renderTable({
      Import  <- input$calc_import
      Export  <- input$calc_export
      Demand  <- input$calc_demand
      SNSP    <- input$calc_snsp
      RES     <- input$calc_res     
      
      curtailed_res    <- SNSP * (Demand + Export) - Import
      curtailed_target <- curtailed_res / Demand
      potential_target <- RES / Demand
      
      data.frame(
        Metric = c("Curtailed RES",
                   "Curtailed Target (%)",
                   "Potential Target (%)"),
        Value = c(
          format(round(curtailed_res), big.mark = ",", scientific = FALSE),
          if (Demand == 0 || is.na(curtailed_target)) "—"
          else sprintf("%.2f%%", 100 * curtailed_target),
          if (Demand == 0 || is.na(potential_target))   "—"
          else sprintf("%.2f%%", 100 * potential_target)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
#end of server    
    })
  }

