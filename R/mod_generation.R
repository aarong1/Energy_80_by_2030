
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
        
        h4("Selecting a date shows the RES generated, dispatc down and the target for that day. The units are MWh."),

        # h4(HTML("$$\\text{SNSP} = \\frac{\\text{Actual RES} + \\text{Import}} {\\text{Demand} + \\text{Export}}$$")),
        # h4(HTML("$$\\text{Actual RES} = \\text{SNSP} * (\\text{Demand} + \\text{Export}) - \\text{Import}$$")),
        # h4(HTML("$$\\text{Curtailed RES} = \\text{Available RES} - \\text{Actual RES}$$")),
        
        
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
        h2("SNSP and Target Calculator",
           style = "color:#355070; margin-top:40px; margin-bottom:20px;"),
        p("Adjust parameters below to calculate SNSP and Target."),
        
        fluidRow(
          column(
            6,
            sliderInput(ns("calc_import"), "Import (MWh):",
                        min = 0,
                        max = 9000,
                        value = 4000
            ),
            sliderInput(ns("calc_export"), "Export (MWh):",
                        min = 0,
                        max = 3000,
                        value = 1000
            ),
            sliderInput(ns("calc_demand"), "Demand (MWh):",
                        min = 15000,
                        max = 25000,
                        value = 18000
            ),
            sliderInput(ns("calc_res"), "Generated RES (MWh):",
                        min = 0,
                        max = 15000,
                        value = 10000
          )
        ),
        column(
          6, 
          h4("Results"),
          tableOutput(ns("calc_curtailed_res"))
          
        )),

#-------clickable plots
        tags$hr(),
        h2("Interactive Trajectory and CI-Driven KPIs",
           style = "color:#355070; margin-top:40px; margin-bottom:20px;"),
        p("Click any plot to select a date; the selection propagates across all measurements.
           Choose which forecast series to use for downstream KPIs."),
        
        fluidRow(
          column(
            4,
            div(
              style = "margin-top: 8px;",
              strong("Selected date: "),
              textOutput(ns("selected_date_lbl2"), inline = TRUE),
              HTML("&nbsp;&nbsp;"),
              actionLink(ns("clear_selected_date2"), "Clear")
            )
          )
        ),
        
        uiOutput(ns("interactive_trajectory_plots")),

        h3("Historical Dispatch Down (Wind & Solar)"),
        plotOutput(ns("dd_history_plot"), height = "350px"),
        tags$hr(),

        radioButtons(
          inputId = ns("scenario_choice"),
          label = "Scenario:",
          choices = c(
            "Medium case" = "medium",
            "The worst case" = "worst",
            "Optimum case" = "best"
          ),
          selected = "medium",
          inline = TRUE
        ),

        checkboxInput(
          inputId = ns("downward_regulation"),
          label = "Progress an interim solution for downward regulation (negative reserve)",
          value = FALSE
        ),

        checkboxInput(
          inputId = ns("security_standards"),
          label = "Review of operational security standards",
          value = FALSE
        ),

        checkboxInput(
          inputId = ns("reduce_mustruns"),
          label = "Perform a review to reduce the number of must-run units from 3 to 2",
          value = FALSE
        ),

        checkboxInput(
          inputId = ns("phase1_lcis"),
          label = "Monitor the delivery of the Phase I Low Carbon Inertia Services",
          value = FALSE
        ),
        
        checkboxInput(
          inputId = ns("phase2_lcis"),
          label = "Commence procurement process of Phase II Low Carbon Inertia Services",
          value = FALSE
        ),
        
        checkboxInput(
          inputId = ns("reduce_moyle"),
          label = "Perform a review of ability of the TSO to reduce the net transfer capacity of the Moyle HVDC interconnector",
          value = FALSE
        ),

        checkboxInput(
          inputId = ns("ldes"),
          label = "SONI to coordinate with the Utility Regulator to create a credible for a procurement mechanism to procure enhanced system flexibility through Long Duration Energy Storage",
          value = FALSE
        ),
        
        checkboxInput(
          inputId = ns("sn_interconnector"),
          label = "Construction of the second North-South Interconnector",
          value = FALSE
        ),

        tableOutput(ns("selected_date_kpis")),
        
        h4("Selected Series Combined Forecast Table (MWh)"),
        tableOutput(ns("combined_forecast_table_ci")),
        downloadButton(ns("download_combined_forecast_ci"),
                       "Download Selected Series Forecast (CSV)"),
        tableOutput(ns("yearly_sums_table")),
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
        `Available Wind` = numeric(),
        `Available Solar` = numeric(),
        `Available RES` = numeric(),
        `Generated Wind` = numeric(),
        `Generated Solar` = numeric(),
        `Generated RES` = numeric(),
        `Fossil Fuel` = numeric(),
        Demand = numeric(),
        SNSP = numeric(),
        `Dispatch Down` = numeric(),
        `Target` = numeric(),
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
          `Available Wind` = .data$sum_avai_wind,
          `Available Solar` = .data$sum_avai_solar,
          `Available RES` = `Available Wind` + `Available Solar`,
          `Generated Wind` = .data$sum_wind, 
          `Generated Solar` = .data$sum_solar, 
          `Generated RES` = `Generated Wind` + `Generated Solar`,
          `Fossil Fuel` = .data$sum_system_gen.y,
          Demand = .data$sum_demand, 
          SNSP = .data$median_SNSP.x,
          `Dispatch Down` = `Available RES` - `Generated RES`,
          `Target` = `Generated RES`/Demand, 
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
        `Available Wind` = numeric(),
        `Available Solar` = numeric(),
        `Available RES` = numeric(),
        `Generated Wind` = numeric(),
        `Generated Solar` = numeric(),
        `Generated RES` = numeric(),
        `Fossil Fuel` = numeric(),
        Demand = numeric(),
        SNSP = numeric(),
        `Dispatch Down` = numeric(),
        `Target` = numeric(),
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
    generated_res_from_rv <- reactive({
      req(nrow(rv$table) > 0)
      rv$table %>%
        dplyr::transmute(
          date = as.Date(.data$Date),
          generated_res = .data$`Generated RES`
        ) %>%
        dplyr::arrange(.data$date)
    })
    
    available_res_from_rv <- reactive({
      req(nrow(rv$table) > 0)
      rv$table %>%
        dplyr::transmute(
          date = as.Date(.data$Date),
          available_res = .data$`Available RES`  
        ) %>%
        dplyr::arrange(.data$date)
    })
    
    monthly_from_rv <- reactive({
      req(nrow(rv$table) > 0)
      rv$table %>%
        dplyr::transmute(
          date = as.Date(.data$Date),
          Generated = .data$`Generated RES`,
          Available = .data$`Available RES`
        ) %>%
        dplyr::mutate(
          month_start = as.Date(format(.data$date, "%Y-%m-01"))
        ) %>%
        dplyr::group_by(.data$month_start) %>%
        dplyr::summarise(
          sum_gen_res = sum(.data$Generated, na.rm = TRUE),
          sum_ava_res = sum(.data$Available, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    output$res_ts <- renderPlot({
      gen_df <- generated_res_from_rv()
      ava_df <- available_res_from_rv()
      
      req(nrow(gen_df) > 0, nrow(ava_df) > 0)
      
      ggplot() +
        geom_line(
          data = gen_df,
          aes(x = date, y = generated_res, colour = "Generated RES"),
          linewidth = 0.6, alpha = 0.3
        ) +
        geom_smooth(
          data = gen_df,
          aes(x = date, y = generated_res, colour = "Generated RES"),
          method = "loess", span = 0.2, se = FALSE, linewidth = 1.1
        ) +
        
        geom_line(
          data = ava_df,
          aes(x = date, y = available_res, colour = "Available RES"),
          linewidth = 0.6, alpha = 0.3
        ) +
        geom_smooth(
          data = ava_df,
          aes(x = date, y = available_res, colour = "Available RES"),
          method = "loess", span = 0.2, se = FALSE, linewidth = 1.1
        ) +
        
        scale_colour_manual(values = c(
          "Generated RES" = "#eaac8b",
          "Available RES" = "#6d597a"
        )) +
        labs(
          title = "RES: Generated vs Available (daily + smooth)",
          x = NULL, y = "Units / Ratio", colour = NULL
        ) +
        theme_minimal()
    })
    
    output$res_monthly_stack <- renderPlot({
      m_df <- monthly_from_rv()
      req(nrow(m_df) > 0)
      
      res_yearly_long <- m_df %>%
        dplyr::select(month_start, Generated = .data$sum_gen_res) %>%
        dplyr::full_join(
          m_df %>% dplyr::select(month_start, Available = .data$sum_ava_res),
          by = "month_start"
        ) %>%
        tidyr::pivot_longer(
          cols = c(.data$Generated, .data$Available),
          names_to = "Type",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Type = factor(.data$Type, levels = c("Generated", "Available")),
          Year = format(as.Date(.data$month_start), "%Y")
        ) %>%
        dplyr::group_by(.data$Year, .data$Type) %>%
        dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = "drop")
      
      ggplot(res_yearly_long, aes(x = Year, y = Value, fill = Type, group = Type)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.7) +
        scale_fill_manual(values = c(
          "Generated" = "#eaac8b",
          "Available" = "#6d597a"
        )) +
        labs(
          title = "RES: Generated vs Available (Yearly sums, side-by-side)",
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

      plot(d, main = "RES (Solar + Wind) MWh", xlab = "Value", ylab = "Density",
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

      plot(d, main = "Demand (MWh)", xlab = "Value", ylab = "Density",
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
        plot(d, main = "RES (Solar + Wind) MWh", xlab = "Value", ylab = "Density",
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
        plot(d, main = "Demand (MWh)", xlab = "Value", ylab = "Density",
             col = "#6d597a", lwd = 2, xlim = c(0, ifelse(length(x)==0, 1, max(x))))
        arrows(qx, qy + dy, qx, qy, col = "#b56576", lwd = 2, length = 0.08)
        text(qx, qy + dy * 1.2, labels = req(input$q_choice_dem), col = "#b56576", cex = 0.9)
        axis(1, at = pretty(x), labels = format(pretty(x), big.mark = ",", scientific = FALSE))
        dev.off()
      }
    )

#---model-based predictions
    results <- local({
      file <- "./data/results_all_vars.rda"
      env <- new.env()
      load(file, envir = env)
      env$results
    })
    
    # results_lm <- list()
    # for (v in vars) {
    #   message("Fitting Linear model for: ", v)
    #   min_date <- if (v %in% c("sum_import", "sum_export")) "2022-01-01" else NULL
    #   
    #   results_lm[[v]] <- run_lm_forecast(
    #     monthly_df, target = v, val_year = val_year, h = h,
    #     min_train_date = min_date
    #   )
    # }
    
    output$all_forecast_plots <- renderUI({
      plot_list <- lapply(names(results), function(varname) {
        tagList(
          plotOutput(session$ns(paste0("plot_xgb_", varname)), height = "350px"),
          tags$br(),
          
          # h4("Linear Model Forecast"),
          # plotOutput(session$ns(paste0("plot_lm_", varname)), height = "350px"),
          # tags$hr()
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
          `Generated RES` = (sum_wind + sum_solar),
          `Available RES` = (sum_avai_wind + sum_avai_solar),
           Target = `Generated RES` / sum_demand,
          `Dispatch Down` = `Available RES` - `Generated RES`
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
      RES     <- input$calc_res     
      
      snsp <- (RES + Import)/(Demand + Export)
      target <- RES / Demand

      data.frame(
        Metric = c("SNSP",
                   "Target (%)"),
        Value = c(snsp, target),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
#---clickable plots 
    selected_date2 <- reactiveVal(NULL)
    output$selected_date_lbl2 <- renderText({
      if (is.null(selected_date2())) "—" else format(selected_date2(), "%Y-%m-%d")
    })
    
    observeEvent(input$clear_selected_date2, ignoreInit = TRUE, {
      selected_date2(NULL)
    })
    
    dd_hist <- read.csv("./data/dispatch_down_history.csv", stringsAsFactors = FALSE)
    percent_to_num <- function(x) as.numeric(sub("%", "", x)) / 100
    dd_hist <- dd_hist %>%
      mutate(across(where(is.character), percent_to_num)) %>%
      mutate(
        wind_total  = wind_constraint + wind_curtailment,
        solar_total = solar_constraint + solar_curtailment
      )
    
    line_types <- c(
      "Total" = "solid",
      "Constraint" = "dashed",
      "Curtailment" = "dotdash"
    )
    
    output$dd_history_plot <- renderPlot({
      dd_long <- dd_hist %>%
        dplyr::select(
          year,
          wind_total,
          wind_constraint,
          wind_curtailment,
          solar_total,
          solar_constraint,
          solar_curtailment
        ) %>%
        tidyr::pivot_longer(
          cols = -year,
          names_to = "type",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          source = dplyr::case_when(
            grepl("^wind",  type) ~ "Wind",
            grepl("^solar", type) ~ "Solar",
            TRUE ~ NA_character_
          ),
          category = dplyr::case_when(
            grepl("total",       type) ~ "Total",
            grepl("constraint",  type) ~ "Constraint",
            grepl("curtailment", type) ~ "Curtailment",
            TRUE ~ NA_character_
          ),
          year = as.integer(year)
        ) %>%
        dplyr::filter(!is.na(source), !is.na(category))
      
      source_colors <- c(
        "Wind"  = "#1f77b4",  
        "Solar" = "#e6550d"  
      )
      
      line_types <- c(
        "Total"       = "solid",
        "Constraint"  = "dashed",
        "Curtailment" = "dotdash"
      )
      
      point_shapes <- c(
        "Total"       = 16,
        "Constraint"  = 17,
        "Curtailment" = 15
      )
      
      ggplot(dd_long, aes(
        x = year,
        y = value,
        color = source,
        linetype = category,
        shape = category
      )) +
        geom_line(linewidth = 1.2, alpha = 0.9) +
        geom_point(size = 2.4, alpha = 0.9) +
        scale_color_manual(values = source_colors, name = "Source") +
        scale_linetype_manual(values = line_types, name = "Component") +
        scale_shape_manual(values = point_shapes, name = "Component") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        scale_x_continuous(breaks = scales::pretty_breaks()) +
        labs(
          title = "Historical Dispatch‑Down (%): Wind vs Solar",
          subtitle = "Line type = Total / Constraint / Curtailment  •  Color = Wind vs Solar",
          x = "Year",
          y = "Percentage"
        ) +
        guides(
          color = guide_legend(order = 1, override.aes = list(linetype = "solid", shape = 16)),
          linetype = guide_legend(order = 2),
          shape = "none"  
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          legend.title = element_text(size = 11),
          legend.text  = element_text(size = 10)
        )
    })
    
    output$interactive_trajectory_plots <- renderUI({
      req(results)
      vars <- names(results)                     
      plot_blocks <- lapply(seq_along(vars), function(i) {
        varname <- vars[i]                       
        tagList(
          h4(paste(
            "Trajectory:",
            if (!is.null(nice_names[[varname]])) nice_names[[varname]] else varname
          )),
          
          radioButtons(
            inputId = session$ns(paste0("ci_choice2_", varname)),
            label   = paste(
              "Value for",
              if (!is.null(nice_names[[varname]])) nice_names[[varname]] else varname
            ),
            choices = c(
              "Forecast (point)" = "pred",
              "Lower 80%"       = "lo80",
              "Upper 80%"       = "hi80",
              "Lower 95%"       = "lo95",
              "Upper 95%"       = "hi95"
            ),
            selected = "pred",
            inline = TRUE
          ),
          
          plotOutput(
            session$ns(paste0("plot_click_", varname)),
            height = "300px",
            click  = session$ns(paste0("plot_click_", varname, "_click"))
          ),
          tags$hr()
        )
      })
      do.call(tagList, plot_blocks)
    })
    
    observe({
      req(results)  
      vars <- names(results)
      
      for (v in vars) {
        local({
          vn <- v 
          observeEvent(input[[paste0("plot_click_", vn, "_click")]], {
            click <- input[[paste0("plot_click_", vn, "_click")]]
            if (!is.null(click$x)) {
              df_dates <- results[[vn]]$forecast$date
              df_dates <- as.Date(df_dates, origin = if (is.numeric(df_dates)) "1970-01-01" else NULL)
              selected_date2(snap_to_nearest_date(click$x, df_dates))
            }
          }, ignoreInit = TRUE)
          
          output[[paste0("plot_click_", vn)]] <- renderPlot({
            p <- plot_fc(results[[vn]], vn)  
            sd <- selected_date2()
            if (!is.null(sd)) {
              p <- p + ggplot2::geom_vline(
                xintercept = as.numeric(sd),
                colour = "#B56576",
                linetype = "dashed",
                linewidth = 0.6
              )
            }
            p
          })
        })
      }
    })
    
    combined_forecast_ci <- reactive({
      req(results)
      
      vars <- names(results)
      choices <- setNames(
        lapply(vars, function(v) input[[paste0("ci_choice2_", v)]] %||% "pred"),
        vars
      )
      
      tables <- lapply(vars, function(varname) {
        df <- results[[varname]]$forecast
        df$date <- as.Date(df$date, origin = "1970-01-01")
        
        value_col <- pick_fc_col(df, choices[[varname]])
        
        
        df %>%
          dplyr::select(date, !!rlang::sym(value_col)) %>%
          dplyr::rename(!!varname := !!rlang::sym(value_col))
      })

      df_all <- Reduce(function(x, y) full_join(x, y, by = "date"), tables) %>%
        arrange(date)
      #apply scenarios
      scenario <- input$scenario_choice
      
      if (scenario == "worst") {
        {
          fc  <- results$sum_solar$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_solar <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_solar = dplyr::coalesce(lo95_solar, sum_solar))
        }
        
        {
          fc  <- results$sum_wind$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_wind <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_wind = dplyr::coalesce(lo95_wind, sum_wind))
        }
        
        {
          fc  <- results$sum_demand$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95")
          hi95_demand <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_demand = dplyr::coalesce(hi95_demand, sum_demand))
        }
      }
      
      if (scenario == "best") {
        {
          fc  <- results$sum_solar$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95") 
          hi95_solar <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_solar = dplyr::coalesce(hi95_solar, sum_solar))
        }
        
        {
          fc  <- results$sum_wind$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95")
          hi95_wind <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_wind = dplyr::coalesce(hi95_wind, sum_wind))
        }
        
        {
          fc  <- results$sum_demand$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_demand <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_demand = dplyr::coalesce(lo95_demand, sum_demand))
        }
      }
      #apply policies
      df_all <- df_all %>%
        mutate(`Generated RES` = sum_wind + sum_solar)
      
     
      if (isTRUE(input$downward_regulation)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + 50,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$security_standards)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + 75,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$reduce_mustruns)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + 100,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$phase1_lcis)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2027-02-01"),
                                     `Generated RES` + 100,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$phase2_lcis)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2029-01-01"),
                                     `Generated RES` + 100,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$reduce_moyle)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + 850,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$ldes)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2027-04-01"),
                                     `Generated RES` + 600,
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$sn_interconnector)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + 900,
                                     `Generated RES`)
          )
      }
      
      df_all <- df_all %>%
        mutate(
          Target = `Generated RES` / sum_demand,
          SNSP   = (`Generated RES`  + sum_import) / (sum_demand + sum_export),
          
          `Planned SNSP` = case_when(
            lubridate::year(date) >= 2025 & lubridate::year(date) <= 2027 ~ 0.80,
            lubridate::year(date) >= 2028 & lubridate::year(date) <= 2029 ~ 0.85,
            lubridate::year(date) == 2030 ~ 0.90,
            TRUE ~ NA_real_
          ),
          # `Generated RES` = sum_res,
          `Dispatch Down Wind`  = sum_avai_wind  - sum_wind,
          `Dispatch Down Solar` = sum_avai_solar - sum_solar,
          .dd_total = `Dispatch Down Wind` + `Dispatch Down Solar`,
          
          Curtailment = if_else(
            .dd_total > 0,
            (0.1667 * `Dispatch Down Wind` + 0.1461 * `Dispatch Down Solar`) / .dd_total,
            NA_real_
          ),
          Constraint = if_else(
            .dd_total > 0,
            (0.8318 * `Dispatch Down Wind` + 0.8542 * `Dispatch Down Solar`) / .dd_total,
            NA_real_
          ),
          
          `Curtailment SNSP` = ifelse(SNSP > `Planned SNSP`, "Curtailment SNSP", "")
        ) %>%
        select(-.dd_total) %>%
        rename_with(~ nice_names[.x], .cols = names(nice_names)) %>%
        mutate(date = format(date, "%Y-%m-%d"))
    })
    
    
    output$combined_forecast_table_ci <- renderTable({
      combined_forecast_ci()
    }, digits = 3, rownames = FALSE)
    
    output$download_combined_forecast_ci <- downloadHandler(
      filename = function() paste0("selected_series_forecast_", Sys.Date(), ".csv"),
      content  = function(file) {
        write.csv(combined_forecast_ci(), file, row.names = FALSE)
      }
    )
    
    output$selected_date_kpis <- renderTable({
      df <- combined_forecast_ci()
      sd <- selected_date2()
      
      if (is.null(sd)) {
        return(data.frame(
          Metric = c("Selected date"),
          Value  = c("—"),
          check.names = FALSE, stringsAsFactors = FALSE
        ))
      }
      
      df$date <- as.Date(df$date)
      row <- df[df$date == sd, , drop = FALSE]
      
      if (nrow(row) == 0) {
        return(data.frame(
          Metric = c("Selected date"),
          Value  = c("No data for selected date"),
          check.names = FALSE, stringsAsFactors = FALSE
        ))
      }
      
      target          <- row$Target 
      `Dispatch Down` <- row$`Dispatch Down Wind` + row$`Dispatch Down Solar`
      
      data.frame(
        Metric = c("Target", "Dispatch Down"),
        Value = c(
          format(target,    big.mark = ",", scientific = FALSE),
          format(round(`Dispatch Down`),    big.mark = ",", scientific = FALSE)
        ),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }, digits = 3, rownames = FALSE)
    
    output$yearly_sums_table <- renderTable({
      df <- combined_forecast_ci()
      df$date <- as.Date(df$date)
      
      df$year <- format(df$date, "%Y")
      
      num_cols <- sapply(df, is.numeric)
      
      yearly_df <- df %>%
        group_by(year) %>%
        summarise(across(which(num_cols), mean, na.rm = TRUE)) %>%
        
        mutate(
          Target = `Generated RES` / `Demand`,
          SNSP   = (`Generated RES` + `Imports`) /
            (`Demand` + `Exports`)
        ) %>%
        
        mutate(
          `Planned SNSP` = case_when(
            year >= 2025 & year <= 2027 ~ 0.80,
            year >= 2028 & year <= 2029 ~ 0.85,
            year == 2030               ~ 0.90,
            TRUE                       ~ NA_real_
          )
        ) %>%
        mutate(.dd_total = `Dispatch Down Wind` + `Dispatch Down Solar`,
          
          Curtailment = if_else(
            .dd_total > 0,
            (0.1667 * `Dispatch Down Wind` + 0.1461 * `Dispatch Down Solar`) / .dd_total,
            NA_real_
          ),
          Constraint = if_else(
            .dd_total > 0,
            (0.8318 * `Dispatch Down Wind` + 0.8542 * `Dispatch Down Solar`) / .dd_total,
            NA_real_
          ),
          
          `Curtailment SNSP` = ifelse(SNSP > `Planned SNSP`, "Curtailment SNSP", ""))
      
      yearly_df
    }, digits = 3, rownames = FALSE)
   
    
    
    
#end of server    
    })
  }
