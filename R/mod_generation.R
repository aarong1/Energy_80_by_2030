library(shiny)
library(bslib)
library(lubridate)
library(tidyverse)
library(future)
library(promises)
library(memoise)
library(digest)
library(echarts4r)
print('f')
#ui
f <- function(p){format(big.mark=',',round(p))}

target_date = '2030-01-01'
end_date = '2032-01-01'

mons = unique(floor_date(seq( from = as.Date('2025-01-01'), 
                              to = as.Date(end_date),
                              by = 1 ), 'month')
)




generation_ui <- function(id) {
  ns <<- NS(id)
  
  tagList(
  #     tags$nav(class = "navbar navbar-expand-lg glass-card rounded-0 p-2 fixed-top", #  bg-white
  #          `data-bs-theme` = "light",
  #   div(class = "container-fluid d-inline",
  #     tags$a(class = "navbar-brand fw-bold d-inline", href = "#top",
  #       "Generation Dashboard",#br(),
  #       p(class = 'lead d-inline',"80% by 2030")
  #     )
  #   )
  # ),
br(),br(),
    tabsetPanel(    type = 'pills',
      id = ns("tabs"),
      
      tabPanel(
        title = "Assumptions & Current Data",
        
#-------daily and monthly SNSP (historical data)
hr(),
br(),br(),
        div(class= 'alert alert-light', 
            h2("Tier 1: A11 – SNSP, inertia, RoCoF & minimum conventional generation limits"#,
           # style = "color: #355070; margin-bottom : 70px; text-align: left;")
            )
           ),
        
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
      
      tabPanel(class = 'p-5 m-5',
        title = "Model‑Based Predictions",
        hr(),
        br(),br(),
        div(class= 'alert alert-light', h2('Predictions to 2030')),
        # Policy Options Card
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            class = "shadow-sm border-0 rounded-3",
            card_header(
              class = "bg-white h5 fw-bolder p-3",
              "Operational & Technical Reviews"
            ),
            card_body(
              div(
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("downward_regulation"),
                    "Progress an interim solution for downward regulation (negative reserve)"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("downward_regulation"),
                    h5(class = 'h4 fw-bold',2026)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("downward_regulation"),
                      autocomplete = "off"
                    )
                  )
                ),
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("security_standards"),
                    "Review of operational security standards"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("security_standards"),
                    h5(class = 'h4 fw-bold',2026)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("security_standards"),
                      autocomplete = "off"
                    )
                  )
                ),
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("reduce_mustruns"),
                    "Perform a review to reduce the number of must-run units from 3 to 2"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("reduce_mustruns"),
                    h5(class = 'h4 fw-bold',2026)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("reduce_mustruns"),
                      autocomplete = "off"
                    )
                  )
                ),
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("reduce_moyle"),
                    "Perform a review of ability of the TSO to reduce the net transfer capacity of the Moyle HVDC interconnector"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("reduce_moyle"),
                    h5(class = 'h4 fw-bold',2026)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("reduce_moyle"),
                      autocomplete = "off"
                    )
                  )
                )
              ),
              
              tags$script(HTML(sprintf("
                $(document).ready(function() {
                  ['%s', '%s', '%s', '%s'].forEach(function(checkboxId) {
                    $('#' + checkboxId).on('change', function() {
                      Shiny.setInputValue(checkboxId, this.checked);
                    });
                    Shiny.setInputValue(checkboxId, $('#' + checkboxId).prop('checked'));
                  });
                });
              ", ns("downward_regulation"), ns("security_standards"), 
                 ns("reduce_mustruns"), ns("reduce_moyle"))))
            )
          ),
          card(
            class = "shadow-sm border-0 rounded-3",
            card_header(
              class = "bg-white h5 fw-bolder p-3",
              "Infrastructure & Services"
            ),
            card_body(
              div(
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("phase1_lcis"),
                    "Monitor the delivery of the Phase I Low Carbon Inertia Services"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("phase1_lcis"),
                    h5(class = 'h4 fw-bold',2027)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("phase1_lcis"),
                      autocomplete = "off"
                    )
                  )
                ),
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("phase2_lcis"),
                    "Commence procurement process of Phase II Low Carbon Inertia Services"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("phase2_lcis"),
                    h5(class = 'h4 fw-bold',2029)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("phase2_lcis"),
                      autocomplete = "off"
                    )
                  )
                ),
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("ldes"),
                    "SONI to coordinate with the Utility Regulator to create a credible for a procurement mechanism to procure enhanced system flexibility through Long Duration Energy Storage"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("ldes"),
                    h5(class = 'h4 fw-bold',2030)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("ldes"),
                      autocomplete = "off"
                    )
                  )
                ),
                div(
                  class = "input-group mb-2",
                  tags$label(
                    class = "form-control",
                    `for` = ns("sn_interconnector"),
                    "Construction of the second North-South Interconnector"
                  ),
                  tags$label(
                    class = "input-group-text",
                    `for` = ns("sn_interconnector"),
                    h5(class = 'h4 fw-bold',2031)
                  ),
                  div(
                    class = "input-group-text",
                    tags$input(
                      type = "checkbox",
                      class = "form-check-input mt-0",
                      id = ns("sn_interconnector"),
                      autocomplete = "off"
                    )
                  )
                )
              ),
              tags$script(HTML(sprintf("
                $(document).ready(function() {
                  ['%s', '%s', '%s', '%s'].forEach(function(checkboxId) {
                    $('#' + checkboxId).on('change', function() {
                      Shiny.setInputValue(checkboxId, this.checked);
                    });
                    Shiny.setInputValue(checkboxId, $('#' + checkboxId).prop('checked'));
                  });
                });
              ", ns("phase1_lcis"), ns("phase2_lcis"), 
                 ns("ldes"), ns("sn_interconnector"))))
            )
          ),
        #   div(  
        #     card(
        #     class = "shadow-sm border-0 rounded-3",
        #     card_header(
        #       class = "bg-white h5 fw-bolder p-3",
        #       "Source of Renewable Simulation"
        #     ),
        #     card_body(class = 'p-3',
        #               tags$label("Select Source of Renewable Simulation:", style = "display: block; margin-bottom: 10px; font-weight: bold;"),
        #               div(
        #                 class = "btn-group ",
        #                 role = "group",
        #                 `aria-label` = "Scenario selection",
        #                 tags$input(
        #                   type = "radio",
        #                   class = "btn-check shiny-input-radiogroup shiny-input-bound",
        #                   name = ns("scenario_input_choice"),
        #                   id = ns("policy_simulation"),
        #                   value = 'policy_simulation',
        #                   autocomplete = "off"
        #                 ),
        #                 tags$label(
        #                   class = "btn btn-outline-primary opacity-75",
        #                   `for` = ns("policy_simulation"),
        #                   "REPD projection using Policy simulation"
        #                 ),
        #                 tags$input(
        #                   type = "radio",
        #                   class = "btn-check shiny-input-radiogroup",
        #                   name = ns("scenario_input_choice"),
        #                   id = ns("forecasts"),
        #                   value = 'forecasts',
        #                   checked = "checked",
        #                   autocomplete = "off"
        #                 ),
        #                 tags$label(
        #                   class = "btn btn-outline-primary opacity-75",
        #                   `for` = ns("forecasts"),
        #                   "Forecasts"
        #                 )
        #               )
        #               ),
        #               tags$script(HTML(sprintf("
        #         $(document).ready(function() {
        #           var radioName = '%s';
        #           $('input[name=\"' + radioName + '\"]').on('change', function() {
        #             if (this.checked) {
        #               Shiny.setInputValue(radioName, this.value);
        #             }
        #           });
        #           // Set initial value
        #           var checkedRadio = $('input[name=\"' + radioName + '\"]:checked');
        #           if (checkedRadio.length > 0) {
        #             Shiny.setInputValue(radioName, checkedRadio.val());
        #           }
        #         });
        #       ", ns("scenario_input_choice"))))
        #     
        #   ),
        # 
        # 
        #    uiOutput(fill = T,ns('selection_dependent'))
        #    
        #     
        #   
        # )
        div(
          card(
            class = "shadow-sm border-0 rounded-3",
            card_header(
              class = "bg-white h5 fw-bolder p-3",
              "Source of Renewable Simulation"
            ),
            card_body(
              class = 'p-3',
              tags$label(
                "Select Source of Renewable Simulation:",
                style = "display: block; margin-bottom: 10px; font-weight: bold;"
              ),
              
              div(
                class = "btn-group",
                role = "group",
                
                # Checkbox 1
                tags$input(
                  type = "checkbox",
                  class = "btn-check",
                  id = ns("policy_simulation"),
                  value = "policy_simulation",
                  autocomplete = "off"
                ),
                tags$label(
                  class = "btn btn-outline-primary opacity-75",
                  `for` = ns("policy_simulation"),
                  "REPD projection using Policy simulation"
                ),
                
                # Checkbox 2
                tags$input(
                  type = "checkbox",
                  class = "btn-check",
                  id = ns("forecasts"),
                  value = "forecasts",
                  autocomplete = "off",
                  checked = "checked"
                ),
                tags$label(
                  class = "btn btn-outline-primary opacity-75",
                  `for` = ns("forecasts"),
                  "Forecasts"
                )
              )
            ),
            
            # JS: send vector of checked boxes to Shiny
            tags$script(HTML(sprintf("
      $(document).ready(function() {
        var ids = ['%s', '%s'];

        function updateValue() {
          var selected = [];
          ids.forEach(function(id) {
            if ($('#' + id).is(':checked')) {
              selected.push($('#' + id).val());
            }
          });
          Shiny.setInputValue('%s', selected);
        }

        ids.forEach(function(id) {
          $('#' + id).on('change', updateValue);
        });

        // Initial update
        updateValue();
      });
    ", 
                                     ns("policy_simulation"),
                                     ns("forecasts"),
                                     ns("scenario_input_choice")
            )))
          ),
          
          uiOutput(fill = TRUE, ns('selection_dependent'))
        )
        ),
        
        br(), br(),
        h5("Selected Forecast Combined Series (MWh)"),
        
        br(),br(),
        DTOutput(ns("last_year_sums_table_dt")),
        br(),br(),
        # DTOutput(ns("combined_forecast_table_ci_dt")),
          div(class='d-flex ',
        div( class="container",
        div( class="row",
          
          div(class="col-6",echarts4rOutput( outputId = ns('combined_forecast_table_ci_plot_generated_available'))),
          # div(class="col-4",echarts4rOutput( outputId = ns('combined_forecast_table_ci_plot_available_renewable'))),
          div(class="col-6",echarts4rOutput( outputId = ns('combined_forecast_table_ci_plot_percentage_renewable')))
        ),
        div(class="row",
          div(class="col-6",echarts4rOutput( outputId = ns('combined_forecast_table_ci_plot_flow'))),
          div(class="col-6",echarts4rOutput( outputId = ns('combined_forecast_table_ci_plot_dispatch_down')))
          ),
        div(class="row",
            div(class="col-12",
                div(echarts4rOutput( outputId = ns('combined_forecast_table_ci_plot_so_what')))
                ))
        ),
        div(sticky_side_bar(ns))
          ),
        # sticky_side_bar(),
        
        
        # card(
        #   class = "shadow-sm border-0 rounded-3 p-5 m-5",
        #   card_body(#class='',
        #    DTOutput(ns("yearly_sums_table_dt"))
        #   )
        # ),
        
        div(
        class = "shadow-sm border-0 rounded-3 p-5 m-5",
        div(#class='',
          DTOutput(ns("yearly_sums_table_dt"))
        )
      ),
      
        div(
        class = "shadow-sm border-0 rounded-3 p-5 m-5",
        tagList(
          # Sticky header CSS and full width
          tags$head(
            tags$style(HTML("
      .header-sticky { position: sticky; top: 0; background: white; z-index: 1; }
      .reactable { width: 100% !important; }
    "))
          ),
          h3("The cumulative values by end of 2030"),
          fluidRow(
            column(6,
                   h4("Training 2014-2025"),
                   gt_output(ns("sheet1_tbl"))
            ),
            column(6,
                   h4("Training 2023-2025"),
                   gt_output(ns("sheet2_tbl"))
            )
          )
        )
        ),
      
  
      
      
        

        
        
        
    
        # card(
        #   class = "shadow-sm border-0 rounded-3",
        #   card_header(
        #     class = "bg-white",
        #     "Selected Series Combined Forecast Table (MWh)"
        #   ),
        #   card_body(
        #     tableOutput(ns("combined_forecast_table_ci")),
        #     downloadButton(ns("download_combined_forecast_ci"),
        #                   "Download Selected Series Forecast (CSV)",
        #                   class = "btn-primary mt-2")
        #   )
        # ),
        
    
      
        
#-------model-based predictions
        card(
          class = "shadow-sm border-0 rounded-3",
          card_header(
            class = "bg-white",
            tags$h4("Model-Based Predictions", class = "mb-0")
          ),
          card_body(
            h5("Forecast Plots"),
            uiOutput(ns("all_forecast_plots"))
          )
        ),
        
        card(
          class = "shadow-sm border-0 rounded-3",
          card_header(
            class = "bg-white",
            "XGBoost Forecast Table"
          ),
          card_body(
#-------table based on predicted values
            tableOutput(ns("combined_forecast_table")),
            downloadButton(ns("download_combined_forecast"), 
                          "Download XGBoost Forecast (CSV)",
                          class = "btn-primary mt-2")
          )
        ),
        
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

        # KPIs and Forecast Data Card
        card(
          class = "shadow-sm border-0 rounded-3",
          card_header(
            class = "bg-white",
            "Key Performance Indicators"
          ),
          card_body(
            tableOutput(ns("selected_date_kpis"))
          )
        )
  
      )
      )
    )
}

#server
generation_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    
    new_renewables_reactive <- reactive({
      state$renewables_run
      offshore_wind <- read.csv('offshore_wind.csv')
      forward_projects <- read.csv('forward_projects.csv')
      # forward_projects_outcome_first <- read.csv('forward_projects_outcome_first.csv')
      current_projects_projected_forward <- read.csv('./current_projects_projected_forward.csv')
      
      params = list()
      params$number_runs = max(forward_projects$run)
      
      z <- bind_rows(
        current_projects_projected_forward,
        forward_projects)|> #View()
        filter(passed_planning &
                 passed_connection &
                 passed_construction) |> 
        mutate(finished = #format(passed_construction_date, format = '%Y-%m')) |> 
                 floor_date( as.Date(passed_construction_date),'month')) |> 
        group_by(finished, 
                 tech, 
                 run) |> 
        summarise(MW = sum(`Installed.Capacity..MWelec.`,na.rm = T),
                  no_proj = n()) |> 
        ungroup() %>% 
        left_join(expand.grid(
          finished = mons,
          tech = unique(.$tech),
          run = 1:params$number_runs),.
        ) |> 
        replace_na(replace = list(MW = 0, no_proj = 0)) |> 
        arrange(finished) %>% 
        group_by(finished, 
                 tech) |> 
        summarise(MW = mean(`MW`,na.rm = T),
                  no_proj = mean(n))
      
      
      q <- offshore_wind %>% 
        mutate(finished = as.Date(finished)) %>% 
        mutate(tech = 'Wind Offshore') %>% 
        left_join(expand.grid(
          finished = mons,
          tech = unique(.$tech)),.
        ) %>% 
        replace_na(replace = list( MW = 0, no_proj = 0)) %>% 
        mutate(cumMW = mean*1000)
      
      
      
      # q <- q %>% 
      #   mutate(cumMW = ifelse(finished>'2028-04-01',500,0)) %>% 
      #   mutate(finished = as.Date(finished))
      
      z <- z %>% 
        group_by(tech) %>% 
        arrange(finished) %>% 
        mutate(cumMW = cumsum(MW))
      
      z <- z %>% 
        bind_rows(.,q) %>%
        mutate(finished = as.Date(finished)) %>% 
        arrange(tech) %>% 
        ungroup() %>% 
        mutate(tech = factor(tech, levels = c('Solar Photovoltaics', 'Wind Onshore', 'Wind Offshore'))) 

   z
      })
    
  #   output$selection_dependent <- renderUI({
  #     
  # 
  #     if(input$`scenario_input_choice`=='policy_simulation'){
  #       new_renewables_reactive() %>%   
  #         filter(finished<'2033-01-01') %>% 
  #         group_by(tech) %>% 
  #         e_charts(finished,height='200px') %>%
  #         e_grid(right='23%', bottom='0%',top ='0%') %>% 
  #         e_area(cumMW,stack = 'd', endLabel = list(show = T,color = 'lightgrey', formatter = '{a}'),
  #                lineStyle = list(color='white',opacity = 1,symbol = 'none'), 
  #                itemStyle = list(opacity = 1),symbol = 'none',legend=F) %>% 
  #         e_color(c('yellow', 'steelblue','cyan')) %>% 
  #         e_y_axis(name = 'MW') %>%
  #         e_tooltip(trigger='axis') %>% 
  #         e_theme('walden')
  #       
  #     }else
  #       {
  #   div(
  #     card(
  #       class = "shadow-sm border-0 rounded-3",
  #     card_header(
  #       class = "bg-white  h5 fw-bolder p-3",
  #       "Scenario Configuration"
  #     ),
  #     card_body(class = 'p-3',
  #   tags$label("Select Scenario for renewables:", style = "display: block; margin-bottom: 10px; font-weight: bold;"),
  #   div(
  #     class = "btn-group ",
  #     role = "group",
  #     `aria-label` = "Scenario selection",
  #     tags$input(
  #       type = "radio",
  #       class = "btn-check shiny-input-radiogroup",
  #       name = session$ns("scenario_choice"),
  #       id = session$ns("scenario_worst"),
  #       value = "worst",
  #       autocomplete = "off"
  #     ),
  #     tags$label(
  #       class = "btn btn-outline-danger opacity-75",
  #       `for` = session$ns("scenario_worst"),
  #       "Pessimistic case"
  #     ),
  #     tags$input(
  #       type = "radio",
  #       class = "btn-check shiny-input-radiogroup",
  #       name = session$ns("scenario_choice"),
  #       id = session$ns("scenario_medium"),
  #       value = "medium",
  #       checked = "checked",
  #       autocomplete = "off"
  #     ),
  #     tags$label(
  #       class = "btn btn-outline-warning opacity-75",
  #       `for` = session$ns("scenario_medium"),
  #       "Medium case"
  #     ),
  #     
  #     tags$input(
  #       type = "radio",
  #       class = "btn-check shiny-input-radiogroup",
  #       name = session$ns("scenario_choice"),
  #       id = session$ns("scenario_best"),
  #       value = "best",
  #       autocomplete = "off"
  #     ),
  #     tags$label(
  #       class = "btn btn-outline-success opacity-75",
  #       `for` = session$ns("scenario_best"),
  #       "Optimum case"
  #     )
  #   ),
  #   tags$script(HTML(sprintf("
  #               $(document).ready(function() {
  #                 var radioName = '%s';
  #                 $('input[name=\"' + radioName + '\"]').on('change', function() {
  #                   if (this.checked) {
  #                     Shiny.setInputValue(radioName, this.value);
  #                   }
  #                 });
  #                 // Set initial value
  #                 var checkedRadio = $('input[name=\"' + radioName + '\"]:checked');
  #                 if (checkedRadio.length > 0) {
  #                   Shiny.setInputValue(radioName, checkedRadio.val());
  #                 }
  #               });
  #             ", session$ns("scenario_choice"))))
  #   )))
  #     }
  # })
    output$selection_dependent <- renderUI({
      
      choices <- input$scenario_input_choice
      section1_plot <- new_renewables_reactive() %>%
        filter(finished < '2033-01-01') %>%
        group_by(tech) %>%
        e_charts(finished, height='200px') %>%
        e_grid(right='23%', bottom='0%', top='0%') %>%
        e_area(
          cumMW, stack='d',
          endLabel = list(show = TRUE, color='lightgrey', formatter='{a}'),
          lineStyle = list(color='white', opacity=1, symbol='none'),
          itemStyle = list(opacity=1),
          symbol='none', legend=FALSE
        ) %>%
        e_color(c('yellow','steelblue','cyan')) %>%
        e_y_axis(name='MW') %>%
        e_tooltip(trigger='axis') %>%
        e_theme('walden')
      
      
      section2_ui <- div(
        card(
          class = "shadow-sm border-0 rounded-3",
          card_header(
            class = "bg-white h5 fw-bolder p-3",
            "Scenario Configuration"
          ),
          card_body(
            class = 'p-3',
            tags$label("Select Scenario for renewables:",
                       style="display:block;margin-bottom:10px;font-weight:bold;"),
            
            div(
              class = "btn-group",
              role = "group",
              `aria-label` = "Scenario selection",
              
              tags$input(
                type = "radio",
                class = "btn-check shiny-input-radiogroup",
                name = session$ns("scenario_choice"),
                id = session$ns("scenario_worst"),
                value = "worst"
              ),
              tags$label(
                class = "btn btn-outline-danger opacity-75",
                `for` = session$ns("scenario_worst"),
                "Pessimistic case"
              ),
              
              tags$input(
                type = "radio",
                class = "btn-check shiny-input-radiogroup",
                name = session$ns("scenario_choice"),
                id = session$ns("scenario_medium"),
                value = "medium",
                checked = "checked"
              ),
              tags$label(
                class = "btn btn-outline-warning opacity-75",
                `for` = session$ns("scenario_medium"),
                "Medium case"
              ),
              
              tags$input(
                type = "radio",
                class = "btn-check shiny-input-radiogroup",
                name = session$ns("scenario_choice"),
                id = session$ns("scenario_best"),
                value = "best"
              ),
              tags$label(
                class = "btn btn-outline-success opacity-75",
                `for` = session$ns("scenario_best"),
                "Optimum case"
              )
            ),
            
            tags$script(HTML(sprintf("
                $(document).ready(function() {
                  var radioName = '%s';
                  $('input[name=\"' + radioName + '\"]').on('change', function() {
                    if (this.checked) {
                      Shiny.setInputValue(radioName, this.value);
                    }
                  });
                  var checkedRadio = $('input[name=\"' + radioName + '\"]:checked');
                  if (checkedRadio.length > 0) {
                    Shiny.setInputValue(radioName, checkedRadio.val());
                  }
                });
              ", session$ns("scenario_choice"))))
          )
        )
      )
      
      
      
      # Only policy simulation
      if ("policy_simulation" %in% choices && !("forecasts" %in% choices)) {
        return(section1_plot)
      }
      
      # Only forecasts
      if ("forecasts" %in% choices && !("policy_simulation" %in% choices)) {
        return(section2_ui)
      }
      
      
      if (all(c("policy_simulation", "forecasts") %in% choices)) {
        
        return(
          tagList(
            h4("Policy Simulation and Forecasts", class="fw-bold mb-3"),
            
            # Section 1 output
            div(
              class="mb-4",
              h5("Policy Simulation"),
              section1_plot
            ),
            
            # Section 2 output
            div(
              class="mt-4",
              h5("Forecast Configuration"),
              section2_ui
            )
          )
        )
      }
      
    })
    

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
      file <- "./data/results_xgb_all.rda"
      env  <- new.env()
      load(file, envir = env)
      env$results_full
    })
    
    results_last3 <- local({
      file <- "./data/results_xgb_all.rda"
      env  <- new.env()
      load(file, envir = env)
      env$results_last3
    })
    
    output$all_forecast_plots <- renderUI({
      vars_full  <- if (!is.null(results)) names(results) else character(0)
      vars_last3 <- if (!is.null(results_last3)) names(results_last3) else character(0)
      varnames   <- union(vars_full, vars_last3)
      
      plot_list <- lapply(varnames, function(varname) {
        
        has_full  <- !is.null(results)       && !is.null(results[[varname]])
        has_last3 <- !is.null(results_last3) && !is.null(results_last3[[varname]])
        
        tagList(
          h3(nice_names[[varname]] %||% varname, style = "margin-top:40px; color:#355070;"),
          
          div(
            style = "display:flex; justify-content:space-between; font-weight:bold; margin-bottom:10px;",
            div("Full Training (2014–2025)",  style="width:48%; text-align:center;"),
            div("Training 2023–2025",         style="width:48%; text-align:center;")
          ),
          
          div(
            style = "
        display:flex;
        gap:20px;
        border-bottom:1px solid #ccc;
        padding-bottom:20px;
        margin-bottom:30px;
      ",
            
            div(
              style = "width:48%;",
              if (has_full) {
                plotOutput(session$ns(paste0("plot_xgb_full_", varname)), height = '350px')
              }
            ),
            
            div(
              style = "width:48%; border-left:1px solid #ddd; padding-left:20px;",
              if (has_last3) {
                plotOutput(session$ns(paste0("plot_xgb_last3_", varname)), height = '350px')
              }
            )
          )
        )
      })
      
      
      for (v in vars_full) {
        local({
          vn <- v
          output[[paste0("plot_xgb_full_", vn)]] <- renderPlot({
            req(results[[vn]])
            plot_fc(results[[vn]], vn)
          })
        })
      }
      
      for (v in vars_last3) {
        local({
          vn <- v
          output[[paste0("plot_xgb_last3_", vn)]] <- renderPlot({
            req(results_last3[[vn]])
            plot_fc_last3(results_last3[[vn]], vn)
          })
        })
      }
      
      do.call(tagList, plot_list)
    })

#---table based on predicted values
    combined_forecast <- reactive({
      tables <- lapply(names(results_last3), function(varname) {
        df <- results_last3[[varname]]$forecast

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
          Target = pmin(Target, 1),
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
      Target = pmin(target, 1)

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
        "Wind"  = "#3d405b",  
        "Solar" = "#e07a5f"  
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
      req(results_last3)
      vars <- names(results_last3)                     
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
      req(results_last3)  
      vars <- names(results_last3)
      
      for (v in vars) {
        local({
          vn <- v 
          observeEvent(input[[paste0("plot_click_", vn, "_click")]], {
            click <- input[[paste0("plot_click_", vn, "_click")]]
            if (!is.null(click$x)) {
              df_dates <- results_last3[[vn]]$forecast$date
              # df_dates <- as.Date(df_dates, origin = if (is.numeric(df_dates)) "1970-01-01" else NULL)
              selected_date2(snap_to_nearest_date(click$x, df_dates))
            }
          }, ignoreInit = TRUE)
          
          output[[paste0("plot_click_", vn)]] <- renderPlot({
            p <- plot_fc(results_last3[[vn]], vn)  
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
      req(results_last3)
      
      vars <- names(results_last3)
      choices <- setNames(
        lapply(vars, function(v) input[[paste0("ci_choice2_", v)]] %||% "pred"),
        vars
      )
      
      tables <- lapply(vars, function(varname) {
        df <- results_last3[[varname]]$forecast
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
          fc  <- results_last3$sum_solar$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_solar <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_solar = dplyr::coalesce(lo95_solar, sum_solar))
        }
        
        {
          fc  <- results_last3$sum_wind$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_wind <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_wind = dplyr::coalesce(lo95_wind, sum_wind))
        }
        
        {
          fc  <- results_last3$sum_demand$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "pred")
          med_demand <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_demand = dplyr::coalesce(med_demand, sum_demand))
        }
        
        {
          fc  <- results_last3$sum_avai_solar$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_avai_solar <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_avai_solar = dplyr::coalesce(lo95_avai_solar, sum_avai_solar))
        }
        
        {
          fc  <- results_last3$sum_avai_wind$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "lo95")
          lo95_avai_wind <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_avai_wind = dplyr::coalesce(lo95_avai_wind, sum_avai_wind))
        }
      }
      
      if (scenario == "best") {
        {
          fc  <- results_last3$sum_solar$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95")
          hi95_solar <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_solar = dplyr::coalesce(hi95_solar, sum_solar))
        }
        
        {
          fc  <- results_last3$sum_wind$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95")
          hi95_wind <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_wind = dplyr::coalesce(hi95_wind, sum_wind))
        }
        
        {
          fc  <- results_last3$sum_demand$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "pred")
          med_demand <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_demand = dplyr::coalesce(med_demand, sum_demand))
        }
        
        {
          fc  <- results_last3$sum_avai_solar$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95")
          hi95_avai_solar <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_avai_solar = dplyr::coalesce(hi95_avai_solar, sum_avai_solar))
        }
        
        {
          fc  <- results_last3$sum_avai_wind$forecast
          fcd <- as.Date(fc$date, origin = "1970-01-01")
          col <- pick_fc_col(fc, "hi95")
          hi95_avai_wind <- fc[[col]][ match(as.Date(df_all$date), fcd) ]
          df_all <- df_all %>%
            mutate(sum_avai_wind = dplyr::coalesce(hi95_avai_wind, sum_avai_wind))
        }
      }
      
      #apply policies
      df_all <- df_all %>%
        mutate(`Generated RES` = sum_wind + sum_solar,
               `Available RES` = sum_avai_wind + sum_avai_solar)
      
     
      if (isTRUE(input$downward_regulation)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + (50 * 24 * 30),
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$security_standards)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + (75 * 24 * 30),
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$reduce_mustruns)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
                                     `Generated RES` + (60 * 24 * 30),
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$phase1_lcis)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2027-02-01"),
                                     `Generated RES` + (100 * 24 * 30),
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$phase2_lcis)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2029-01-01"),
                                     `Generated RES` + (100 * 24 * 30),
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$reduce_moyle)) {
        df_all <- df_all %>%
          mutate(
            sum_import = ifelse(date >= as.Date("2026-01-01"),
                                sum_import - (450 * 24 * 30),
                                sum_import),
            sum_export = ifelse(date >= as.Date("2026-01-01"),
                                sum_export + (400 * 24 * 30),
                                sum_export),
            # `Generated RES` = ifelse(date >= as.Date("2026-01-01"),
            #                          `Generated RES` + (850 * 24 * 30),
            #                          `Generated RES`)
          ) %>%
          mutate(sum_import = if_else(is.na(sum_import), NA_real_, pmax(sum_import, 0)))
      }
      if (isTRUE(input$ldes)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2030-10-01"),
                                     `Generated RES` + (500 * 24 * 30),
                                     `Generated RES`)
          )
      }
      if (isTRUE(input$sn_interconnector)) {
        df_all <- df_all %>%
          mutate(
            `Generated RES` = ifelse(date >= as.Date("2031-01-01"),
                                     `Generated RES` + (900 * 24 * 30),
                                     `Generated RES`)
          )
      }
      # if ("policy_simulation" %in% input$scenario_input_choice) {
      #   
      #   print(names(df_all))
      #   print(names(new_renewables_reactive()))
      #   
      #   new_renewables_wider <- pivot_wider(new_renewables_reactive(),
      #                                       id_cols = finished, 
      #                                      names_from = tech, 
      #                                      values_from = cumMW)
      #   
      #   start_dates <- new_renewables_wider %>%
      #     summarise(
      #       wind_onshore_start = min(finished[`Wind Onshore` > 0], na.rm = TRUE),
      #       wind_offshore_start = min(finished[`Wind Offshore` > 0], na.rm = TRUE),
      #       solar_start         = min(finished[`Solar Photovoltaics` > 0], na.rm = TRUE)
      #     )
      #   
      #   
      #   df_all <- df_all %>%
      #     left_join(new_renewables_wider,by = c('date'='finished')) %>% 
      #     mutate(
      #       `Available RES` = `Available RES` +  `Wind Onshore` + `Wind Offshore` + `Solar Photovoltaics`
      #     )
      # }
      
      
      if ("policy_simulation" %in% input$scenario_input_choice) {
        
        # 1) Start from project-level data (NOT floored to month)
        current_projects_projected_forward <- read.csv('current_projects_projected_forward.csv')
        offshore_wind <- read.csv('offshore_wind.csv')
        forward_projects <- read.csv('forward_projects.csv')
        projects <- bind_rows(
          current_projects_projected_forward,
          forward_projects
        ) %>%
          filter(passed_planning, passed_connection, passed_construction) %>%
          transmute(
            tech,
            start = as.Date(passed_construction_date),
            MW    = `Installed.Capacity..MWelec.`
          ) %>%
          mutate(tech = recode(tech,
                               "Wind Offshore" = "Wind Offshore",
                               "Wind Onshore"  = "Wind Onshore",
                               "Solar Photovoltaics" = "Solar Photovoltaics"))
        
        end_date <- as.Date("2030-12-31")
        new_renewables_wider <- pivot_wider(new_renewables_reactive(),
                                            id_cols = finished,
                                            names_from = tech,
                                            values_from = cumMW)
        # Capacity factors
        
        
        monthly_hours <- function(date) {
          lubridate::days_in_month(date) * 24
        }
        
        policy_mwh <- df_all %>%
          select(date) %>%
          left_join(new_renewables_wider, by = c("date" = "finished")) %>%
          mutate(
            across(c(`Wind Onshore`, `Wind Offshore`, `Solar Photovoltaics`),
                   ~replace_na(.x, 0))
          ) %>%
          mutate(
            hours = monthly_hours(date),
            mwh_onshore  = `Wind Onshore`        * hours ,
            mwh_offshore = `Wind Offshore`       * hours ,
            mwh_solar    = `Solar Photovoltaics` * hours ,
            RES_MWh_policy = mwh_onshore + mwh_offshore + mwh_solar
          )
        
        
        
        df_all <- df_all %>%
          left_join(policy_mwh, by = "date") %>%
          mutate(across(c(mwh_onshore, mwh_offshore, mwh_solar, RES_MWh_policy),
                        ~replace_na(.x, 0))) %>%
          mutate(
            `Available RES` = `Available RES` + RES_MWh_policy
          )
      }
      
      
      df_all <- df_all %>%
        mutate(
          `Generated RES` = pmin(`Generated RES`, `Available RES`, na.rm = FALSE),
          SNSP   = (`Generated RES`  + sum_import) / (sum_demand + sum_export),
          
          `Planned SNSP` = case_when(
            lubridate::year(date) >= 2025 & lubridate::year(date) <= 2027 ~ 0.80,
            lubridate::year(date) >= 2028 & lubridate::year(date) <= 2029 ~ 0.85,
            lubridate::year(date) == 2030 ~ 0.90,
            TRUE ~ NA_real_
          ),
          
          # `Generated RES` = sum_res,
          # `Dispatch Down Wind`  = sum_avai_wind  - sum_wind,
          # `Dispatch Down Solar` = sum_avai_solar - sum_solar,
          # .dd_total = `Dispatch Down Wind` + `Dispatch Down Solar`,
          
          # Curtailment = if_else(
          #   .dd_total > 0,
          #   (0.1667 * `Dispatch Down Wind` + 0.1461 * `Dispatch Down Solar`) / .dd_total,
          #   NA_real_
          # ),
          # Constraint = if_else(
          #   .dd_total > 0,
          #   (0.8318 * `Dispatch Down Wind` + 0.8542 * `Dispatch Down Solar`) / .dd_total,
          #   NA_real_
          # ),
          `Dispatch Down` = `Available RES` - `Generated RES`,
          # `Dispatch Down` = pmax(`Dispatch Down`, 0),
          
          
          `Curtailment SNSP` = ifelse(SNSP > `Planned SNSP`, "Curtailment SNSP", "")
        ) %>%
        mutate(
          possible_res = `Planned SNSP` * (sum_demand + sum_export) - sum_import,
          `Dispatch Down` = if_else(
            `Curtailment SNSP` == "Curtailment SNSP",
            `Dispatch Down` + `Generated RES` - possible_res,
            `Dispatch Down`
          ),
          `Generated RES` = if_else(
            `Curtailment SNSP` == "Curtailment SNSP",
            pmin(possible_res, `Generated RES`),
            `Generated RES`
          ),
          Curtailment = 0.17 * `Dispatch Down`,
          Constraint = 0.83 * `Dispatch Down`,
          
        ) %>%
        mutate( 
          Target = `Generated RES` / sum_demand,
          Target = pmin(Target, 1)
        ) %>%
        mutate(SNSP = pmin(SNSP, `Planned SNSP`, na.rm = FALSE),) %>%
        select(- possible_res) %>%
        rename_with(~ nice_names[.x], .cols = names(nice_names))
      
      df_all <- df_all %>%
        mutate(date = format(date, "%Y-%m-%d"))
    })
    
    
    output$combined_forecast_table_ci_plot_flow <- renderEcharts4r({
    dt <- combined_forecast_ci()
    print(names(dt))
    
    dt %>% 
      mutate(exports = -1* Exports) %>% 
      e_charts(date) %>% 
      e_bar(Imports,color='cornflowerblue', stack = 'f',emphasis = list(focus= 'series')) %>% 
      e_line(Imports,color='cornflowerblue') %>% 
      e_bar(exports,color='red', stack = 'f',emphasis = list(focus= 'series')) %>% 
      e_line(exports,color='red') %>% 
      e_tooltip(formatter = e_tooltip_item_formatter("decimal")) %>%      
      e_theme('walden') %>% 
      e_title('Imports and Exports')
    })
    
    
    output$combined_forecast_table_ci_plot_generated_available <- renderEcharts4r({
      dt <- combined_forecast_ci()
      
      dt %>% 
        mutate(exports = -1* Exports) %>% 
        e_charts(date) %>% 
        e_area(`Generated RES`, stack = 'f', color='cornflowerblue', emphasis = list(focus= 'series')) %>% 
        e_line(`Available RES`, color='grey',emphasis = list(focus= 'series')) %>% 
        e_tooltip(formatter = e_tooltip_item_formatter("decimal")) %>% 
        e_y_axis(name = 'MWh') %>%
        e_theme('walden') %>% 
        e_title('Available vs Generation')
    })
    
    
    # output$combined_forecast_table_ci_plot_generated_renewable <- renderEcharts4r({
    #   dt <- combined_forecast_ci()
    #   
    #   dt %>% 
    #     mutate(exports = -1* Exports) %>% 
    #     e_charts(date) %>% 
    #     e_area(`Generated Solar`, stack = 'f', color='cornflowerblue', emphasis = list(focus= 'series')) %>% 
    #     e_area(`Generated Wind`, stack = 'f', color='#FFDE21',emphasis = list(focus= 'series')) %>% 
    #     e_line(`Generated RES`, color='grey',emphasis = list(focus= 'series')) %>% 
    #     e_tooltip(formatter = e_tooltip_item_formatter("decimal")) %>% 
    #     e_y_axis(name = 'MWh') %>%
    #     e_theme('walden') %>% 
    #     e_title('Generation')
    # })
    # 
    # output$combined_forecast_table_ci_plot_available_renewable <- renderEcharts4r({
    #   dt <- combined_forecast_ci()
    #   
    #   dt %>% 
    #     mutate(exports = -1* Exports) %>% 
    #     e_charts(date) %>% 
    #     e_area(`Available Solar`, stack = 'f', color='cornflowerblue',emphasis = list(focus= 'series')) %>% 
    #     e_area(`Available Wind`, stack = 'f', color='#FFDE21',emphasis = list(focus= 'series')) %>% 
    #     e_tooltip(formatter = e_tooltip_item_formatter("decimal")) %>% 
    #     e_y_axis(label = 'MWh') %>%
    #     e_theme('walden') %>% 
    #     e_title('Available')
    # })
    
    output$combined_forecast_table_ci_plot_percentage_renewable <- renderEcharts4r({
      dt <- combined_forecast_ci()
      # data.frame(x=1,y=1) %>%
      #   e_charts(x) %>%
      #   e_scatter(y)
      dt %>%
        mutate(
          perc_res = `Generated RES` / `Available RES`*100) %>%
        e_charts(date) %>%
        e_line(perc_res,  color='#FFDE21', name = 'Availability of Renewable', emphasis = list(focus= 'series')) %>%  # #FFDE21
        # e_line(perc_wind,  color='cornflowerblue',name = 'Availability of Wind', emphasis = list(focus= 'series')) %>%
        e_format_y_axis(suffix='%') %>%
        e_mark_line(data = list(name= 'Theoretical Max',
                                yAxis= 100),
                    lineStyle = list(type='solid', color='grey')) %>% 
        e_tooltip(formatter = e_tooltip_item_formatter("decimal")) %>% 
        e_x_axis(label = 'Time', type='time') %>%
        e_theme('walden') %>% 
        e_title('Percentage of Available RES Generated')
        
    })
    
    output$combined_forecast_table_ci_plot_dispatch_down <- renderEcharts4r({
      dt <- combined_forecast_ci()
      # data.frame(x=1,y=1) %>% 
      #   e_charts(x) %>% 
      #   e_scatter(y)
      
      dt %>%
      e_charts(date) %>%
        
        e_bar(Curtailment,
              stack = "dd",
              name = "Curtailment",
              color = "firebrick",
              emphasis = list(focus = 'series')
        ) %>%
        e_bar(Constraint,
              stack = "dd",
              name = "Constraint",
              color = "salmon",
              emphasis = list(focus = 'series')
        ) %>%
        
      # e_bar(`Dispatch Down`,color='salmon', stack = 'f', name = 'Dispatch Down',emphasis = list(focus= 'series')) %>%
      # e_line(Dispatch.Down.Wind,color='salmon', stack = 'h') %>%
      # e_bar(`Dispatch Down Solar`,color='firebrick', stack = 'f', name = 'Dispatch Down Solar',emphasis = list(focus= 'series')) %>%
      # e_line(Dispatch.Down.Solar,color='lightcoral', stack = 'h') %>%
      e_x_axis(name = 'Time', type='time') %>%
      # e_loess(formula = Dispatch.Down.Solar~date,color='black') %>%
        e_tooltip(formatter = e_tooltip_item_formatter("decimal")) %>% 
        e_theme('walden') %>% 
        e_title('Dispatch Down')
    
    })
    
    output$combined_forecast_table_ci_plot_so_what <- renderEcharts4r({
      dt <- combined_forecast_ci()
      
      dt %>% 

        e_charts(date) %>%
        e_line(Target,endLabel = list(show = F),emphasis = list(focus='series')) %>% # formatter = '{a}'  # #FFDE21
        e_line(SNSP,itemStyle = list(opacity=0),emphasis = list(focus='series')) %>% 
        e_line(`Planned SNSP`, lineStyle = list(type='dashed'), itemStyle = list(opacity=0),emphasis = list(focus='series')) %>%  # #FFDE21
        # e_line(Curtailment, itemStyle = list(opacity=0),emphasis = list(focus='series')) %>% 
        # e_line(Constraint, itemStyle = list(opacity=0),emphasis = list(focus='series')) %>%
        # e_line(`Available RES`, itemStyle = list(opacity=0),emphasis = list(focus='series')) %>% 
        # e_line(`Generated RES`, itemStyle = list(opacity=0),emphasis = list(focus='series')) %>% 
        e_x_axis(type = 'time' ,max = "2030-12-01") %>%
   
        e_color(c('black','red', 'lightcoral',  'royalblue', 'steelblue', '#b3d89c','#9dc3c2')) %>%
        e_annotations(default_color = 'grey',
                      legend = F,
                      name = 'Target',
                      list(list(
                        lineStyle = "none",
                        # rectStyle = "none",
                        arrowStyle = "none",
                        x = '2030-02-01',
                        y = last(dt$Target),
                        text = "RES/Demand<br>Target",
                        offsetX = 80,
                        offsetY = 0
                      )) ) %>%
        e_grid(right='10%') %>%
        e_tooltip() %>% 
        e_theme('walden') %>% 
        e_y_axis( formatter = e_axis_formatter(style = 'percent')) %>% 
        e_title('RES Target with SNSP, DD, Curatilment and Constraint Trajectory', 'Dashed line = Planned SNSP')
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
    
    # output$selected_date_kpis <- renderTable({
    #   df <- combined_forecast_ci()
    #   sd <- selected_date2()
    # 
    #   if (is.null(sd)) {
    #     return(data.frame(
    #       Metric = c("Selected date"),
    #       Value  = c("—"),
    #       check.names = FALSE, stringsAsFactors = FALSE
    #     ))
    #   }
    # 
    #   df$date <- as.Date(df$date)
    #   row <- df[df$date == sd, , drop = FALSE]
    # 
    #   if (nrow(row) == 0) {
    #     return(data.frame(
    #       Metric = c("Selected date"),
    #       Value  = c("No data for selected date"),
    #       check.names = FALSE, stringsAsFactors = FALSE
    #     ))
    #   }
    # 
    #   target          <- row$Target
    #   `Dispatch Down` <- row$`Dispatch Down Wind` + row$`Dispatch Down Solar`
    # 
    #   data.frame(
    #     Metric = c("Target", "Dispatch Down"),
    #     Value = c(
    #       format(target,    big.mark = ",", scientific = FALSE),
    #       format(round(`Dispatch Down`),    big.mark = ",", scientific = FALSE)
    #     ),
    #     check.names = FALSE, stringsAsFactors = FALSE
    #   )
    # }, digits = 3, rownames = FALSE)
    
    output$selected_date_kpis <- renderTable({
      df <- combined_forecast_ci()

      # 1) Make sure we have data
      validate(need(nrow(df) > 0, "Forecast data not available yet."))

      # 2) Ensure 'date' is Date (combined_forecast_ci() ends with character date "YYYY-MM-DD")
      df$date <- as.Date(df$date)

      # 3) Ensure the clicked selection is also Date
      sd <- selected_date2()
      if (is.null(sd)) {
        return(data.frame(
          Metric = c("Selected date"),
          Value  = c("—"),
          check.names = FALSE, stringsAsFactors = FALSE
        ))
      }
      sd <- as.Date(sd)   # <---- important

      # 4) Pull the row for the selected date
      row <- df[df$date == sd, , drop = FALSE]
      if (nrow(row) == 0) {
        return(data.frame(
          Metric = c("Selected date"),
          Value  = c("No data for selected date"),
          check.names = FALSE, stringsAsFactors = FALSE
        ))
      }

      nm <- names(row)

      # 5) Target: use the renamed column if present; otherwise recompute
      target_val <- if ("Target" %in% nm) {
        row[["Target"]]
      } else if (all(c("Generated RES","Demand") %in% nm)) {
        suppressWarnings(row[["Generated RES"]] / row[["Demand"]])
      } else {
        NA_real_
      }

      # 6) Dispatch Down: prefer the aggregate (policy/SNSP‑aware) if present
      dd_val <- NULL
      if ("Dispatch Down" %in% nm) {
        dd_val <- row[["Dispatch Down"]]
      } else {
        # fallback to per-tech sum if they exist under the *post‑rename* names
        # try the raw names first
        wind_nm  <- intersect(nm, c("Dispatch Down Wind"))
        solar_nm <- intersect(nm, c("Dispatch Down Solar"))

        # if nice_names renamed them, try to map via nice_names
        if (length(wind_nm) == 0 && exists("nice_names")) {
          mapped_wind  <- unname(nice_names[["Dispatch Down Wind"]])
          mapped_solar <- unname(nice_names[["Dispatch Down Solar"]])
          wind_nm  <- intersect(nm, mapped_wind)
          solar_nm <- intersect(nm, mapped_solar)
        }

        wind_dd  <- if (length(wind_nm))  row[[wind_nm[1]]]  else 0
        solar_dd <- if (length(solar_nm)) row[[solar_nm[1]]] else 0
        dd_val   <- wind_dd + solar_dd
      }

      data.frame(
        Metric = c("Selected date", "Target (%)", "Dispatch Down (MWh)"),
        Value = c(
          format(sd, "%Y-%m-%d"),
          if (is.na(target_val)) "—" else sprintf("%.1f", 100 * target_val),
          format(round(dd_val), big.mark = ",", scientific = FALSE)
        ),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }, digits = 3, rownames = FALSE)
    
    
    output$last_year_sums_table_dt <- renderDT({
      print('combined_forecast_ci()')
      print(combined_forecast_ci())
      print('----------------------')
      
      df <- combined_forecast_ci()
      
      df$date <- as.Date(df$date)
      
      df$year <- format(df$date, "%Y")
      
      num_cols <- sapply(df, is.numeric)
      
      yearly_df <- df %>%
        filter(year ==2030) %>% 
        group_by(year) %>%
        summarise(across(which(num_cols), sum, na.rm = TRUE)) %>%
        
        
        mutate(
          Target = `Generated RES` / `Demand`,
          Target = pmin(Target, 1),
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
        mutate(
          # .dd_total = `Dispatch Down Wind` + `Dispatch Down Solar`,
          #      
          #      Curtailment = if_else(
          #        .dd_total > 0,
          #        (0.1667 * `Dispatch Down Wind` + 0.1461 * `Dispatch Down Solar`) / .dd_total,
          #        NA_real_
          #      ),
          #      Constraint = if_else(
          #        .dd_total > 0,
          #        (0.8318 * `Dispatch Down Wind` + 0.8542 * `Dispatch Down Solar`) / .dd_total,
          #        NA_real_
          #      ),
          
               
               `Curtailment SNSP` = ifelse(SNSP > `Planned SNSP`, "Curtailment SNSP", ""))%>%
        mutate(
          possible_res = `Planned SNSP` * (Demand + Exports) - Imports,
          `Dispatch Down` = if_else(
            `Curtailment SNSP` == "Curtailment SNSP",
            `Generated RES` - possible_res,
            `Dispatch Down`
          ),
          `Generated RES` = if_else(
            `Curtailment SNSP` == "Curtailment SNSP",
            possible_res,
            `Generated RES`
          ),
          Curtailment = 0.17 * `Dispatch Down`,
          Constraint = 0.83 * `Dispatch Down`,
          
        ) %>%
        mutate(
               `Cumulative Demand (MWh)` = cumsum(`Demand`),
               `Cumulative RES (MWh)`    = cumsum(`Generated RES`),
               `Cumulative Dispatch Down (MWh)` = cumsum(`Dispatch Down`)
               ) %>%
        mutate(SNSP = pmin(SNSP, `Planned SNSP`, na.rm = FALSE))
      
      
      
      # page_fluid(
      #   icon('arrow-right',class= 'visually-hidden'),
      
      # yearly_df <- yearly_df %>% 
      #   mutate(across(-c('Curtailment SNSP','year'),
      #                 ~as.numeric(round(.x))))
      
      yearly_df %>% 
        rowwise( ) %>% 
        mutate(.before = 2,Flow=as.character(tagList(div(style = 'min-width:100px;',class= 'm-1 p-1',
                                                         div(div(class='lead',icon('arrow-left'),
                                                                 f(Imports)	),
                                                             p(class='text-muted text-bg-info p-1 m-1 rounded-2','Imports')),
                                                         div(class = 'float-right',
                                                             span(class='text-muted',f(Exports)	,icon('arrow-right')),
                                                             p(class='text-bg-danger p-1 m-1 rounded-2','Exports')
                                                         ),
        )))) %>% 
        mutate(.after = 2, Renewables=as.character(tagList(div(
          h4(class='text-bg-success p-1 m-1 rounded-2',icon('leaf'),f(`Generated RES`)),
          div(class = 'text-center',
              div(f(`Generated Solar`),'/',f(`Available Solar`), icon(class='text-yellow','sun')),
              div(f(`Generated Wind`),'/',f(`Available Wind`), icon(class='text-blue','fan'))
          ))))) %>% 
        mutate(
          .after = 3,Demand=as.character(tagList(div( class= 'm-1 p-1', 
                                                      icon(class = 'fw-bold','bolt'),icon(class = 'fw-bold','bolt'),icon(class = 'fw-bold','bolt'),
                                                      icon(class= 'float-end','industry'),icon(class= 'float-end','house'),
                                                      p(class='text-bg-dark p-1 rounded-2 m-1' ,' MWh/year ' ,f(Demand), 
                                                        icon(class= 'text-centre','industry'),
                                                        icon(class= '','house')
                                                      )
          )))) %>% 
        mutate(.after = 4,year= as.character(h1(year))) %>% 
        mutate(Target = as.character(div(style = 'width:105px;',circular_value(f(Target*100)) ) ))%>% 
        # mutate(.after = 5,Target = as.character(h1(Target)) )%>% 
        mutate(gap='') %>%
        mutate(SNSP = as.character(div(class= 'm-1 p-1',tags$ul(tags$li(class='text-muted','Actual / Planned SNSP'),
                                                                tags$li(paste(format(SNSP,digits=2) ,'/', format(`Planned SNSP`,digits=2) )),
                                                                tags$li( class='text-danger',paste('\u394',format(`Planned SNSP`-SNSP,digits=2) ))
        )))) %>%
        mutate(`Dispatch Down` = as.character(div(class= 'm-1 p-1',
                                                  tags$ul(
                                                    tags$li(paste('Constraint:',format(Constraint,digits=2) )),
                                                    tags$li(paste('Curtailment:',format(Curtailment,digits=2) ))),
                                                  
                                                  div(style = 'border-style: double', 
                                                      class=' rounded-3 border-4 text-muted text-center','Dispatch Down',
                                                      h5(class = 'fw-bold','MWh/year',f(`Dispatch Down`)))
                                                  
        ))) %>%
        select(Target, year, Flow, Renewables, SNSP,Demand,  `Dispatch Down` ) %>%  #View()
        DT::datatable(height = '100vh',
                      escape = F, 
                      
                      rownames=F,
                      selection='none',
                      options = list(dom = '',ordering = FALSE,pageLength = 10, scrollX = TRUE))})
    
    
    
    last_year_target <- reactive({
      df <- combined_forecast_ci()
      df$date <- as.Date(df$date)
      df$year <- format(df$date, "%Y")
      
      num_cols <- sapply(df, is.numeric)
      
      yearly_df <- df %>%
        filter(year == 2030) %>%
        group_by(year) %>%
        summarise(across(which(num_cols), sum, na.rm = TRUE))
    
      target <- yearly_df$`Generated RES` / yearly_df$Demand
      target <- min(target, 1)
      
      return(target)  
    })
    
    output$last_year_target_pct <- renderText({
      round(last_year_target() * 100)  
    })
    
    
    
    
    output$yearly_sums_table_dt <- renderDT({
      print('combined_forecast_ci()')
      print(combined_forecast_ci())
      print('----------------------')
      
      df <- combined_forecast_ci()
      
      df$date <- as.Date(df$date)
      
      df$year <- format(df$date, "%Y")
      df <- df %>% dplyr::filter(year >= 2026)
      
      num_cols <- sapply(df, is.numeric)
      
      yearly_df <- df %>%
        group_by(year) %>%
        summarise(across(which(num_cols), sum, na.rm = TRUE)) %>%
        
        mutate(
          Target = `Generated RES` / `Demand`,
          Target = pmin(Target, 1),
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
        mutate(
          # .dd_total = `Dispatch Down Wind` + `Dispatch Down Solar`,
          #      
          #      Curtailment = if_else(
          #        .dd_total > 0,
          #        (0.1667 * `Dispatch Down Wind` + 0.1461 * `Dispatch Down Solar`) / .dd_total,
          #        NA_real_
          #      ),
          #      Constraint = if_else(
          #        .dd_total > 0,
          #        (0.8318 * `Dispatch Down Wind` + 0.8542 * `Dispatch Down Solar`) / .dd_total,
          #        NA_real_
          #      ),
               
        `Curtailment SNSP` = ifelse(SNSP > `Planned SNSP`, "Curtailment SNSP", "")) %>%
        mutate(
          possible_res = `Planned SNSP` * (Demand + Exports) - Imports,
          `Dispatch Down` = if_else(
            `Curtailment SNSP` == "Curtailment SNSP",
            `Generated RES` - possible_res,
            `Dispatch Down`
          ),
          `Generated RES` = if_else(
            `Curtailment SNSP` == "Curtailment SNSP",
            possible_res,
            `Generated RES`
          ),
          Curtailment = 0.17 * `Dispatch Down`,
          Constraint = 0.83 * `Dispatch Down`,
          
        ) %>%
        mutate(
          `Cumulative Demand (MWh)` = cumsum(`Demand`),
          `Cumulative RES (MWh)`    = cumsum(`Generated RES`),
          `Cumulative Dispatch Down (MWh)` = cumsum(`Dispatch Down`)
        ) %>%
        mutate(SNSP = pmin(SNSP, `Planned SNSP`, na.rm = FALSE))
      # page_fluid(
      #   icon('arrow-right',class= 'visually-hidden'),
      
      # yearly_df <- yearly_df %>% 
      #   mutate(across(-c('Curtailment SNSP','year'),
      #                 ~as.numeric(round(.x))))
        
      yearly_df %>% 
          rowwise( ) %>% 
          mutate(.before = 2,Flow=as.character(tagList(div(style = 'min-width:100px;',class= 'm-1 p-1',
            div(div(class='lead',icon('arrow-left'),
                    f(Imports)	),
                p(class='text-muted text-bg-info p-1 m-1 rounded-2','Imports')),
            div(class = 'float-right',
                span(class='text-muted',f(Exports)	,icon('arrow-right')),
                p(class='text-bg-danger p-1 m-1 rounded-2','Exports')
            ),
          )))) %>% 
          mutate(.after = 2, Renewables=as.character(tagList(div(
            h4(class='text-bg-success p-1 m-1 rounded-2',icon('leaf'),f(`Generated RES`)),
            div(class = 'text-center',
                div(f(`Generated Solar`),'/',f(`Available Solar`), icon(class='text-yellow','sun')),
                div(f(`Generated Wind`),'/',f(`Available Wind`), icon(class='text-blue','fan'))
            ))))) %>% 
          mutate(
            .after = 3,Demand=as.character(tagList(div( class= 'm-1 p-1', 
              icon(class = 'fw-bold','bolt'),icon(class = 'fw-bold','bolt'),icon(class = 'fw-bold','bolt'),
              icon(class= 'float-end','industry'),icon(class= 'float-end','house'),
              p(class='text-bg-dark p-1 rounded-2 m-1' ,' MWh/year ' ,f(Demand), 
              icon(class= 'text-centre','industry'),
              icon(class= '','house')
)
            )))) %>% 
          mutate(.after = 4,year= as.character(h1(year))) %>% 
          mutate(Target = as.character(div(style = 'width:105px;',circular_value(f(Target*100)) ) ))%>% 
          # mutate(.after = 5,Target = as.character(h1(Target)) )%>% 
          mutate(gap='') %>%
          mutate(SNSP = as.character(div(class= 'm-1 p-1',tags$ul(tags$li(class='text-muted','Actual / Planned SNSP'),
                                             tags$li(paste(format(SNSP,digits=2) ,'/', format(`Planned SNSP`,digits=2) )),
                                             tags$li( class='text-danger',paste('\u394',format(`Planned SNSP`-SNSP,digits=2) ))
          )))) %>%
          mutate(`Dispatch Down` = as.character(div(class= 'm-1 p-1',
            tags$ul(
              tags$li(paste('Constraint:',format(Constraint,digits=2) )),
              tags$li(paste('Curtailment:',format(Curtailment,digits=2) ))),
            
            div(style = 'border-style: double', 
                class=' rounded-3 border-4 text-muted text-center','Dispatch Down',
                h5(class = 'fw-bold','MWh/year',f(`Dispatch Down`)))
            
          ))) %>%
         # mutate(`Cumulative Demand (MWh)`) %>%
          # select(Target, year, Flow, Renewables, SNSP,Demand,  `Dispatch Down`, `Cumulative Demand (MWh)`,
          #        `Cumulative RES (MWh)`   ,`Cumulative Dispatch Down (MWh)`) %>% 
          select(Target, year, Flow, Renewables, SNSP,Demand,  `Dispatch Down`) %>% 
          DT::datatable(height = '100vh',
                        escape = F, 
                        
                        rownames=F,
                        selection='none',
                        options = list(dom = '',ordering = FALSE,pageLength = 10, scrollX = TRUE))
      #)
    })
    
    # output$yearly_sums_table <- renderTable({
    #   df <- combined_forecast_ci()
    #   df$date <- as.Date(df$date)
    # 
    #   df$year <- format(df$date, "%Y")
    # 
    #   num_cols <- sapply(df, is.numeric)
    # 
    #   yearly_df_2 <- df %>%
    #     group_by(year) %>%
    #     summarise(across(which(num_cols), mean, na.rm = TRUE)) %>%
    # 
    #     mutate(
    #       Target = `Generated RES` / `Demand`,
    #       Target = pmin(Target, 1),
    #       SNSP   = (`Generated RES` + `Imports`) /
    #         (`Demand` + `Exports`)
    #     ) %>%
    # 
    #     mutate(
    #       `Planned SNSP` = case_when(
    #         year >= 2025 & year <= 2027 ~ 0.80,
    #         year >= 2028 & year <= 2029 ~ 0.85,
    #         year == 2030               ~ 0.90,
    #         TRUE                       ~ NA_real_
    #       )
    #     ) %>%
    #     mutate(
    #       # .dd_total = `Dispatch Down Wind` + `Dispatch Down Solar`,
    # 
    #       # Curtailment = if_else(
    #       #   .dd_total > 0,
    #       #   (0.1667 * `Dispatch Down Wind` + 0.1461 * `Dispatch Down Solar`) / .dd_total,
    #       #   NA_real_
    #       # ),
    #       # Constraint = if_else(
    #       #   .dd_total > 0,
    #       #   (0.8318 * `Dispatch Down Wind` + 0.8542 * `Dispatch Down Solar`) / .dd_total,
    #       #   NA_real_
    #       # ),
    # 
    #       `Curtailment SNSP` = ifelse(SNSP > `Planned SNSP`, "Curtailment SNSP", ""))
    #     # mutate(
    #     #   possible_res = `Planned SNSP` * (Demand + Exports) - Imports,
    #     #   `Dispatch Down` = if_else(
    #     #     `Curtailment SNSP` == "Curtailment SNSP",
    #     #     `Generated RES` - possible_res,
    #     #     `Dispatch Down`
    #     #   ),
    #     #   `Generated RES` = if_else(
    #     #     `Curtailment SNSP` == "Curtailment SNSP",
    #     #     possible_res,
    #     #     `Generated RES`
    #     #   ),
    #     #   Curtailment = 0.17 * `Dispatch Down`,
    #     #   Constraint = 0.83 * `Dispatch Down`,
    #     # 
    #     # ) %>%
    #     # 
    #     # mutate(SNSP = pmin(SNSP, `Planned SNSP`, na.rm = FALSE))
    # 
    #   yearly_df_2
    #   }, digits = 3, rownames = FALSE)
    
    sheet1_df <- read_excel(
      "./data/policy_uplift.xlsx",
      sheet = "all_3"
    )
    
    sheet2_df <- read_excel(
      "./data/policy_uplift.xlsx",
      sheet = "last3_3"
    )
    
    sheet1_df <- sheet1_df %>%
      select("Policy", "Year", "RES increase", "Target increase", "dispatch down decrease percentage")
    
    sheet2_df <- sheet2_df %>%
      select("Policy", "Year", "RES increase", "Target increase", "dispatch down decrease percentage")
    
    
    # --- RENDER GT TABLES ---
    
    output$sheet1_tbl <- render_gt({
      sheet1_df %>%
        mutate(`RES increase` = `RES increase` / 1000) %>%   # convert MWh → GWh
        gt() %>%
        fmt_number(
          columns = "RES increase",
          decimals = 1,
          use_seps = TRUE,
          pattern = "{x} GWh"   # label in the table
        ) %>%
        fmt_percent(columns = "Target increase", decimals = 1) %>%
        fmt_percent(columns = "dispatch down decrease percentage", decimals = 1) %>%
        tab_options(table.width = pct(100), data_row.padding = px(6)) %>%
        tab_style(
          style = cell_fill(color = "#f7f7f7"),
          locations = cells_body()
        )
    })
    
    
    output$sheet2_tbl <- render_gt({
      sheet2_df %>%
        mutate(`RES increase` = `RES increase` / 1000) %>%   # convert MWh → GWh
        gt() %>%
        fmt_number(
          columns = "RES increase",
          decimals = 1,
          use_seps = TRUE,
          pattern = "{x} GWh"   # label in the table
        ) %>%
        fmt_percent(columns = "Target increase", decimals = 1) %>%
        fmt_percent(columns = "dispatch down decrease percentage", decimals = 1) %>%
        tab_options(table.width = pct(100), data_row.padding = px(6)) %>%
        tab_style(
          style = cell_fill(color = "#f7f7f7"),
          locations = cells_body()
        )
    })
    
   
    
    
    
#end of server    
    })
  }
