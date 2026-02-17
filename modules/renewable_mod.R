# Renewable Energy Simulation Module

# Module UI
renewable_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Add padding for navbar when used in tabPanel
    div(style = "padding-top: 20px;"),
    
    div(class = 'd-flex justify-content-between flex-grow',
      ### column - 1 ----
      div(class = 'px-5 w-25',
        
        # ScrollSpy Navigation in left panel
        div(class = "mb-5 position-fixed", style = "top: 100px; z-index: 100;",
          div(class = "glass-card card",
            div(class = "card-body p-2",
              tags$ul(class = "nav nav-pills justify-content-center gap-2 mb-3",
                tags$li(class="nav-item",
                  tags$a(class = "nav-link p-2", href = "#parameters",
                    title = "Top level NI Energy Parameters",
                    `data-bs-toggle` = "tooltip",
                    `data-bs-placement` = "bottom",
                    icon("sliders-h")
                  )),
                tags$a(class = "nav-link p-2", href = "#capacity",
                  title = "Offshore Pre-planning and Capacity Factors",
                  `data-bs-toggle` = "tooltip",
                  `data-bs-placement` = "bottom",
                  icon("battery-three-quarters")
                ),
                tags$a(class = "nav-link p-2", href = "#progression",
                  title = "Stage Progression Probability",
                  `data-bs-toggle` = "tooltip",
                  `data-bs-placement` = "bottom",
                  icon("arrow-right")
                ),
                tags$a(class = "nav-link p-2", href = "#timelines",
                  title = "Stage Duration Timelines",
                  `data-bs-toggle` = "tooltip",
                  `data-bs-placement` = "bottom",
                  icon("clock")
                )
              )
            ),
            actionButton(inputId = ns('submit'), 
              label = tagList(icon("play"), " Run Simulation"),
              class = "btn btn-primary",
              style = "white-space: nowrap;")
          )
        ),
        
        # Add spacing to prevent overlap
        div(style = "margin-top: 140px;"),
        
        # Note: All the input controls from app.R would go here
        # For brevity, I'm showing the structure - you'll need to copy all controls from app.R
        # and wrap their IDs with ns()
        
        div(id = "parameters", class = "scroll-section",
          h1(class = 'text-body-secondary', " "),
          
          div(class = "ion-range-container",
            h5(class = 'lead', icon("chart-area"), " Baseline Renewables (GWhr)"),
            tags$input(
              id = ns("baseline_renewable"),
              type = "number",
              step = 10,
              name = "baseline_renewable",
              value = 3126
            )
          ),
          
          div(class = "slider-section",
            h5(class = 'lead', icon("calendar-alt"), " 2030 Demand (TWhr/yr)"),
            tags$input(
              id = ns("demand_2030"),
              type = "number",
              step = 0.2,
              name = "demand_2030",
              value = 8
            )
          ),
          
          div(class = "slider-section",
            h5(class = 'lead', "Number of Simulation runs"),
            tags$input(
              id = ns("number_runs"),
              type = "number",
              step = '10',
              name = "number_runs",
              value = 5
            )
          )
          
          # ... Continue with all other input controls from app.R
          # Each input ID must be wrapped with ns()
        )
      ),
      
      ### column - 2 ----
      div(class = 'px-5 flex-grow-1',
        # Charts section
        div(class = 'd-flex flex-wrap justify-content-between gap-3 mb-4',
          div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
            h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("chart-line"), " Pre-planning Cumulative"),
            div(style = 'height: 300px;',
              echarts4rOutput(ns('preplanning_cumulative_echarts'), height = '300px')
            ),
            div(class = 'd-flex justify-content-center mt-2',
              div(
                style = "border:5px solid red;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:120px;height:120px;",
                tags$small(class = 'text-muted', 'Cumulative'),
                h5(textOutput(inline = T, ns('new_res_preplanning_cumulative_end_date')),'MW'),
                tags$small(class = 'text-muted text-center', style = 'font-size: 0.7rem;', 'by end of simulation')
              )
            )
          ),
          
          div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
            h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("chart-bar"), " Pre-planning Monthly"),
            div(style = 'height: 300px;',
              echarts4rOutput(ns('preplanning_yearly'), height = '300px')
            )
          )
        )
        
        # ... Continue with all other charts and outputs
        # Each output ID must be wrapped with ns()
      ),
      
      ### column - 3 (sticky sidebar) ----
      div(style = 'position:sticky; top:8%; height:90vh;',
        # Loading overlay
        div(id = ns('loading'), 
          class = 'alert alert-danger rounded-3 p-3',
          style = 'position:fixed;top:10%;left:60%;transform:translate(-50%,-50%);z-index:9999;opacity:0.8;display:none;box-shadow:0 4px 6px rgba(0,0,0,0.3);',
          div(class='d-flex gap-3 align-items-center',
            span(class="loader"),
            h4(class = 'mb-0 lead','Loading '),
            span(class = 'badge pill-rounded text-bg-light',
              tags$small(class=' mb-0','Inputs locked')
            )
          )
        ),
        
        div(class = 'py-2 px-5 me-5 bg-info-subtle rounded-5 position-relative',
          span(class="badge bg-info rounded-2 rounded-pill",
            style="top: -15px; left: 10px; position: absolute;",
            h6(style="text-align:center;color:white;",'80% by 2030')
          ),
          
          div(class = 'alert alert-info',
            style = "justify-content: center;border-radius: 25px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
            tags$small(class = 'mb-1 text-muted text-center', 'Proportion of Demand Renewable Sources'),
            h3(textOutput(inline = T, ns('target_pct_target_date')),"%"),
            tags$small(class = 'text-muted text-centre text-center','Total RES/ TER'),
            tags$h4(class = 'mt-3text-white text-centre text-center','by 2030')
          )
        )
      )
    )
  )
}

# Module Server
renewable_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function
    getSliderValue <- function(inputValue, defaultValue) {
      if (is.null(inputValue) || inputValue == "") {
        return(defaultValue)
      }
      return(as.numeric(inputValue))
    }
    
    # Reactive values
    params <- reactiveValues(
      baseline_renewables = 3961,
      demand_2030 = 9,
      number_runs = 100,
      capacity_factor = 22,
      planning_connection_prob = 77,
      connection_construction_prob = 78,
      connection_completion_prob = 100,
      planning_connection_time = 8*4,
      connection_construction_time = 17*4,
      construction_completion_time = 4*4,
      planning_connection_time_wind = 29*4,
      connection_construction_time_wind = 45*4,
      construction_completion_time_wind = 12*4,
      time_approach = "empirical",
      progression_prob_method = "empirical",
      offshore_option_selected = "offshore_exclude",
      offshore_start = "2032-01-01",
      offshore_capacity = 0.5
    )
    
    # Observe submit button
    observeEvent(input$submit, {
      session$sendCustomMessage("toggleLoadingBtn", "show")
      
      # Your simulation logic from app.R goes here
      # ...
      
      session$sendCustomMessage("toggleLoadingBtn", "hide")
    })
    
    # Outputs
    output$preplanning_cumulative_echarts <- renderEcharts4r({
      # Chart rendering logic
    })
    
    output$target_pct_target_date <- renderText({
      "80" # Placeholder
    })
    
    # ... Add all other outputs from app.R server logic
  })
}
