# Combined Energy Planning Dashboard

library(shiny)
library(bslib)
library(lubridate)
library(tidyverse)
library(future)
library(promises)
library(memoise)
library(digest)
library(echarts4r)

# Load renewable simulation sources
options(shiny.devmode = TRUE)
options(shiny.sanitize.errors = TRUE)
target_date = '2030-01-01'
end_date = '2032-01-01'

mons = unique(floor_date(seq( from = as.Date('2025-01-01'), 
                              to = as.Date(end_date),
                              by = 1 ), 'month')
)

load('init_vars.RData')
f <- function(p){ format(big.mark=',',round(p)) }

# Helper function to get slider value
getSliderValue <- function(inputValue, defaultValue) {
  if (is.null(inputValue) || inputValue == "") {
    return(defaultValue)
  }
  return(as.numeric(inputValue))
}

source('./components/circular_value.R')
source('./components/sticky_side_bar.R')
source('./components/colour_scale_bar.R')

mons = unique(floor_date(seq( from = as.Date('2025-01-01'), 
                              to = as.Date(end_date),
                              by = 1 ), 'month')
)

source('1_pipeline_state.R')
source('2_project_progression.R')
source('3_forecast_projects.R')
source('4_0_mechanism_utils.R')
source('4_2_mechanism_graphics.R')

# Set up for async processing
plan(multisession)

# Load energy flow diagram modules
source("./preprocess/params.R")
source("./preprocess/functions.R")
source("./R/mod_diagram.R")
source("./R/mod_generation.R")
source("./R/mod_transmission.R")
source("./R/mod_distribution.R")
source("./R/mod_supply.R")

# Load renewable simulation module
# source("./modules/renewable_mod.R")

#ui
ui <- navbarPage( 
  title = "Energy Planning Dashboard",
  theme = bs_theme(version = 5, 
                   bootswatch = 'lumen',
                   primary = '#2196F3',
                   success = 'rgb(140,233,106)'),

  id = "main_nav",
  
  header = tags$head(
    # External CDN resources for renewable simulation
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/css/ion.rangeSlider.min.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/js/ion.rangeSlider.min.js"),
    tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
    tags$script(id = "MathJax-script", async = TRUE, src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
    
    # Local custom files
    tags$link(rel = "stylesheet", href = "styles/energy-app.css"),
    tags$script(src = "js/mathjax-config.js"),
    tags$script(src = "js/scrollspy.js"),
    tags$script(src = "js/ion-slider-init.js"),
    tags$script(src = "js/slider-config.js"),
    tags$script(src = "js/progress-handlers.js"),
    tags$script(src = "js/radio-handlers.js"),
    tags$script(src = "js/input_date_handlers.js"),
    
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleLoadingBtn', function(state) {
        var loadingEl = document.getElementById('loading');
        if (loadingEl) {
          if (state === 'show') {
            loadingEl.style.display = 'block';
          } else {
            loadingEl.style.display = 'none';
          }
        }
      });
      
      // Initialize Bootstrap tooltips
      $(document).ready(function() {
        var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
          return new bootstrap.Tooltip(tooltipTriggerEl);
        });
      });
      
      // Energy flow diagram click handler
      document.addEventListener('click', function(ev) {
        var host = document.getElementById('diagram-diagram');
        var anchor = ev.target.closest && ev.target.closest('a');
        if (!host || !anchor || !host.contains(anchor)) return;

        ev.preventDefault();
        ev.stopPropagation();

        var href = anchor.getAttribute('xlink:href') || anchor.getAttribute('href') || '';
        var idx = href.indexOf('#');
        var frag = (idx >= 0) ? href.substring(idx + 1) : '';
        if (frag) Shiny.setInputValue('diagram_click', frag, {priority: 'event'});
      }, true);
    ")),
    
    tags$style(HTML("
    shiny-error-console {
    visibility:hidden;
}

    td {
    border-color: white;
    border-style: solid;
    border-width: 2px;
}
      .back-bar { position: sticky; top: 0; z-index: 10; background: #fff; padding: 8px 0; }
      .centered-content { text-align: center; }
      #diagram svg { display: block; margin: 0 auto; max-width: 90%; height: auto; }
      .glass-card { background: rgba(255,255,255,0.95); backdrop-filter: blur(10px); }
    "))
  ),
  
  # Tab 1: Renewable Energy Simulation (from app.R)
  tabPanel("Renewable Simulation",
    icon = icon("solar-panel"),
    value = "renewable_tab",
    
    tags$head(
      # External CDN resources
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/css/ion.rangeSlider.min.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/js/ion.rangeSlider.min.js"),
      tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
      tags$script(id = "MathJax-script", async = TRUE, src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
      
      # Local custom files
      tags$link(rel = "stylesheet", href = "styles/energy-app.css"),
      tags$script(src = "js/mathjax-config.js"),
      tags$script(src = "js/scrollspy.js"),
      tags$script(src = "js/ion-slider-init.js"),
      tags$script(src = "js/slider-config.js"),
      tags$script(src = "js/progress-handlers.js"),
      tags$script(src = "js/radio-handlers.js"),
      tags$script(src = "js/input_date_handlers.js"),
      
      tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleLoadingBtn', function(state) {
        var loadingEl = document.getElementById('loading');
        if (loadingEl) {
          if (state === 'show') {
            loadingEl.style.display = 'block';
          } else {
            loadingEl.style.display = 'none';
          }
        }
      });
      
      // Initialize Bootstrap tooltips
      $(document).ready(function() {
        var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
          return new bootstrap.Tooltip(tooltipTriggerEl);
        });
      });
    "))
      
    ),
    
    # Navigation bar - simplified with text only
    tags$nav(class = "navbar navbar-expand-lg glass-card rounded-0 p-2 sticky-top", #  fixed-top bg-white
             `data-bs-theme` = "light",
             div(class = "container-fluid d-inline",
                 tags$a(class = "navbar-brand fw-bold d-inline", href = "#top",
                        "Renewable Energy Simulation and Projection",#br(),
                        p(class = 'lead d-inline',"80% by 2030")
                 ),
                 # div(class = "ms-auto",
                 #   tags$span(class = "navbar-text",
                 #     "80% by 2030"
                 #   )
                 # )
             )
    ),
    
    # Add padding for fixed navbar
    # div(style = "padding-top: 80px;"),
    
    br(), br(),
    
    #fluidRow(
    #  column(4, #offset = 1,ß
    div( class = 'd-flex justify-content-between flex-grow',
         ### column - 1 ----
         div(class = 'px-5 w-25 ',#mx-5
             
             # ScrollSpy Navigation in left panel
             div(class = "mb-5 position-sticky", style = "top: 90px; z-index: 100;",
                 div(class = "glass-card card",
                     div(class = "card-body p-2",
                         tags$ul(class = "nav nav-pills justify-content-center gap-2 mb-3",
                                 tags$li(class="nav-item",
                                         tags$a(class = "nav-link p-2", href = "#parameters",
                                                title = "Top level NI Energy Parameters",
                                                `data-bs-toggle` = "tooltip",
                                                `data-bs-placement` = "bottom",
                                                icon("sliders-h")
                                         )
                                         ),
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
                     
                     div(class = 'btn-group',
                     actionButton(inputId = 'submit', 
                                  label = tagList(icon("play"), " Run Simulation"),
                                  class = "btn btn-primary",
                                  style = "white-space: nowrap;"),
                     actionButton(inputId = 'take_state', 
                                  label = tagList(icon("arrow-up-right-from-square"), " Take state"),
                                  class = "btn btn-primary",
                                  style = "white-space: nowrap;"),
                 ))
             ),
             
             # Add spacing to prevent overlap with fixed navbar and to separate from content
             div(style = "margin-top: 60px;"),
             
             # Parameters Section
             div(id = "parameters", class = "scroll-section",
                 # h2(class = 'text-body-secondary', icon("sliders-h"), " Energy Parameters"),
                 h1(class = 'text-body-secondary',  " "),
                 
                 # Ion Range Slider with flat skin
                 div(class = "ion-range-container",
                     h5(class = 'lead',icon("chart-area"), " Baseline Renewables (GWhr)"),
                     tags$input(
                       id = "baseline_renewable",
                       type = "number",
                       step = 10,
                       name = "baseline_renewable",
                       value = 3126
                     )
                 ),
                 
                 div(class = "slider-section",
                     h5(class = 'lead', icon("calendar-alt"), " 2030 Demand (TWhr/yr)"),
                     tags$input(
                       id = "demand_2030",
                       type = "number",
                       step = 0.2,
                       name = "demand_2030",
                       value = 8
                     )
                 ),
                 
                 div(class = "slider-section",
                     h5(class = 'lead', "Number of Simulation runs"),
                     tags$input(
                       id = "number_runs",
                       type = "number",
                       step = '10',
                       name = "number_runs",
                       value = 5
                     ),
                     
                     tags$ul(class = 'text-body-secondary pt-3',
                             tags$li(class = 'small','The data is heavily skewed, a larger range of runs is needed for robust confidence intervals'),
                             tags$li(class = 'small','If experimenting you may lower the number of runs',tags$i('at your own risk')),
                             tags$li(class = 'small','For publication and reporting quality results a minimum of 80 runs is necessary'),
                             tags$li(class = 'small','For comparing scenarios a minimum of 80 runs is necessary'),
                             tags$li(class = 'small','For  Robust confidence intervals a minimum of 80 runs is necessary',
                                     tags$small(class = 'text-body-secondary', 'Confidence intervals should always be reported along side Point estimates'))
                             
                     )
                 ),
                 br()
             ),
             
             # div(class = "slider-section",
             #   # h4('⚡ Capacity Parameters'),
             #    h6(class = 'lead', "Capacity Factor (%)"),
             #   tags$input(
             #     id = "capacity_factor",
             #     type = "number",
             #     name = "capacity_factor",
             #     value = ""
             #   )
             # ),   
             
             div(id = 'capacity', class = "scroll-section",
                 div(class = "slider-section",
                     h6(class = 'lead', " Capacity Factor") #icon("battery-three-quarters")
                 ),
                 # Mathematical equation for capacity calculation
                 div(class = "alert alert-info mt-3 mb-3 m-4", role = "alert",
                     h6(class = "lead alert-heading", icon("calculator"), " Nameplate Capacity Formula"),
                     br(),
                     tags$small(class = "text-muted fs-8",  " Conversion between Nameplate capacity and actual
             Gernerated output is described by the capacity factor, CF"),
                     
                     div(class = "text-center p-3", style = "background-color: #f8f9fa; border-radius: 8px; margin: 10px 0;",
                         tags$p(style = "font-size: 12px; margin: 0; color:grey;",
                                "$$\\text{GW (nameplate)} = \\frac{\\text{TWh/yr}}{8.76 \\times \\text{CF}}$$"
                         )
                     ),
                     tags$small(class = "text-muted",
                                tags$strong("Where"), " CF is the Capacity Factor (% as a decimal), 8.76 = hours per year ÷ 1000"
                     )
                 ),
                 
                 div(class = "slider-section",
                     
                     div(class = "input-group mb-3",
                         tags$span(class = "input-group-text", " Capacity Factor"),
                         tags$input(id = "capacity_factor", type = "number", value = 22, min = 15, max = 40, step = 1, 
                                    class = "form-control bg-success-subtle", `aria-label` = "Capacity factor percentage"),
                        tags$span(class = "input-group-text", icon("percent"))
                     ),
                     
                     # Real-time capacity factor feedback
                     div(class = "alert alert-light mt-2", style = "padding: 8px;",
                         tags$small(
                           icon("info-circle"), " Current CF: ",
                           tags$strong(textOutput("capacityFactorValue", inline = TRUE)), "%"
                         )
                     ),
                     tags$p(textOutput('generationCapacityConversion'))
                 ),
                 
                 div(class = "slider-section",
                     
                     h5(class = 'lead', " Pre - Planning Estimate"),
                     
                     div( class="btn-group", role="group", `aria-label`="Basic radio toggle button group",
                          tags$input(type="radio", class="btn-check", name="PlanEstimate", id="optimistic",
                                     autocomplete="off"),
                          tags$label(class="btn btn-outline-primary", `for`="optimistic", icon("thumbs-up"), " Optimistic"),
                          
                          tags$input(type="radio", class="btn-check", name="PlanEstimate", id="conservative", checked = TRUE,
                                     autocomplete="off"),
                          tags$label(class="btn btn-outline-primary", `for`="conservative", icon("shield-alt"), " Conservative"),
                          
                          tags$input(type="radio", class="btn-check", name="PlanEstimate", id="survey",
                                     autocomplete="off"),
                          tags$label(class="btn btn-outline-primary", `for`="survey", icon("poll"), " Sampled from Survey")
                     )
                 ),
                 
                 # Project Progression Probability Method Selection
                 
                 
                 div(class = "slider-section",
                     
                     
                     h5(class='lead', icon("water"), ' Offshore Wind Options'), 
                     
                     div( class="btn-group", role="group", `aria-label`="Basic radio toggle button group",
                          tags$input(type="radio", class="btn-check", name="offshore_option", id="offshore_include",
                                     autocomplete="off"),
                          tags$label(class="btn btn-outline-success", `for`="offshore_include", icon("water"), " Include Offshore Wind"),
                          
                          tags$input(type="radio", class="btn-check", name="offshore_option", id="offshore_exclude",
                                     autocomplete="off", checked = "checked"),
                          tags$label(class="btn btn-outline-success", `for`="offshore_exclude", icon("times-circle"), " Onshore Only")
                     ),
                     
                     br(),
                     div(class = 'mb-3',
                         tags$label(`for`="offshore_start",'Start month'),
                         tags$input(class = 'form-control bg-success-subtle', #bg-primary-subtle
                                    type="month", 
                                    id="offshore_start", 
                                    name="start", 
                                    min="2029-01", 
                                    max="2032-01", 
                                    value="2031-01"
                         )
                     ),
                     br(),br(),
                     
                     h5(class='lead', ' Offshore Wind Capacity (GW)'), 
                     
                     
                     tags$input(
                       id = "offshore_capacity",
                       type = "number",
                       step = 0.1, 
                       name = "offshore_capacity",
                       value = 0.5
                     )
                 )
             ),
             
             div(class = "slider-section",
                 
                 h5(class = 'lead', icon("percentage"), ' Project Progression Probability Method'),
                 
                 div( class="btn-group", role="group", `aria-label`="Basic radio toggle button group",
                      tags$input(type="radio", class="btn-check", name="progression_prob_method", id="progression_prob_custom",
                                 autocomplete="off"),
                      tags$label(class="btn btn-outline-primary", `for`="progression_prob_custom", icon("edit"), " Custom"),
                      
                      tags$input(type="radio", class="btn-check", name="progression_prob_method", id="progression_prob_empirical",
                                 autocomplete="off", checked = "checked"),
                      tags$label(class="btn btn-outline-primary", `for`="progression_prob_empirical", icon("chart-bar"), " Empirical")
                 )
             ),
             
             # Project Progression Section
             div(id = "progression", class = "slider-section scroll-section",
                 h5(class='text-body-secondary', icon("percent"), ' Project Progression Probability'),
                 h6(class = 'lead',  " Planning → Connection"),
                 tags$input(
                   id = "planning_connection_prob",
                   type = "number",
                   name = "planning_connection_prob",
                   value = 72
                 ),
                 br(), br(),
                 h6(class = 'lead',  " Connection → Construction"),
                 tags$input(
                   id = "connection_construction_prob",
                   type = "number",
                   name = "connection_construction_prob",
                   value = 85
                 ),
                 br(), br(),
                 h6(class = 'lead',  " Construction → Completion"),
                 tags$input(
                   id = "connection_completion_prob",
                   type = "number",
                   name = "connection_completion_prob",
                   value = 100
                 )
             ),
             
             
             
             div(class = "slider-section",
                 
                 h5(class = 'text-body-secondary', icon("clock"), ' Project progression Time'),
                 div( class = "btn-group", 
                      role = "group",
                      `aria-label` = "Basic radio toggle button group",
                      
                      tags$input(type="radio", class="btn-check", name="project_progression", id="custom",
                                 autocomplete="off"),
                      tags$label(class="btn btn-outline-primary", `for`="custom", icon("edit"), " Custom"),
                      
                      
                      tags$input(type="radio", class="btn-check", name="project_progression", id="empirical",
                                 autocomplete="off", checked = "checked"),
                      tags$label(class="btn btn-outline-primary", `for`="empirical", icon("chart-bar"), " Empirical")
                 )
             ),
             
             # checkboxInput("apply_manual_times", 
             #               "", 
             #               value = T),
             
             # Timelines Section
             div(id = "timelines", class = "slider-section scroll-section",
                 h5(class = 'text-body-secondary', icon("sun"), ' Solar Photovoltaic Timelines (months)'),
                 h6(class = 'lead',  " Planning → Connection"),
                 tags$input(
                   id = "planning_connection_time",
                   class= 'disabled',
                   type = "number",
                   step = 1,
                   name = "planning_connection_time",
                   value = 17
                 ),
                 br(), 
                 br(),
                 h6(class = 'lead',  " Connection → Construction"),
                 tags$input(
                   id = "connection_construction_time",
                   type = "number",
                   step = 1,
                   name = "connection_construction_time",
                   value = 4
                 ),
                 br(), 
                 br(),
                 h6(class = 'lead',  " Construction → Completion"),
                 tags$input(
                   id = "construction_completion_time",
                   type = "number",
                   step = 1,
                   name = "construction_completion_time",
                   value = 8
                 )
             ),    
             
             div(class = "slider-section",
                 h5(icon("wind"), ' Wind Power Timelines'),
                 h6(class = 'lead',  " Planning → Connection "),
                 tags$input(
                   id = "planning_connection_time_wind",
                   type = "number",
                   step= 1,
                   name = "planning_connection_time_wind",
                   value = 29
                 ),
                 br(), br(),
                 h6(class = 'lead',  " Connection → Construction "),
                 tags$input(
                   id = "connection_construction_time_wind",
                   type = "number",
                   step = 1,
                   name = "connection_construction_time_wind",
                   value = 45
                 ),
                 br(), br(),
                 h6(class = 'lead',  " Construction → Completion "),
                 tags$input(
                   id = "construction_completion_time_wind",
                   type = "number",
                   step = 1,
                   name = "construction_completion_time_wind",
                   value = 12
                 )
             )
         ),
         ### column - 2----
         #column(6, class = 'px-5 ',#offset = 1, mx-5
         div(class = 'px-5 flex-grow-1',#offset = 1, mx-5
             # Dashboard Section
             #div(id = "dashboard", class = "scroll-section",
             # h2(class = "text-center mb-4", icon("chart-pie"), " Energy Dashboard"),
             
             #h2(class = "text-center mb-4",  " "),
             
             
             
             # Scenario input section
             # div(class='d-flex flex-column justify-content-center align-items-center ',
             #     div(class='w-50',
             #    div(class = "input-group mb-3",
             #        tags$input(type = "text", class = "form-control", 
             #                   placeholder = " Submit Scenario for simulation ", 
             #                   `aria-label` = "Scenario name input", 
             #                   disabled = T,
             #                   `aria-describedby` = "button-addon2"),
             
             #  actionButton(inputId = 'submit', 
             #               label = tagList(icon("play"), " Run Simulation"),
             #               class = "btn btn-primary",
             #               style = "white-space: nowrap;"),
             
             
             #  ),
             
             # Cache management section
             #  div(class = "input-group mb-3",
             #      tags$button(class = "btn btn-outline-secondary btn-sm", type = "button", id = "clear-cache",
             #                  icon("trash"), " Clear Cache",
             #                  title = "Clear all cached simulation results"
             #      ),
             
             #      tags$input(type = "text", class = "form-control", 
             #                 placeholder = "Clear cache", 
             #                 `aria-label` = "Clear cache", 
             #                 disabled = T,
             #                 `aria-describedby` = "Clear cache")
             
             #      )
             
             #  )#,
             
             # div(class = "input-group-text small text-muted",
             #     "Cache saves time for identical parameters"
             #)
             
             #),
             
             
             
             # div(class = "module-border-wrap",
             #     div(class = "module",
             #       div(h4( class ='flex-direction-row', textOutput('planning_connection_prob'),'%')),
             #       p(class = "text-center text-muted",  " Production")
             #     )
             #   ),
             
             
             # plotOutput("distPlot"),
             # plotOutput('preplanning_cumulative'),
             
             # Charts arranged horizontally in rows
             div(class = 'd-flex flex-wrap justify-content-between gap-3 mb-4',
                 # Pre-planning Cumulative
                 div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
                     h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("chart-line"), " Pre-planning Cumulative"),
                     div(style = 'height: 300px;',
                         echarts4rOutput('preplanning_cumulative_echarts', height = '300px')
                     ),
                     div(class = 'd-flex justify-content-center mt-2',
                         div(
                           style = "border:5px solid red;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:120px;height:120px;",
                           tags$small(class = 'text-muted', 'Cumulative'),
                           h5(textOutput(inline = T,'new_res_preplanning_cumulative_end_date'),'MW'),
                           tags$small(class = 'text-muted text-center', style = 'font-size: 0.7rem;', 'by end of simulation')
                         )
                     )
                 ),
                 # Pre-planning Monthly
                 div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
                     h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("chart-bar"), " Pre-planning Monthly"),
                     div(style = 'height: 300px;',
                         echarts4rOutput('preplanning_yearly', height = '300px')
                     ),
                     div(class = 'd-flex justify-content-center mt-2',
                         div(
                           style = "border:5px solid red;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:120px;height:120px;",
                           tags$small(class = 'text-muted', 'Annual New RES'),
                           h5(textOutput(inline = T,'new_res_preplanning_yearly_end_date'),'MW'),
                           tags$small(class = 'text-muted', style = 'font-size: 0.7rem;', 'at simulation end')
                         )
                     )
                 )
             ),
             
             div(class = 'd-flex flex-wrap justify-content-between gap-3 mb-4',
                 # Current Projects Cumulative
                 div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
                     h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("chart-area"), " Current Projects Cumulative"),
                     div(style = 'height: 300px;',
                         echarts4rOutput('current_cumulative', height = '300px')
                     ),
                     div(class = 'd-flex justify-content-center mt-2',
                         div(
                           style = "border:5px solid #2196F3;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:120px;height:120px;",
                           tags$small(class = 'text-muted', 'Total New RES'),
                           h5(textOutput(inline = T,'new_res_current_cumulative_end_date'),'MW'),
                           tags$small(class = 'text-muted', style = 'font-size: 0.7rem;', 'by simulation end')
                         )
                     )
                 ),
                 # Current Projects Monthly
                 div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
                     h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("calendar-alt"), " Current Projects Monthly"),
                     div(style = 'height: 300px;',
                         echarts4rOutput('current_yearly', height = '300px')
                     ),
                     div(class = 'd-flex justify-content-center mt-2',
                         div(
                           style = "border:5px solid var(--bs-info);justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:120px;height:120px;",
                           tags$small(class = 'text-muted', 'New RES'),
                           h5(textOutput(inline = T,'new_res_current_yearly_end_date'),'MW'),
                           tags$small(class = 'text-muted text-center', style = 'font-size: 0.7rem;', 'current pipeline')
                         )
                     )
                 )
             ),
             
             div(class = 'd-flex flex-wrap justify-content-between gap-3 mb-4',
                 # Offshore Wind Capacity Monthly
                 div(class = 'flex-fill', style = 'min-width: 45%; max-width: 48%;',
                     h5(class = 'text-body-secondary px-3 py-2 fw-bold', icon("water"), " Offshore Wind Capacity Monthly"),
                     div(style = 'height: 300px;',
                         echarts4rOutput('offshore_wind_plot', height = '300px')
                     ),
                     div(class = 'd-flex justify-content-center mt-2',
                         div(
                           style = "border:5px solid limegreen;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:120px;height:120px;",
                           tags$small(class = 'text-muted', 'Annually producing'),
                           h6(textOutput(inline = T,'offshore_wind_capacity'),'MW/yr'),
                           h6('From ',textOutput(inline = T,'offshore_wind_start'))
                         )
                     )
                 )
             ),
             
             # Real-time parameter status
             div(class = "alert alert-info mt-3",
                 h6(icon("info-circle"), " Current Server-Side Parameters"),
                 textOutput("parameterStatus")
             ),
             
             # Comprehensive server-side values table
             div(class = "mt-4 d-flex justify-content-between align-items-center flex-column",
                 h5(class =  "text-body-secondary p-5", "Complete Configuration Set"), #icon("table"),
                 tableOutput("serverValuesTable")
             ),
             
             # tableOutput("paramsTable"),
             # tableOutput("summaryTable")
         ),
         
         #column(2,class = 'pe-5',
         ### column-sticky ----
         
         div(style = 'position:sticky; top:8%; height:90vh;',
             
             # Loading overlay - centered and doesn't affect layout
             div(id= 'loading', 
                 class = 'alert alert-danger rounded-3 p-3',
                 style = 'position:fixed;top:15%;left:90%;transform:translate(-50%,-50%);z-index:9999;opacity:1;display:none;box-shadow:0 4px 6px rgba(0,0,0,0.3);',
                 div(class='d-flex gap-3 align-items-center',
                     span(class="loader"),
                     
                     h4(class = 'mb-0 lead','Loading '),
                     span(class = 'badge pill-rounded text-bg-light',
                          style = '',
                          tags$small(class=' mb-0','Inputs locked')
                     )
                 )
                 
             ),
             
             div(class = 'py-2 px-5 me-5 bg-info-subtle rounded-5 position-relative',#offset = 1, mx-5
                 
                 # div(class = 'alert alert-primary rounded-2 top-0 start-100',
                 #     h3(style = 'text-align:center',':80% Target:')),
                 
                 span( class="badge bg-info rounded-2 rounded-pill",
                       style="
          top: -15px;
          left: 10px;
           position: absolute;",
                       
                       h6(style="text-align:center;color:white;",'80% by 2030')
                 ),
                 
                 div(class = 'alert alert-info',
                     style =
                       "justify-content: center;border-radius: 25px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
                     tags$small(class = 'mb-1 text-muted text-center', 'Proportion of Demand Renewable Sources'),
                     h3(textOutput(inline = T, 'target_pct_target_date'),"%"),
                     #h5( "2 MW/yr"),
                     tags$small(class = 'text-muted text-centre text-center','Total RES/ TER'),
                     tags$h4(class = 'mt-3text-white text-centre text-center','by 2030')
                     
                 ),
                 div(
                   style =
                     "background:white;justify-content: center;border-radius: 55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
                   tags$small(class = 'text-muted', 'Cumulative Total New Capacity'),
                   h4(span(textOutput(inline = T,'total_new_res_target_date'),"MW/yr")),
                   #h4( "2 MW/yr"),
                   tags$small(class = 'text-muted text-centre text-center','all new RES by 2030'),
                 ),
                 div(
                   style =
                     "background:white;border:5px solid #2196F3;justify-content: center;border-radius: 55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 10px auto;",
                   tags$small(class = 'text-muted', 'Annual New Capacity'),
                   # h3( "2 MW/yr"),
                   h3(textOutput(inline = T,'new_res_current_cumulative_target_date'),'MW'),
                   tags$small(class = 'text-muted text-centre text-center','current pipeline to 2030'),
                 ), 
                 div(
                   style =
                     "background:white;border:5px solid #2196F3;justify-content: center;border-radius: 55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
                   tags$small(class = 'text-muted', 'Annual New Capacity'),
                   # h5( "2 MW/yr"),
                   h5(textOutput(inline = T,'new_res_preplanning_cumulative_target_date'),'MW'),
                   tags$small(class = 'text-muted text-centre text-center','Pre-planning Pipeline'),
                   
                   tags$div(class = 'h4 text-black text-centre text-center','up to end 2030')
                 )
                 
             )
         )
         
    ),
    
    # Footer section
    div(class = "container",
        tags$footer(class = "d-flex flex-wrap justify-content-between align-items-center py-3 my-4 border-top",
                    p(class = "col-md-4 mb-0 text-body-secondary", "© 80% by 2030"),
                    tags$a(href = "#top", class = "col-md-4 d-flex align-items-center justify-content-center mb-3 mb-md-0 me-md-auto link-body-emphasis text-decoration-none",
                           #icon("bolt", style = "font-size: 32px; color: #2196F3;"),
                           img(src = 'static/DfE.jpeg', height = "29", class = "ms-2")
                    ),
                    tags$ul(class = "nav col-md-4 justify-content-end",
                            tags$li(class = "nav-item",
                                    tags$a(href = "#parameters", class = "nav-link px-2 text-body-secondary", "NI")
                            ),
                            tags$li(class = "nav-item",
                                    tags$a(href = "#capacity", class = "nav-link px-2 text-body-secondary", "Capacity")
                            ),
                            tags$li(class = "nav-item",
                                    tags$a(href = "#progression", class = "nav-link px-2 text-body-secondary", "Progression")
                            ),
                            tags$li(class = "nav-item",
                                    tags$a(href = "#timelines", class = "nav-link px-2 text-body-secondary", "Duration")
                            )#,
                            # tags$li(class = "nav-item",
                            #   tags$a(href = "#dashboard", class = "nav-link px-2 text-body-secondary", "Dashboard")
                            # )
                    )
        )
    )
    ),
  
  # Tab 2: Energy Flow Diagram (original app1.R content)
  tabPanel("Energy Flow Diagram",
    icon = icon("diagram-project"),
    value = "energy_flow_tab",
    uiOutput("main_ui")
  )
)

#server
server <- function(input, output, session) {
  
  # Suppress client-side error notifications
  options(shiny.sanitize.errors = FALSE)
  
  # ==== Energy Flow Diagram Logic (original app1.R) ====
  state <- reactiveValues(
    view = "home",            
    selected_page = NULL,
    renewables_run = NULL
  )
  
  output$main_ui <- renderUI({
    if (state$view == "home") {
      diagram_ui("diagram")  
    } else {
      tagList(
        tags$nav(class = "navbar navbar-expand-lg glass-card rounded-0 p-2 sticky-top", #  fixed-top bg-white
                 `data-bs-theme` = "light",
                 div(class = "container-fluid d-inline",
                     actionButton("go_back", "← Back to Diagram"), #br(),br(),
                     tags$a(class = "ms-3 ps-3 navbar-brand fw-bold d-inline", href = "#top",
                            paste(state$selected_page, 'Targets, Improvement and Projections'),#br(),
                            p(class = 'lead d-inline',"80% by 2030")
                     )
                 )
            ),
        switch(
          state$selected_page,
          "Generation"   = generation_ui("generation"),
          "Transmission" = transmission_ui("transmission"),
          "Distribution" = distribution_ui("distribution"),
          "Supply"       = supply_ui("supply"),
          div(h2("Unknown page"), p("No content defined."))
        )
      )
    }
  })
  
  diagram_server("diagram", state = state)
  generation_server("generation", state = state)
  transmission_server("transmission", state = state)
  distribution_server("distribution", state = state)
  supply_server("supply", state = state)
  
  observeEvent(input$go_back, {
    state$view <- "home"
    state$selected_page <- NULL
  })
  
  observeEvent(input$diagram_click, {
    req(input$diagram_click)
    valid_tabs <- c("Generation", "Transmission", "Distribution", "Supply")
    if (input$diagram_click %in% valid_tabs) {
      state$selected_page <- input$diagram_click
      state$view <- "page"
    }
  })
  
  # ==== Renewable Simulation Logic (from app.R) ====
  # Observer to switch tabs
  observeEvent(input$switch_to_flow, {
    updateNavbarPage(session, "main_nav", selected = "energy_flow_tab")
  })
  
  # Placeholder for renewable simulation logic
  # To complete: Copy all server logic from app.R (lines 734-1637) here
  # Full Renewable Simulation Server Logic from app.R

# server <- function(input, output, session) {
  
  #Offshore ----
  
  # observe({ cat("Selected month:", input$offshore_start, "\n")})
  # observeEvent(input$offshore_option_selected, { print(input$offshore_option_selected) } )
  # observeEvent(input$offshore_capacity, { print(input$offshore_capacity) } )
  # 
  # 
  # observeEvent(input$project_progression_selected, { print(input$project_progression_selected) } )
  # observeEvent(input$preplanning_approach, { print(input$preplanning_approach) } )
  # observeEvent(input$progression_prob_method_selected, { print(input$progression_prob_method_selected) } )
  observe({ 
    cat(input$optimistic)
    cat(input$conservative)
    cat(input$survey)
    })
  
  # Reactive values to store all input parameters
  params <- reactiveValues(
    # Energy range parameters
    baseline_renewables = 3961,
    # Demand and capacity parameters  
    demand_2030 = 9,
    number_runs = 100,
    capacity_factor = 22,
    
    # Project progression probabilities (%)
    planning_connection_prob = 77,
    connection_construction_prob = 78,
    connection_completion_prob = 100,
    # Offshore capacity (GW)
    # Solar timeline parameters (months)
    planning_connection_time = 8*4,
    connection_construction_time = 17*4,
    construction_completion_time = 4*4,
    
    # Wind timeline parameters (months)
    planning_connection_time_wind = 29*4,
    connection_construction_time_wind = 45*4,
    construction_completion_time_wind = 12*4,
    
    # User choices
    # preplanning_approach = switch(input$optimistic,input$conservative,input$survey) # optimistic, conservative, survey
    time_approach = "empirical", # custom, empirical
    progression_prob_method = "empirical", # custom, empirical
    offshore_option_selected = "offshore_exclude",
    offshore_start = "2032-01-01",
    offshore_capacity = 0.5
    
  )
  
  # Debounced parameter collection (waits 500ms after last change)
  params_debounced <- reactive({
    print(input$progression_prob_method_selected)
    list(
      baseline_renewables = getSliderValue(input$baseline_renewable, 3961),
      demand_2030 = getSliderValue(input$demand_2030, 9),
      
      capacity_factor = input$capacity_factor,
      number_runs = getSliderValue(input$number_runs, 30),
      
      planning_connection_prob = getSliderValue(input$planning_connection_prob, 71),
      connection_construction_prob = getSliderValue(input$connection_construction_prob, 85),
      connection_completion_prob = getSliderValue(input$connection_completion_prob, 100),
      
      planning_connection_time = getSliderValue(input$planning_connection_time, 33),
      connection_construction_time = getSliderValue(input$connection_construction_time, 70),
      construction_completion_time = getSliderValue(input$construction_completion_time, 4),
      
      planning_connection_time_wind = getSliderValue(input$planning_connection_time_wind, 29),
      connection_construction_time_wind = getSliderValue(input$connection_construction_time_wind, 45),
      construction_completion_time_wind = getSliderValue(input$construction_completion_time_wind, 12),
      
      preplanning_approach = if (!is.null(input$preplanning_approach)) input$preplanning_approach else "conservative",
      time_approach = if (!is.null(input$project_progression_selected)) input$project_progression_selected else "empirical",
      progression_prob_method = if (!is.null(input$progression_prob_method_selected)) input$progression_prob_method_selected else "empirical",
      
      offshore_included = if (!is.null(input$offshore_option_selected)) (input$offshore_option_selected == "offshore_include") else FALSE,
      offshore_start = if (is.null(input$offshore_start)) '2032-01-01' else paste0(input$offshore_start,'-01'),
      offshore_capacity = getSliderValue(input$offshore_capacity, 0.5)
      
      )
  }) %>% 
    debounce(500)  # Wait 500ms after last input change
  
  # Update stored params only when debounced params change
  observe({
    new_params <- params_debounced()
    for (name in names(new_params)) {
      params[[name]] <- new_params[[name]]
    }
    
    # Debug output (less frequent now)
    cat("Parameters updated via debounced reactive:\n")
    cat("Number of Runs:", params$number_runs, "\n")
    cat("Estimation Approach:", params$preplanning_approach, "\n")
    cat("---\n")
  })
  
 
  # Specific listener for capacity factor numeric input
  observeEvent(input$capacity_factor, {
    if (!is.null(input$capacity_factor) && !is.na(input$capacity_factor)) {
      # Update the reactive values
      params$capacity_factor <- as.numeric(input$capacity_factor)
      
      # Log the change
      cat("Capacity Factor updated via numeric input:", params$capacity_factor, "%\n")
      
      # Optional: Add validation
      if (params$capacity_factor < 10 || params$capacity_factor > 50) {
        showNotification(
          paste("Capacity factor of", params$capacity_factor, "% seems unusual. Typical values are 15-40%."),
          type = "warning",
          duration = 3
        )
      }
    }
  }, ignoreInit = TRUE) # Don't trigger on app start
  
  # Radio button listeners for estimation approach
  observeEvent(input$preplanning_approach, {
    
      # params$preplanning_approach <- "optimistic"
      showNotification(
        paste("Estimation approach set to", input$preplanning_approach," "),
        type = "default",
        duration = 3
      )
      #cat("Estimation approach changed to: Optimistic\n")
    #}
  }, ignoreInit = TRUE)
  
 
  
  # Add a text output for real-time parameter monitoring (for debugging)
  output$parameterStatus <- renderText({
    paste0("Server-Side Values: ",
           "Nrun: ", params$number_runs, " |",
           'Base Renewable', params$baseline_renewable, " TWh | ",
           "Demand: ", params$demand_2030, " TWh/yr | ",
           "CF: ", params$capacity_factor, "% | ",
           "Approach: ", params$preplanning_approach, " | ",
           "Timeline: ", params$time_approach, " | ",
           "Risk weighting: ", params$progression_prob_method, " | ",
           "Offshore: ", ifelse(params$offshore_included, "Yes", "No"), " | ",
           ifelse(!is.null(params$offshore_included), 
                  paste("Capacity: ",params$offshore_capacity,"|"), "")
           )
  })
  
  # Comprehensive server-side values table
   output$serverValuesTable <- renderTable({

    data.frame(
      Parameter = c(
        "2030 Demand", "Capacity Factor", "Number of Runs",
        "Planning → Connection Prob", "Connection → Construction Prob", "Connection → Completion Prob",
        "Solar: Planning → Connection", "Solar: Connection → Construction", "Solar: Construction → Completion",
        "Wind: Planning → Connection", "Wind: Connection → Construction", "Wind: Construction → Completion",
        "Estimation Approach", "Timeline Approach", "Progression Prob Method", "Offshore Included"#,

      ),
      Current_Value = c(

        paste0(params$demand_2030, " TWh/yr"),
        paste0(params$capacity_factor, "%"),
        as.character(params$number_runs),
        paste0(params$planning_connection_prob, "%"),
        paste0(params$connection_construction_prob, "%"),
        paste0(params$connection_completion_prob, "%"),
        paste0(params$planning_connection_time, " months"),
        paste0(params$connection_construction_time, " months"),
        paste0(params$construction_completion_time, " months"),
        paste0(params$planning_connection_time_wind, " months"),
        paste0(params$connection_construction_time_wind, " months"),
        paste0(params$construction_completion_time_wind, " months"),
        params$preplanning_approach,
        params$time_approach,
        params$progression_prob_method,
        paste0(ifelse(params$offshore_included, "Yes", "No"),' ')#,
 
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Real-time capacity factor display
  output$generationCapacityConversion <- renderText({

  paste0('1 TWhr = ',round(1/(8.765*as.numeric(params$capacity_factor)/100)*1000)/1000,' GW installed capacity')
  })

  output$capacityFactorValue <- renderText({
    as.character(params$capacity_factor)
    # as.character(params$capacity_factor)
  })

  transition_probs <- reactiveVal({
    transition_probs_empirical()
    
  })
  
  
  observe({
    print(params$progression_prob_method)
        df <- switch( 
          params$progression_prob_method,
                'empirical' =  transition_probs_empirical(),
                'progression_prob_custom' = tribble(
        ~from, ~to, ~prob,
        'Planning', 'Connection', params$planning_connection_prob /100,
        'Connection', 'Construction', params$connection_construction_prob /100,
        'Construction', 'Completed', params$connection_completion_prob /100
      )
        )
        print('df')
        print(df)
      transition_probs(df)
    })
  
  # stage_duration reactive ----
  stage_duration <- reactiveVal({
    stage_duration_empirical()
  })
  
  observe({
      df <- switch( 
      params$time_approach,
        'empirical' =  stage_duration_empirical(),
        'custom' = tribble(
      ~from, ~to,~tech, ~wks,
      'Planning', 'Connection','Solar Photovoltaic', params$planning_connection_time * 4 ,
      'Connection', 'Construction','Solar Photovoltaic', params$connection_construction_time * 4 ,
      'Construction', 'Completed','Solar Photovoltaic', params$construction_completion_time * 4 ,
      'Planning', 'Connection','Wind Onshore', params$planning_connection_time_wind * 4  ,
      'Connection', 'Construction','Wind Onshore', params$connection_construction_time_wind * 4 ,
      'Construction', 'Completed','Wind Onshore', params$construction_completion_time_wind * 4 
        )
      )
    
      print(df)
      stage_duration(df)
      
  })
    
observeEvent(input$take_state, { 

    state$renewables_take_state = date()
    state$selected_page <- "Generation"
    state$view <- "page"
    
    # Navigate to Energy Flow Diagram tab
    updateNavbarPage(session, "main_nav", selected = "energy_flow_tab")

})

  observeEvent(input$submit, { 

    state$renewables_run = date()
    print('Start Run1')
    
      session$sendCustomMessage("toggleLoadingBtn", "show")
      
    projections_projects <-  switch(EXPR = params$preplanning_approach,
                                    'conservative' = projections_projects_conservative,
                                    'optimistic' = projections_projects_optimistic,
                                    'survey' = projections_projects_survey
    )
    
    shiny_forecast <- function(){
      projections_projects |> 
        filter(type == 'forecast') |> 
        filter(Date > '2024-12-01') |> 
        filter(Date< end_date) |> 
        select(tech, ,Date, date, n ) |> 
        mutate(lower = floor(n),
               upper = ceiling(n),
               remainder = n-lower) |> 
        rowwise() |> 
        mutate(sample_deterministic = 
                 sample(c(lower, upper), 
                        size = 1, 
                        prob = c(1-remainder, remainder)))
    }
    
    forward_projects <- data.frame(); 
    for(j in 1:params$number_runs){
      print(j)
      
      for( i in 1:nrow(shiny_forecast())){
        print(i)
        n <- t(shiny_forecast())[,i]
        # print(n)
        nn <- lift_historic_projects(pipeline,
                                     n['tech'],
                                     n[['Date']],
                                     n=as.numeric(n['sample_deterministic']))
        # print(nn)
        
        nn$run = j
          
        forward_projects <- rbind(forward_projects, nn)
        
      }
    }
    
    print('forward projects')
    print(forward_projects)
    
    forward_projects <- forward_projects |> 
      mutate(broad_status = 'Planning') |> 
      mutate(broad_status_start = 'Planning') |> 
      mutate(Date = as.Date(Date))

    print(head(forward_projects))
    print(class(forward_projects))
    
    print(transition_probs()$prob [transition_probs()$from == unique(forward_projects$broad_status)])
    
    forward_projects <- forward_projects |> 
      rowwise() |> 
      mutate(passed_planning = sample(c(T,F),
                                      replace = T,
                                      prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                               1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                      size = 1)) |>
      
      mutate(
        passed_planning_time = (get_empirical_time(pipeline,tech,broad_status)),
        passed_planning_time_wk = as.numeric(passed_planning_time, units = 'weeks' ) ,#(60*60*24*7),
        passed_planning_date = Date + passed_planning_time) |>
      mutate(broad_status = 'Connection') |> 
      
      mutate(passed_connection = sample(c(T,F),
                                        replace = T,
                                        prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                                 1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                        size = 1)) |> 
      mutate(
        passed_connection_time = (get_empirical_time(pipeline,tech,broad_status)),
        passed_connection_time_wk = as.numeric(passed_connection_time, units = 'weeks' ) ,#(60*60*24*7),
        passed_connection_date = passed_planning_date + passed_connection_time
      ) |> 
      mutate(broad_status = 'Construction') |> 
      mutate(passed_construction = sample(c(T,F),
                                          replace = T,
                                          prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                                   1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                          size = 1)) |> 
      mutate(
        passed_construction_time = (get_empirical_time(pipeline,tech,broad_status)),
        passed_construction_time_wk = as.numeric(passed_construction_time, units = 'weeks' ) ,#(60*60*24*7),
        passed_construction_date = passed_connection_date + passed_construction_time) |> 
      
      mutate(broad_status = 'Completed')
    
    # print('forward_projects')
    # print(forward_projects)
    write.csv(forward_projects,'forward_projects.csv')
    state$forward_projects <- forward_projects
    
    x <- forward_projects |> 
      filter(passed_planning &
               passed_connection &
               passed_construction) |> 
      mutate(finished = #format(passed_construction_date, format = '%Y-%m')) |> 
               floor_date( passed_construction_date,'month')) |> 
      group_by(finished ,
               run) |> 
      summarise(MW = sum(`Installed Capacity (MWelec)`,na.rm = T),
                no_proj = n()) |> 
      ungroup()
    
    # print('x')
    # print(x)
    
    
    # yrs = unique(floor_date(seq( from = as.Date('2025-01-01'), 
    #                              to = as.Date('2031-01-01'),
    #                              by =1 ),'year'))
    # qtr = unique(floor_date(seq( from = as.Date('2025-01-01'), 
    #                              to = as.Date('2031-01-01'),
    #                              by =1 ),'quarter'))
    
    
    mons = unique(floor_date(seq( from = as.Date('2025-01-01'), 
                                  to = as.Date(end_date),
                                  by = 1 ), 'month')
    )
    
    # wks = unique(floor_date(seq( from = as.Date('2025-01-01'), 
    #                        to = as.Date('2031-01-01'),
    #                        by =1 ),'week'))
    
    
    
    forward_projects_outcome_first(
      left_join(
      expand.grid(
        finished = mons,
        run = 1:params$number_runs),
      x
    ) |> 
      replace_na(replace = list(MW = 0, no_proj = 0)) |> 
      arrange(finished)
    

    )
    
    session$sendCustomMessage("toggleLoadingBtn", "hide")
    
    
  })
  
  #Current Projects ---- 
  observeEvent(input$submit, { 
    
    print('Start Run')
    
    print(transition_probs())
    
    print(stage_duration() )
    
    current_projects <- pipeline  |> 
      filter(broad_status %in% c('Planning','Connection','Construction')) |> 
      # filter(`Development Status (short)` != 'Revised')|> 
      select(
        `Development Status (short)`,
        `Installed Capacity (MWelec)`,
        'Ref ID',
        Planning,
        Connection,
        Construction,
        'date' = `Planning Application Submitted`,
        passed_planning_date = `Planning Permission Granted`,
        `Planning Permission Granted`,
        passed_connection_date = `Under Construction`,
        passed_construction_date = `Operational`,
        
        'Date' = `Planning Application Submitted`,
        'tech',
        broad_status)
    
    
    splitting_criteria <- current_projects$broad_status
    
    list_statuses_original <- current_projects %>%
      mutate(broad_status_start = broad_status) %>%
      mutate(      passed_planning = T,
                   passed_planning_time = Planning,       
                   passed_planning_time_wk = as.numeric(Planning,unit = 'weeks'),#(60*60*24*7),    
                   #passed_planning_date = NA,    
                   
                   passed_connection = T,          
                   passed_connection_time = Connection,     
                   passed_connection_time_wk = as.numeric(Connection,unit = 'weeks'),
                   #passed_connection_date = NA,
                   
                   passed_construction = T,
                   passed_construction_time = Construction,
                   passed_construction_time_wk = as.numeric(Construction,unit = 'weeks'),
                   # passed_construction_date = NA
                   
      ) |>
      
      split(f = splitting_criteria) 
    
 
    current_projects_projected_forward <- data.frame()
    
    for (j in 1:params$number_runs){
      print(j)
      list_statuses <- list_statuses_original
      
      while( sum(
        sapply(
          list_statuses, 
          function(pipeline){
            sum(pipeline$broad_status!='Completed')})) != 0) {
  
        for (i in 1:length(list_statuses)){
          
          print(i)
          #print(list_statuses[[i]])
          switch(
            unique(list_statuses[[i]]$broad_status),
            Planning = {list_statuses[[i]] <- 
              list_statuses[[i]] |> 
              rowwise() |>
              mutate(passed_planning = sample(c(T,F),
                                              replace = T,
                                              prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                                       1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                              size = 1)) |>
              mutate(
                passed_planning_time = (get_empirical_time(pipeline,tech,broad_status)),
                passed_planning_time_wk = as.numeric(passed_planning_time, units = 'weeks' ) ,#(60*60*24*7),
                passed_planning_date = Date + passed_planning_time) |> 
              
              mutate(broad_status = 'Connection') |> ungroup()
            },
            
            Connection = { 
              
              # print(transition_probs()$prob )
              # print(transition_probs()$from == broad_status)
              # print(transition_probs()$prob[transition_probs()$from == broad_status])
              
              print(unique(list_statuses[[i]]$broad_status))
              print(transition_probs())
              
              list_statuses[[i]] <- list_statuses[[i]] |> 

              rowwise() |>
              mutate(passed_connection = ifelse(runif(n = 1) < (transition_probs()$prob [transition_probs()$from == broad_status]),
                                                T,
                                                F)
                     
              ) |> 
              
              mutate(
                passed_connection_time = (get_empirical_time(pipeline,tech,broad_status)),
                passed_connection_time_wk = as.numeric(passed_connection_time, units = 'weeks' ) , #(60*60*24*7),
                passed_connection_date = passed_planning_date + passed_connection_time) |> 
              
              mutate(broad_status = 'Construction') |> ungroup()
            },
            
            Construction = {
              list_statuses[[i]] <- list_statuses[[i]] |> 
                
                rowwise() |> 
                mutate(passed_construction = sample(c(T,F),
                                                    replace = T,
                                                    prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                                             1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                                    size = 1)) |> 
                
                mutate(
                  passed_construction_time = (get_empirical_time(pipeline,tech,broad_status)),
                  passed_construction_time_wk = as.numeric(passed_construction_time, units = 'weeks' ) ,#(60*60*24*7),
                  passed_construction_date = passed_connection_date + passed_construction_time) |> 
                
                mutate(broad_status = 'Completed') |> ungroup()
            }
          )
        }
        
      }
      
      current_projects_projected_forward <- rbind(current_projects_projected_forward,
                                                  reduce(list_statuses,.f = rbind) |> mutate(run = j)
      )
    }
    
    print('current_projects_projected_forward')
    print(current_projects_projected_forward)
    write.csv(current_projects_projected_forward,'current_projects_projected_forward.csv')
    state$current_projects_projected_forward <- current_projects_projected_forward
    
    x <- current_projects_projected_forward |> 
      filter(passed_planning &
               passed_connection &
               passed_construction) |> 
      mutate(MW = `Installed Capacity (MWelec)`,
             yr = year(passed_construction_date),
             mn = month(passed_construction_date)
      ) |>  mutate(finished = #format(passed_construction_date, format = '%Y-%m')) |> 
                     floor_date( passed_construction_date,'month')) |> 
      group_by(finished,
               run) |> 
      summarise(MW = sum(`MW`) ) 
    
    # Monthly SUM
    
      expand.grid(yr = 2012:year(end_date),
                  mn = 1:12,
                  run = 1:params$number_runs ) |> 
      mutate(finished = as.Date(paste0(yr, ifelse(mn < 10, paste0('-0', mn,'-01'), paste0('-',mn,'-01'))))) |> 
      left_join(x) |> 
      replace_na(list(MW=0)) |> 
    current_projects_outcome_first()
    
  })
  
  forward_projects_outcome <- reactiveVal({forward_projects_outcome})
  current_projects_outcome <- reactiveVal({current_projects_outcome})
  forward_projects_outcome_cumulative <- reactiveVal({forward_projects_outcome_cumulative})
  current_projects_outcome_cumulative <- reactiveVal({current_projects_outcome_cumulative})
  forward_projects_outcome_first <- reactiveVal({forward_projects_outcome_first})
  current_projects_outcome_first <- reactiveVal({current_projects_outcome_first})
  
  offshore_wind_df <- reactive({ 
    print(params$offshore_included)
    
    print(params$offshore_start)
    
    offshore_wind_data <- data.frame(finished = mons) |> 
                mutate(mean = ifelse(finished > params$offshore_start, 
                                     params$offshore_capacity/12, 
                                     0)
                ) |> 
                mutate(mean = mean*params$offshore_included )
    
    write.csv(x = offshore_wind_data, file = 'offshore_wind.csv')
    state$offshore_wind <- offshore_wind_data
    
    data.frame(finished = mons) |> 
      mutate(mean = ifelse(finished > params$offshore_start, 
                           params$offshore_capacity/12, 
                           0)
      ) |> 
      mutate(mean = mean*params$offshore_included )
    
  })
  
  observeEvent(current_projects_outcome_first(), {
    conf_level <- 0.95
    alpha      <- 1 - conf_level
    
    current_projects_outcome( current_projects_outcome_first() |> 
      group_by(finished) |> 
      summarise(
        mean   = mean(MW, na.rm = TRUE,trim=0.05),
        median = median(MW, na.rm = TRUE),
        sd     = sd(MW, na.rm = TRUE),
        n      = params$number_runs,#dplyr::n(),
        se     = sd / sqrt(n),
        # CLT / t-interval
        
        u=mean+2 * se,
        l=mean-2 * se,
        lwr_norm = mean + qt(alpha/2, df = n - 1) * se,
        upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
        # Bootstrap percentile CI
        lwr_pct  = quantile(MW, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
        upr_pct  = quantile(MW, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE),
        .groups  = "drop"
        
      ) 
    )
    
    
    
    current_projects_outcome_cumulative(
      
      current_projects_outcome_first() |> 
      group_by(finished,
               run) |> 
      summarise(MW = cumsum(`MW`) ) |>
      group_by(run) |> 
      arrange((finished)) |> 
      # count(passed_construction_date,tech,wt = MW) |> 
      mutate(cs = cumsum(MW)) |> 
      group_by(finished) |> 
      summarise(
        mean   = mean(cs, na.rm = TRUE,trim=0.05),
        median = median(cs, na.rm = TRUE),
        sd     = sd(cs, na.rm = TRUE),
        n      = params$number_runs,#dplyr::n(),
        se     = sd / sqrt(n),
        # CLT / t-interval
        
        u=mean+2*se,
        l=mean-2*se,
        lwr_norm = mean + qt(alpha/2, df = n - 1) * se,
        upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
        # Bootstrap percentile CI
        lwr_pct  = quantile(cs, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
        upr_pct  = quantile(cs, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE),
        .groups  = "drop"
        
      )
    )
  })

    observeEvent(forward_projects_outcome_first(), {
      
      # write.csv(forward_projects_outcome_first(),'forward_projects_outcome_first.csv')
      
      conf_level <- 0.95
      alpha      <- 1 - conf_level
      
      forward_projects_outcome_first() |>  
      group_by(finished) |> 
      summarise(
        mean   = mean(MW, na.rm = TRUE),
        median = median(MW, na.rm = TRUE),
        sd     = sd(MW, na.rm = TRUE),
        
        # avg_projects = mean(),
        n      = dplyr::n(), #nrun,#
        se     = sd / sqrt(n),
        # CLT / t-interval
        
        u = mean+2*se,
        l = mean-2*se,
        
        lwr_norm = max(0,mean + qt(alpha/2, df = n - 1) * se),
        upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
        # Bootstrap percentile CI
        
        lwr_pct  = quantile(MW, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
        upr_pct  = quantile(MW, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE),
        
        .groups  = "drop"
      ) |> 
    forward_projects_outcome( )#update reactiveVal
    
    forward_projects_outcome_cumulative(
      forward_projects_outcome_first() |> 
      group_by(run) |>
      arrange(finished) |> 
      mutate(cs = cumsum(MW)) |> 
      group_by(finished) |> 
      summarise(mean = mean(cs),
                median = median(cs, na.rm = TRUE),
                sd     = sd(cs, na.rm = TRUE),
                
                # avg_projects = mean(),
                n      = params$number_runs, #dplyr::n(), #dplyr::n(), #,#
                se     = sd / sqrt(n),
                
                # CLT / t-interval
                u = mean + 2 * se,
                l = mean - 2 * se,
                lwr_norm = mean - qt(alpha/2, df = n - 1) * se,
                upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
                
                # Bootstrap percentile CI
                lwr_pct  = quantile(cs, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE)  ,
                upr_pct  = quantile(cs, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE)  ,
                
                .groups  = "drop") #|>
      
    )
    
  })

    output$current_cumulative <- renderEcharts4r({
      
      # Check if simulation results are available
      #req(simulation_results$current_projects_outcome_cumulative)
      
      current_cumulative_echart(current_projects_outcome_cumulative())
      
    })
    
    output$current_yearly <- renderEcharts4r({
      
      #req(simulation_results$current_projects_outcome)
      
      current_yearly_echart(current_projects_outcome())
      
    })
    
    output$preplanning_cumulative_echarts <- renderEcharts4r({
      
      #req(simulation_results$forward_projects_outcome_cumulative)
      
      print('hello')
      
      preplanning_cumulative_echart(forward_projects_outcome_cumulative())
      
    })
    
    output$preplanning_yearly <- renderEcharts4r({
      #req(simulation_results$forward_projects_outcome)
       # Sys.sleep(5)
      preplanning_yearly_echart(forward_projects_outcome() )
      
      })
    
    output$offshore_wind_plot <- renderEcharts4r({
      
      offshore_wind_plot( 
        paste0(params$offshore_start,'-01'), 
        params$offshore_capacity/12,
        offshore_included = params$offshore_included) 
        
  
})

observe({
 print('current_projects_outcome')
  print(current_projects_outcome())
  current_projects_outcome_target_date <- current_projects_outcome() |>
    filter(year(finished) == year(target_date)) |>
    pull(mean) |> sum()

  forward_projects_outcome_target_date <- forward_projects_outcome() |>
    filter(year(finished) == year(target_date)) |>
    pull(mean) |> sum()

  current_projects_outcome_cumulative_target_date <- current_projects_outcome_cumulative() |>
    filter(finished == target_date) |>
    pull(mean)

  forward_projects_outcome_cumulative_target_date <- forward_projects_outcome_cumulative()|>
    filter(finished == target_date) |>
    pull(mean)

  # By Target Date ----

  current_projects_outcome_end_date <- current_projects_outcome() |>
    filter(year(finished) == year(max(finished))) |>
    pull(mean)|> sum()

  forward_projects_outcome_end_date <- forward_projects_outcome() |>
    filter(year(finished) == year(max(finished))) |>
    pull(mean)|> sum()

  current_projects_outcome_cumulative_end_date <- current_projects_outcome_cumulative() |>
    filter(finished == max(finished)) |>
    pull(mean)

  forward_projects_outcome_cumulative_end_date <- forward_projects_outcome_cumulative() |>
    filter(finished == max(finished)) |>
    pull(mean)

  
  offshore_wind_target_date <- offshore_wind_df() |>
    filter(year(finished) == year(target_date)) |>
    pull(mean)|>
      sum() |>
      magrittr::multiply_by(1000*12)

  offshore_wind_end_date <- offshore_wind_df()|>
    filter(year(finished) == year(max(finished))) |>
    pull(mean)|> 
    sum() |> 
    magrittr::multiply_by(1000*12)
  
  
  output$offshore_wind_capacity_target_date <- renderText({as.character(round(digits = 3,(offshore_wind_target_date))) })
  output$offshore_wind_capacity_end_date <- renderText({as.character(round(digits = 3,(offshore_wind_end_date))) })
  
  
  output$offshore_wind_capacity <- renderText({
    ifelse( params$offshore_included,
            
    as.character(round(digits = 1,(params$offshore_capacity*1000))),
    0)
    
    })
  output$offshore_wind_start <- renderText({
    
    ifelse( params$offshore_included,
      year( params$offshore_start),
      '----')
      
      })
  
  
  output$new_res_current_yearly_end_date <- renderText({as.character(round(digits = 3,(current_projects_outcome_end_date))) })
  output$new_res_current_cumulative_end_date <- renderText({as.character(round(digits = 0,(current_projects_outcome_cumulative_end_date))) })
  output$new_res_preplanning_yearly_end_date <- renderText({as.character(round(digits = 0,(forward_projects_outcome_end_date))) })
  output$new_res_preplanning_cumulative_end_date <- renderText({as.character(round(digits = 0,(forward_projects_outcome_cumulative_end_date))) })
  
  
  output$new_res_current_yearly_target_date <- renderText({as.character(round(digits = 0,(current_projects_outcome_target_date))) })
  output$new_res_preplanning_yearly_target_date <- renderText({as.character(round(digits = 0,(forward_projects_outcome_target_date))) })
  output$new_res_current_cumulative_target_date <- renderText({as.character(round(digits = 0,(current_projects_outcome_cumulative_target_date))) })
  output$new_res_preplanning_cumulative_target_date <- renderText({as.character(round(digits = 0,(forward_projects_outcome_cumulative_target_date))) })
  
  existing_res_target_date <- existing_res_end_date <- params$baseline_renewables/(8.760 * params$capacity_factor/100)

  TER_target_date  <- TER_end_date <- params$demand_2030/(0.008760 * params$capacity_factor/100)

  total_new_res_target_date = (forward_projects_outcome_cumulative_target_date + 
                                 current_projects_outcome_cumulative_target_date +
                                 offshore_wind_target_date)
  
  cat(forward_projects_outcome_cumulative_target_date,'\n')
  cat(current_projects_outcome_cumulative_target_date,'\n')
  cat(offshore_wind_target_date,'\n')
  cat(total_new_res_target_date ,'\n')
  cat(existing_res_target_date ,'\n') 
  cat(TER_target_date,'\n')
  
  
  output$target_pct_target_date <- renderText({as.character(100*round(digits = 4,
                                                                  (total_new_res_target_date + existing_res_target_date)/TER_target_date)) })
  output$total_new_res_target_date <- renderText({as.character(round(digits = 0,(total_new_res_target_date))) })
  
  target_pct = round((total_new_res_target_date + existing_res_target_date)/TER_target_date)
  
  target_status_words_target_date = ifelse(target_pct >= 0.8, 'on track', ifelse(target_pct >= 0.7, 'slightly behind', 'significantly behind'))
  target_status_color_target_date = ifelse(target_pct >= 0.8, 'green', ifelse(target_pct >= 0.7, 'orange', 'red'))

  output$pct_2030 <- renderText({

  })
    })
       
  
}


shinyApp(ui, server)



