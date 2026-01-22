#load modules
source("preprocess/params.R")
source("preprocess/functions.R")
source("R/mod_diagram.R")
source("R/mod_generation.R")
source("R/mod_transmission.R")
source("R/mod_distribution.R")
source("R/mod_supply.R")

#ui
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
  document.addEventListener('click', function(ev) {
    // Use the namespaced id produced by the module: diagram-diagram
    var host = document.getElementById('diagram-diagram'); // <-- updated
    var anchor = ev.target.closest && ev.target.closest('a');
    if (!host || !anchor || !host.contains(anchor)) return;

    ev.preventDefault(); // stop navigation to draw.io
    ev.stopPropagation();

    var href = anchor.getAttribute('xlink:href') || anchor.getAttribute('href') || '';
    var idx = href.indexOf('#');
    var frag = (idx >= 0) ? href.substring(idx + 1) : '';
    if (frag) Shiny.setInputValue('diagram_click', frag, {priority: 'event'});
  }, true);
"))
    ,
    tags$style(HTML("
      .back-bar { position: sticky; top: 0; z-index: 10; background: #fff; padding: 8px 0; }
      .centered-content { text-align: center; }
      #diagram svg { display: block; margin: 0 auto; max-width: 90%; height: auto; }
    "))
  ),
  
  uiOutput("main_ui")
)

#server
server <- function(input, output, session) {
  state <- reactiveValues(
    view = "home",            
    selected_page = NULL    
  )
  
  output$main_ui <- renderUI({
    if (state$view == "home") {
      diagram_ui("diagram")  
    } else {
      tagList(
        div(class = "back-bar", actionButton("go_back", "← Back to image")),
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
}

shinyApp(ui, server)

# app <- shinyApp(ui, server)
# shiny::runApp(app, port = 8733, host = "127.0.0.1", launch.browser = TRUE)

