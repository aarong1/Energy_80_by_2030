
diagram_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "centered-content", 
        h2("Overview of Electricity Industry"),
        p("For more details about each part, please click on it.",
          style = "margin-bottom: 50px;"),
        # Host for the injected SVG
        uiOutput(ns("diagram"))
    )
  )
}

diagram_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Inject the SVG inline so the JS can see the <a> elements
    output$diagram <- renderUI({
      svg_txt <- paste(readLines("www/test1.drawio.svg", warn = FALSE), collapse = "\n")
      HTML(svg_txt)
    })
    
    # If you ever namespace the JS input (e.g., 'diagram-diagram_click'),
    # change input$diagram_click to input[[ns("diagram_click")]].
  })
}
