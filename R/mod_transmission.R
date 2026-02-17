
transmission_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Transmission"),
    div(id = ns("content"),
        p("Transmission page content goes here.")
    )
  )
}

transmission_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Add logic, renderers, etc.
  })
}









