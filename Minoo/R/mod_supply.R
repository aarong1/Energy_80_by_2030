
supply_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Supply"),
    div(id = ns("content"),
        p("Supply page content goes here.")
    )
  )
}

supply_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Add logic, renderers, etc.
  })
}
