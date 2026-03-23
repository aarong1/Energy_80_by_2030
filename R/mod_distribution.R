
distribution_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Distribution"),
    div(id = ns("content"),
        p("Distribution page content goes here.")
    )
  )
}

distribution_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Add logic, renderers, etc.
  })
}




