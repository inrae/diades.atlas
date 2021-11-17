#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- reactiveValues(
    lg = "en"
  )

  observeEvent(
    input$i18n,
    {
      print(input$i18n)
      r$lg <- input$i18n
    }
  )

  connect(session)

  mod_first_server("first_ui_1", r = r)
  mod_second_server("second_ui_1", r = r)
  mod_third_server("third_ui_1", r = r)
  mod_fourth_server("fourth_ui_1", r = r)
}