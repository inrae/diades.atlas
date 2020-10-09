#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  output$fake1 <- renderPlot({
    random_ggplot()
  })
  
  output$fake2 <- renderPlot({
    random_ggplot()
  })
  
  output$fake3 <- renderTable({
    random_table(ncol = 6, nrow = 20)
  })
  
  output$fake4 <- renderPlot({
    random_ggplot()
  })
}
