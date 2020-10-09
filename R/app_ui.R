#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @importFrom shinythemes shinytheme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(
      title = "azti.diades",
      theme = shinythemes::shinytheme("cerulean"),
      tabPanel("Home",
               h1(random_text(nwords = 3)),
               p(random_text(nwords = 250)),
               p(random_text(nwords = 150))
      ),
      tabPanel("Analyse",
               p(random_text(nwords = 10)),
               plotOutput("fake1"),
               plotOutput("fake2"),
      ),
      tabPanel("Synthese",
               p(random_text(nwords = 10)),
               tableOutput("fake3"),
               plotOutput("fake4"),
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'azti.diades'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

