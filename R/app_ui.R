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
    htmlTemplate(
      app_sys("app/www/template.html"),
      translate = build_language_json(),
      welcomemodal = modal(
        inputId = "welcome",
        title = tagList(
          tags$h2("Hello!"),
          tags$p("Welcome to the app")
        ),
        body = tagList(
          tags$div(
            align = "center", 
            tags$img(
              src = "www/diades_vertical.jpg",
              class = "hello-img"
            )
          )
        ),
        footer = tagList(
          tags$div(
            align = "center", 
            tags$h3("We assess and enhance ecosystem services provided by diadromous fishes in a climate change context")
          )
          
        ),
        color = "teal",
        display = "block"
      ),
      menu = menu(
        menuItem(
          "a", "Lorem", i18n = "nav-lorem"
        ),
        menuItem(
          "b", "Ipsum", i18n = "nav-ipsum"
        ),
        menuItem(
          "c", "Dolor", i18n = "nav-dolor"
        ),
        menuItem(
          "d", "Sit", i18n = "nav-sit"
        )
      ), 
      content = tabItems(
        tabItem(
          "a", 
          htmlTemplate(
            app_sys("app/www/main.html"),
            geojsonFeature = glue::glue_collapse(readLines(app_sys("casestudy.json"))), 
            species = glue::glue_collapse(readLines(app_sys("species.json"))),
            services = glue::glue_collapse(readLines(app_sys("services.json"))),
            ecosystems = glue::glue_collapse(readLines(app_sys("ecosystems.json")))
          )
        ),
        tabItem(
          "b", 
          mod_second_ui("second_ui_1")
        ),
        tabItem(
          "c", 
          mod_third_ui("third_ui_1")
        ),
        tabItem(
          "d", 
          mod_fourth_ui("fourth_ui_1")
        )
      ), 
      footer = tagList(
        tags$img(
          src = "www/DiadES.jpg", 
          height="50px"
        ), 
        HTML("<div>&nbsp;</div>"),
        div(
          tags$label(
            `for` = "lg",
            with_i18("Choose a language:", "select-language")
          ),
          tags$select(
            name = "pets",
            id = "lg",
            tags$option(
              value = "en", 
              sprintf("%s English", emo::ji("uk"))
            ),
            tags$option(
              value = "fr",
              sprintf("%s French", emo::ji("france"))
            )
          )
        )
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
      app_title = 'diades.atlas'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

