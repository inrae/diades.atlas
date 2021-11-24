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
    w3css::w3_page(
      shiny:::bootstrapLib(),
      htmlTemplate(
        app_sys("app/www/template.html"),
        translate = HTML(build_language_json()),
        welcomemodal = modal(
          inputId = "welcome",
          title = tagList(
            tags$h2("Welcome on DiadES Atlas!") %>% with_i18("welcome")
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
              tags$h3("Explore the consequences of climate change on the distribution of diadromous species and associated ecosystem services to adapt the management of your territories in the long term.") %>% with_i18("explore")
              # tags$h3("We assess and enhance ecosystem services provided by diadromous fishes in a climate change context")
            )
          ),
          color = "teal",
          display = "block"
        ),
        menu = menu(
          tags$div(
            class = "w3-col",
            # style = "width:8%",
            HTML('<div id = "language_selector">
                  <!--<label for="lg"><span data-i18n="select-language">Choose a language:</span></label>-->
                    <select name="pets" id="lg">
                        <option value="en">\U0001f1ec\U0001f1e7 English</option>
                        <option value="fr">\U0001f1eb\U0001f1f7 French</option>
                    </select>
                  </div>')
          ),
          menuItem(
            "d", "Sit",
            i18n = "nav-catch"
          ),
          menuItem(
            "a", "Lorem",
            i18n = "nav-present"
          ),
          menuItem(
            "b", "Ipsum",
            i18n = "nav-climate"
          ),
          menuItem(
            "c", "Dolor",
            i18n = "nav-future"
          )
        ),
        content = tabItems(
          tabItem(
            "d",
            mod_fourth_ui("fourth_ui_1")
          ),
          tabItem(
            "a",
            # htmlTemplate(
            #   app_sys("app/www/main.html"),
            #   geojsonFeature = glue::glue_collapse(readLines(app_sys("casestudy.json"))),
            #   species = glue::glue_collapse(readLines(app_sys("species.json"))),
            #   services = glue::glue_collapse(readLines(app_sys("services.json"))),
            #   ecosystems = glue::glue_collapse(readLines(app_sys("ecosystems.json")))
            # )
            mod_first_ui("first_ui_1")
          ),
          tabItem(
            "b",
            mod_second_ui("second_ui_1")
          ),
          tabItem(
            "c",
            mod_third_ui("third_ui_1")
          )
        ),
        footer = htmlTemplate(
          app_sys("app/www/footer.html")
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "diades.atlas"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
