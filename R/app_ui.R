#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @importFrom shinythemes shinytheme
#' @importFrom utils getFromNamespace
#' @noRd
app_ui <- function(request) {
  sess <- new.env()
  connect(sess)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    w3css::w3_page(
      getFromNamespace("bootstrapLib", "shiny")(),
      htmlTemplate(
        app_sys("app/www/template.html"),
        translate = HTML(build_language_json(session = sess)),
        welcomemodal = modal(
          inputId = "welcome",
          title = tagList(
            tags$h2("Welcome to DiadES Atlas!") %>% with_i18("welcome")
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
        header = htmlTemplate(
          app_sys("app/www/header.html")
        ),
        menu = menu(
          # tags$div(
          #   class = "w3-col",
          #   # style = "width:8%",
          #   HTML('<div id = "language_selector">
          #         <!--<label for="lg"><span data-i18n="select-language">Choose a language:</span></label>-->
          #           <select name="pets" id="lg">
          #               <option value="en">\U0001f1ec\U0001f1e7 English</option>
          #               <option value="fr">\U0001f1eb\U0001f1f7 French</option>
          #               <option value="es">\U0001f1ea\U0001f1f8 Spanish</option>
          #               <option value="pt">\U0001f1f5\U0001f1f9 Portuguese</option>
          #           </select>
          #         </div>')
          # ),
          menuItem(
            "a", "Sit",
            i18n = "nav-catch"
          ),
          menuItem(
            "b", "Lorem",
            i18n = "nav-present"
          ),
          menuItem(
            "c", "Ipsum",
            i18n = "nav-climate"
          ),
          menuItem(
            "d", "Dolor",
            i18n = "nav-future"
          )
        ),
        content = tabItems(
          tabItem(
            "a",
            mod_first_ui("first_ui_1")
          ),
          tabItem(
            "b",
            # htmlTemplate(
            #   app_sys("app/www/main.html"),
            #   geojsonFeature = glue::glue_collapse(readLines(app_sys("casestudy.json"))),
            #   species = glue::glue_collapse(readLines(app_sys("species.json"))),
            #   services = glue::glue_collapse(readLines(app_sys("services.json"))),
            #   ecosystems = glue::glue_collapse(readLines(app_sys("ecosystems.json")))
            # )
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
        footer = htmlTemplate(
          app_sys("app/www/footer.html"),
          # select dbversion_date, dbversion_number from diadesatlas.dbversion order by dbversion_id desc limit 1;
          db_version = try(
            dplyr::tbl(get_con(sess), "dbversion") %>% 
              dplyr::collect() %>% 
              dplyr::arrange(dplyr::desc(dbversion_id)) %>% 
              dplyr::slice_head(n = 1) %>% 
              dplyr::pull(dbversion_number),
            silent = TRUE),
          # db_version = 1,
          app_version = try(as.character(desc::desc_get_version()), silent = TRUE)
          # app_version = 2
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
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "diades.atlas",
      head = readLines(app_sys("app/www/ganalytics.html"))
    )#,
    # htmltools::htmlDependency(
    #   path = app_sys("app/www"),
    #   head = readLines(app_sys("app/www/ganalytics.html")),
    #   version = "0.0.1",
    #   name = "ganalytics"
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}