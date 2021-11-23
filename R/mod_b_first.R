#' first UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_first_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Situation in present time", class = "page_caption") %>% with_i18("title-first"),
    container(
      tagList(
        w3css::w3_quarter(
          tagList(
            mod_species_ui(ns("species_ui_1"), multiple = TRUE)
          )
        ),
        tags$span(
          w3css::w3_quarter(
            tags$span(
              w3_hover_button(
                "Select an Ecosystem Service" %>% with_i18("select-ecosystem"),
                content = htmlTemplate(
                  app_sys("app/www/eco_service.html"),
                  geojsonFeature = glue::glue_collapse(readLines(app_sys("casestudy.json"))),
                  species = glue::glue_collapse(readLines(app_sys("species.json"))),
                  services = glue::glue_collapse(readLines(app_sys("services.json"))),
                  ecosystems = glue::glue_collapse(readLines(app_sys("ecosystems.json"))),
                  button_id = ns("eco_service_hover_button"),
                  basin_shiny_id = ns("basin"),
                  casestudy_shiny_id = ns("case_study")
                ),
                content_style = "width:50em",
                button_id = ns("eco_service_hover_button")
              )
            ),
            w3_help_button(
              "Select an ecosystem",
              "select_ecosystem_help"
            )
          )
        ),
        w3css::w3_quarter(
          tags$span(
            w3_hover_button(
              "Select a Case Study" %>% with_i18("select-case_study"),
              content = tagList(
                tags$div(
                  id = ns("case_study"),
                  # DT::DTOutput(ns(("case_study_dt")))
                  htmlTemplate(
                    app_sys("app/www/dt_casestudy.html"),
                    services = glue::glue_collapse(readLines(app_sys("services.json"))),
                    shiny_input = ns("case_study"),
                    button_id = ns("case_study_hover_button")
                  )
                )
              ),
              content_style = "width:50em",
              button_id = ns("case_study_hover_button")
            ),
            w3_help_button(
              "Select a case study",
              "select_casestudy_help"
            )
          )
        ),
        w3css::w3_quarter(
          tags$span(
            w3css::w3_actionButton(
              ns("display"),
              "Show results" %>% with_i18("show-result"),
              class = "w3-border"
            ),
            w3_help_button(
              "Show the result",
              "show_results_help"
            )
          )
        )
      )
    ),
    w3css::w3_col(
      hr()
    ),
    container(
      class = w3css::w3_padding(32),
      tags$span(
        "Situation",
        w3_help_button(
          "Results",
          "situation_table_help"
        )
      ),
      DT::DTOutput(ns("tbl"))
    )
  )
}

#' first Server Functions
#' @import ggplot2
#' @noRd
mod_first_server <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loco <- reactiveValues(
      species = NULL
    )

    mod_species_server(
      "species_ui_1",
      r = loco
    )

    output$tbl <- DT::renderDT({
      input$display
      shinipsum::random_table(5, 10)
    })

    observeEvent(input$basin,
      {
        golem::invoke_js(
          "changeinnerhtmlwithid",
          list(
            id = ns("eco_service_hover_button"),
            content = ecosystem_hover_content(input$basin)
          )
        )
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$case_study,
      {
        golem::invoke_js(
          "changeinnerhtmlwithid",
          list(
            id = ns("case_study_hover_button"),
            content = case_study_hover_content(input$case_study)
          )
        )
      },
      ignoreNULL = FALSE
    )
  })
}

## To be copied in the UI
# mod_first_ui("first_ui_1")

## To be copied in the server
# mod_first_server("first_ui_1")