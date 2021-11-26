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
    h1("Ecosystem services", class = "page_caption") %>% with_i18("title-first"),
    container(
      tagList(
        w3css::w3_quarter(
          tagList(
            mod_species_ui(ns("species_ui_1"), multiple = TRUE)
          )
        ),
        w3css::w3_quarter(
          w3_hover_button(
            "Select a Case Study" %>% with_i18("select-case_study"),
            content = htmlTemplate(
              app_sys("app/www/map_case_study.html"),
              geojsonFeature = glue::glue_collapse(readLines(app_sys("casestudy.json"))),
              species = glue::glue_collapse(readLines(app_sys("species.json"))),
              services = glue::glue_collapse(readLines(app_sys("services.json"))),
              ecosystems = glue::glue_collapse(readLines(app_sys("ecosystems.json"))),
              button_id = ns("case_study_hover_button"),
              basin_shiny_id = ns("basin"),
              casestudy_shiny_id = ns("case_study")
            ),
            content_style = "width:50em",
            button_id = ns("case_study_hover_button")
          )
        ),
        w3css::w3_quarter(
          w3_hover_button(
            "Select an Ecosystem Service" %>% with_i18("select-ecosystem"),
            content = tagList(
              tags$div(
                id = ns("ecosystem"),
                # DT::DTOutput(ns(("ecosystem_dt")))
                htmlTemplate(
                  app_sys("app/www/dt_ecosystem.html"),
                  services = glue::glue_collapse(readLines(app_sys("services.json"))),
                  shiny_input = ns("ecosystem"),
                  button_id = ns("ecoservice_hover_button")
                )
              ),
              content_style = "width:50em",
              button_id = ns("case_study_hover_button")
            ),
            content_style = "width:50em",
            button_id = ns("ecoservice_hover_button")
          )
        ),
        w3css::w3_quarter(
          w3css::w3_actionButton(
            ns("display"),
            "Show results" %>% with_i18("show-result"),
            class = "w3-border"
          )
        )
      )
    ),
    w3css::w3_col(
      hr()
    ),
    container(
      class = w3css::w3_padding(32),
      style = "width:90%!important; margin:auto!important",
      # h4("Catch and bycatch at sea") %>% with_i18("map-bycatch"),
      # plotOutput(ns("raster"), click = ns("map_click"))
      DT::DTOutput(ns("tbl")),
      radioButtons(
        ns("wide"),
        "Species are displayed as" %>% with_i18("species_display"),
        choiceValues = c("Rows", "Columns"),
        choiceNames = list(
          with_i18("Rows", "rows"),
          with_i18("Columns", "columns")
        )
      ) %>% tagAppendAttributes(style = "padding-top:1em")
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
      species = NULL,
      table = data.frame(
        species = character(0),
        case_study = character(0),
        ecosystem = character(0),
        session = character(0)
      )
    )

    mod_species_server(
      "species_ui_1",
      r = loco
    )

    observe({
      cli::cat_rule("species")
      print(input[["species_ui_1-species"]])
    })
    observe({
      cli::cat_rule("case_study")
      print(input$case_study)
    })
    observe({
      cli::cat_rule("ecosystem")
      print(input$ecosystem)
    })

    observeEvent(
      input$display,
      {
        # req(input$display > 0)
        spec <- golem::get_golem_options("species_list") %>%
          filter(local_name %in% input[["species_ui_1-species"]]) %>%
          pull(latin_name)
        spec <- input[["species_ui_1-species"]]
        case_stud <- input$case_study
        ecos <- input$ecosystem
        loco$table <- ecosystem_table(
          species = spec,
          case_study = case_stud,
          ecosystem = ecos,
          r = r,
          session = session
        )
      }
    )

    output$tbl <- DT::renderDT(
      {
        req(loco$table)
        dt_to_show <- loco$table
        if (
          input$wide == "Columns" &
            "fish_name" %in% names(loco$table)
        ) {
          res <- tidyr::pivot_wider(
            dt_to_show,
            names_from = fish_name,
            values_from = esvalue_code
          )

          if (r$lg == "fr") {
            res <- res %>%
              rename(
                '<span data-i18n="casestudy_name">Études de cas/span>' = casestudy_name,
                '<span data-i18n="category">Catégories</span>' = category
              )
          } else {
            res <- res %>%
              rename(
                '<span data-i18n="casestudy_name">Case Study</span>' = casestudy_name,
                '<span data-i18n="category">Category</span>' = category
              )
          }
        } else {
          res <- dt_to_show
          if (r$lg == "fr") {
            res <- res %>%
              rename(
                '<span data-i18n="fish_name">Espèces</span>' = fish_name,
                '<span data-i18n="casestudy_name">Études de cas</span>' = casestudy_name,
                '<span data-i18n="category">Catégories</span>' = category,
                '<span data-i18n="esvalue_code">Valeurs ES</span>' = esvalue_code
              )
          } else {
            res <- res %>%
              rename(
                '<span data-i18n="fish_name">Fish</span>' = fish_name,
                '<span data-i18n="casestudy_name">Case Study</span>' = casestudy_name,
                '<span data-i18n="category">Category</span>' = category,
                '<span data-i18n="esvalue_code">ES Value</span>' = esvalue_code
              )
          }
        }
        res
      },
      escape = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE,
        scrollY = 500,
        language = list(
          emptyTable = as.character(
            with_i18("No data, please make a selection", "no_data_in_dt")
          )
        ),
        pageLength = {
          if (input$wide == "Columns") {
            tidyr::pivot_wider(
              loco$table,
              names_from = fish_name,
              values_from = esvalue_code
            ) %>% nrow()
          } else {
            nrow(loco$table)
          }
        }
      )
    )

    observeEvent(input$basin,
      {
        golem::invoke_js(
          "changeinnerhtmlwithid",
          list(
            id = ns("case_study_hover_button"),
            content = case_study_hover_content(input$basin)
          )
        )
      },
      ignoreNULL = FALSE
    )
 
    observeEvent(input$ecosystem,
      {
        golem::invoke_js(
          "changeinnerhtmlwithid",
          list(
            id = ns("ecoservice_hover_button"),
            content = ecosystem_hover_content(input$ecosystem)
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