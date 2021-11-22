#' third UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_third_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("third", class = "page_caption") %>% with_i18("title-third"),
    container(
      w3css::w3_quarter(
        tagList(
          mod_species_ui(ns("species_ui_1"))
        )
      ),
      w3css::w3_quarter(
        tags$span(
          w3_hover_button(
            "Select a scenario" %>% with_i18("select-scenario"),
            content = w3css::w3_radioButton(
              ns("scenario"),
              "Scenario",
              choices = c("RCP 8.5", "The other one")
            ),
            content_style = "width:25em",
            button_id = ns("scenario_hover")
          ),
          w3_help_button(
            "Select a scenario",
            "choose_a_scenario_help"
          )
        )
      ),
      w3css::w3_quarter(
        tags$span(
          w3_hover_button(
            "Define anthropogenic mortalities" %>% with_i18("h3-anthropogenic"),
            content = container(
              sliderInput(
                ns("ie"),
                "IE",
                min = 0.1,
                max = 2,
                value = 0.2
              ),
              sliderInput(
                ns("uk"),
                "UK",
                min = 0.1,
                max = 2,
                value = 0.2
              ),
              sliderInput(
                ns("fr"),
                "FR",
                min = 0.1,
                max = 2,
                value = 0.2
              ),
              sliderInput(
                ns("es"),
                "ES",
                min = 0.1,
                max = 2,
                value = 0.2
              ),
              sliderInput(
                ns("pt"),
                "PT",
                min = 0.1,
                max = 2,
                value = 0.2
              )
            ),
            content_style = "width:25em",
            button_id = ns("scenario_hover")
          ),
          w3_help_button(
            "Define the anthropogenic mortalities",
            "define_anthropo_help"
          )
        )
      ),
      w3css::w3_quarter(
        tags$span(
          w3_hover_button(
            "Select a date" %>% with_i18("select-daterange"),
            content = container(
              sliderInput(
                ns("date"),
                NULL,
                min = 1950,
                max = 2100,
                value = c(1950, 2100)
              )
            ),
            button_id = ns("date_hover")
          ),
          w3_help_button(
            "Select a date range",
            "choose_a_daterange_help"
          )
        )
      ),
      w3css::w3_col(
        hr()
      ),
      container(
        w3css::w3_col(
          tags$span(
            w3css::w3_actionButton(
              ns("display"),
              "Run the simulation" %>% with_i18("h3-run-simulation"),
              class = "w3-border"
            ),
            w3_help_button(
              "Launch the simulation",
              "run_simulation_help"
            )
          )
        ),
        w3css::w3_half(
          h4(
            with_i18("Abundance in river basins", "map-abundance"),
            w3_help_button(
              "Predicted abundance map:",
              "prediction_map_abundance_help"
            )
          ),
          plotOutput(ns("map"))
        ),
        w3css::w3_half(
          h4(
            with_i18("Evolution of abundance", "plot-evolution"),
            w3_help_button(
              "Predicted abundance evolution:",
              "prediction_plot_abundance_help"
            )
          ),
          plotOutput(ns("prediction"))
        )
      )
    )
  )
}

#' third Server Functions
#'
#' @noRd
#' @import maps
mod_third_server <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loco <- reactiveValues(
      species = NULL
    )

    mod_species_server(
      "species_ui_1",
      r = loco
    )

    output$map <- renderPlot({
      input$display
      ggplot(map_data("france"), aes(long, lat, group = group)) +
        geom_polygon() +
        geom_polygon(
          data = map_data("france") %>%
            dplyr::filter(region %in% sample(
              unique(map_data("france")$region),
              3
            )),
          aes(fill = region)
        ) +
        # coord_map() +
        theme_void() +
        guides(
          fill = FALSE
        )
    })

    output$prediction <- renderPlot({
      input$display
      p1 <- shinipsum::random_ggplot(type = "line")
      p2 <- shinipsum::random_ggplot(type = "line")
      patchwork:::`/.ggplot`(
        p1, p2
      )
    })

    observeEvent(input$scenario, {
      golem::invoke_js(
        "changeinnerhtmlwithid",
        list(
          id = ns("scenario_hover"),
          content = scenario_hover_content(input$scenario)
        )
      )
    })

    observeEvent(input$date, {
      golem::invoke_js(
        "changeinnerhtmlwithid",
        list(
          id = ns("date_hover"),
          content = date_hover_content(input$date)
        )
      )
    })
  })
}

## To be copied in the UI
# mod_third_ui("third_ui_1")

## To be copied in the server
# mod_third_server("third_ui_1")