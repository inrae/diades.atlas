#' second UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_second_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("second", class = "page_caption") %>% with_i18("title-second"),
    container(
      w3css::w3_quarter(
        tagList(
          mod_species_ui(ns("species_ui_1"))
        )
      ),
      w3css::w3_quarter(
        tags$span(
          w3_hover_button(
            "Select a scenario",
            content = w3css::w3_radioButton(
              ns("scenario"),
              "Scenario",
              choices = c(
                "RCP 8.5" = "rcp85",
                "The other one" = "other"
              )
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
            "Select a date",
            content = container(
              sliderInput(
                ns("date"),
                NULL,
                min = 1950,
                max = 2100,
                value = 1950, 
                sep = ""
              )
            ),
            button_id = ns("date_hover")
          ),
          w3_help_button(
            "Select a daterange",
            "choose_a_daterange_help"
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
            "Display the results",
            "show_results_help"
          )
        )
      )
    ),
    w3css::w3_col(
      hr()
    ),
    container(
      w3css::w3_third(
        h4(
          with_i18("Abundance in river basins", "map-abundance"),
          w3_help_button(
            "Abundance map",
            "map_abundance_help"
          )
        ),
        leafletOutput(ns("plot"), height = 600)
      ),
      w3css::w3_twothird(
        h4(
          with_i18("Evolution of abundance", "plot-evolution"),
          w3_help_button(
            "Abundance evolution",
            "plot_evolution_help"
          )
        ),
        plotOutput(ns("prediction"), height = 600)
      )
    )
  )
}

#' second Server Functions
#'
#' @noRd
mod_second_server <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loco <- reactiveValues(
      species = NULL
    )

    mod_species_server(
      "species_ui_1",
      r = loco
    )

    observeEvent(
      input$display,
      {
        if (input$scenario != "rcp85") {
          shiny::showNotification(
            "Scenario not implemented",
            type = "error"
          )
          return(NULL)
        }
        spc <- golem::get_golem_options("species_list")
        loco$model_res <- get_hybrid_model(
          species_id = spc[spc$latin_name == loco$species, "species_id"],
          scenario = input$scenario,
          session = session
        )
        loco$bv_df <- get_bv_geoms(
          unique(loco$model_res$basin_id),
          session
        )
        loco$selected_bv <- sample(
          unique(loco$model_res$basin_id),
          1
        )
        loco$leaflet <- draw_bv_leaflet(
          loco$bv_df,
          loco$model_res,
          input$date
        )
      }
    )

    output$plot <- renderLeaflet({
      loco$leaflet
    })

    observeEvent(input$plot_shape_click, {
      loco$selected_bv_id <- input$plot_shape_click$id
      # Do we need that?
      loco$selected_bv_name <- tbl(get_con(session), "basin") %>%
        filter(basin_id == !!input$plot_shape_click$id) %>%
        collect()
      loco$plot <- plot_hsi_nit(
        loco$model_res,
        input$date,
        loco$selected_bv_id
      )
    })

    output$prediction <- renderPlot({
      loco$plot
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
# mod_second_ui("second_ui_1")

## To be copied in the server
# mod_second_server("second_ui_1")