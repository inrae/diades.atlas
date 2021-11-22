#' fourth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fourth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("fourth", class = "page_caption") %>% with_i18("title-fourth"),
    container(
      tagList(
        w3css::w3_quarter(
          tagList(
            mod_species_ui(ns("species_ui_1"))
          )
        ),
        # w3css::w3_quarter(
        #   w3_hover_button(
        #     "Show Conservation status" %>% with_i18("show-conservation-status"),
        #     content = tagList(
        #       tags$div(
        #         id = ns("conservation_status"),
        #         style = "padding:1em;",
        #         "lorem_ipsum"
        #       )
        #     ),
        #     content_style = "width:25em"
        #   )
        # ),
        w3css::w3_quarter(
          tags$span(
            w3_hover_button(
              "Change Map geometry", # %>% with_i18("show-conservation-status"),
              content = tagList(
                tags$div(
                  id = ns("square_or_division"),
                  w3css::w3_radioButton(
                    ns("square_or_division"),
                    NULL,
                    choices = c("Division" = "division", "Rectangle" = "rectangle")
                  )
                )
              ),
              content_style = "width:25em"
            ),
            w3_help_button(
              "Some explanations",
              "map_geometrie_help"
            )
          )
        ),
        w3css::w3_quarter()
      )
    ),
    w3css::w3_col(
      hr()
    ),
    container(
      w3css::w3_col(
        class = "s10",
        tags$div(
          h4(
            with_i18("Catch and bycatch at sea", "[html]map-bycatch"),
            w3_help_button(
              "Explanation Species",
              "catch_bycatch_help"
            )
          )
        ),
        tmap::tmapOutput(ns("raster"), width = "90%", height = "750px")
      ),
      w3css::w3_col(
        class = "s2",
        h4(
          with_i18(
            "Conservation status",
            "show-conservation-status"
          ),
          w3_help_button(
            "Explanation Species",
            "conservation_status_help"
          )
        ),
        tagList(
          tags$div(
            id = ns("conservation_status"),
            style = "padding:1em;",
            " "
          )
        )
      )
    )
  )
}

#' fourth Server Functions
#' @import tmap
#' @import dplyr
#' @noRd
mod_fourth_server <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loco <- reactiveValues(
      species = NULL
    )

    mod_species_server(
      "species_ui_1",
      r = loco
    )

    output$raster <- tmap::renderTmap({
      req(loco$species)

      tm_draw(
        species_latin_name = loco$species,
        spatial_type = input$square_or_division,
        con = con,
        dataCatchment = golem::get_golem_options("dataCatchment"),
        catchment_geom = golem::get_golem_options("catchment_geom"),
        dataALL = golem::get_golem_options("dataALL"),
        ices_geom = golem::get_golem_options("ices_geom")
      )
    }) %>% bindCache(
      list(
        loco$species,
        input$square_or_division
      )
    )



    observeEvent(loco$species, {
      req(loco$species)
      species_id <- get_active_species() %>%
        dplyr::filter(latin_name == loco$species) %>%
        dplyr::pull(species_id)

      golem::invoke_js(
        "changeinnerhtmlwithid", list(
          id = ns("conservation_status"),
          content = {
            HTML(get_conservation_status(
              as.numeric(species_id),
              get_con()
            )$array_to_string)
          }
        )
      )

      golem::invoke_js(
        "changeinnerhtmlwithid", list(
          id = ns("conservation_source"),
          content = {
            HTML(container(
              shinipsum::random_text(nwords = 1000) %>%
                strsplit(" ") %>%
                .[[1]] %>%
                sample(50) %>%
                paste(collapse = " ")
            ) %>% as.character())
          }
        )
      )
    })
  })
}

## To be copied in the UI
# mod_fourth_ui("fourth_ui_1")

## To be copied in the server
# mod_fourth_server("fourth_ui_1")