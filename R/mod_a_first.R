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
    h1(
      with_i18("first", "title-first"),
      w3_help_button(
        "first",
        "first_title_help"
      ),
      class = "page_caption"
    ),
    container(
      tagList(
        w3css::w3_quarter(
          tagList(
            mod_species_ui(ns("species_ui_1"))
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
              "Select a species",
              "catch_bycatch_help"
            )
          )
        ),
        leaflet::leafletOutput(ns("raster"), width = "90%", height = "750px")
      ),
      w3css::w3_col(
        class = "s2",
        actionButton(ns("showaqua"),
                     label = 'AquaMaps', 
                     style = "background-color: #FFFF0080"),
        # radioButtons(
        #   ns("showaqua"),
        #   label = NULL,
        #   choices = c(
        #     "Hide AquaMaps" = "hide",
        #     "Show AquaMaps" = "show"
        #   )
        # ),
        w3_help_button(
          "Display AquaMpas",
          "display_aquamaps_help"
        ),
        actionButton(ns("positive_catch"),
                     label = with_i18('Positive catch', 'positive_catch_button'),
                     style = "background-color: #00FF0080"),
        w3_help_button(
          "Display positive catch",
          "display_positive_catch_help"
        ),
        h4(
          with_i18(
            "Conservation status",
            "show-conservation-status"
          ),
          w3_help_button(
            "Select a conservation Status",
            "conservation_status_help"
          )
        ),
        tagList(
          tags$div(
            align = "left",
            id = ns("conservation_status"),
            style = "padding:1em;",
            " "
          )
        )
      )
    )
  )
}

#' first Server Functions
#' @import tmap
#' @import dplyr
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

    output$raster <- leaflet::renderLeaflet({
      req(loco$species)

      tm_draw(
        species_latin_name = loco$species,
        spatial_type = "rectangle",
        con = con,
        dataCatchment = golem::get_golem_options("dataCatchment"),
        catchment_geom = golem::get_golem_options("catchment_geom"),
        dataALL = golem::get_golem_options("dataALL"),
        ices_geom = golem::get_golem_options("ices_geom"), 
        ices_division = golem::get_golem_options("ices_division"),
        positive_catch_area = golem::get_golem_options("positive_catch_area"),
        session = session
      )
    }) 
    # %>% bindCache(
    #   list(
    #     loco$species,
    #     input$square_or_division
    #   ),
    #   cache = get_mongo()
    # )

    # observeEvent(input$showaqua, {
    #   if (input$showaqua == "show"){
    #     leafletProxy("raster", session) %>%
    #       leaflet::showGroup("AquaMaps")
    #   } else {
    #     leafletProxy("raster", session) %>%
    #       leaflet::hideGroup("AquaMaps")
    #   }
    # })
    
    observeEvent(input$showaqua, {
      if (input$showaqua %% 2 == 1){
        leafletProxy("raster", session) %>%
          leaflet::showGroup("AquaMaps")
      } else {
        leafletProxy("raster", session) %>%
          leaflet::hideGroup("AquaMaps")
      }
    })
    
    observeEvent(input$positive_catch, {
      if (input$positive_catch %% 2 == 1){
        leafletProxy("raster", session) %>%
          leaflet::showGroup('positive catch of at least one species')
      } else {
        leafletProxy("raster", session) %>%
          leaflet::hideGroup('positive catch of at least one species')
      }
    })

    observeEvent(loco$species, {
      req(loco$species)
      species_id <- get_active_species() %>%
        dplyr::filter(latin_name == loco$species) %>%
        dplyr::pull(species_id)

      golem::invoke_js(
        "changeinnerhtmlwithid", list(
          id = ns("conservation_status"),
          content = {
            status <- get_conservation_status(
              as.numeric(species_id),
              get_con()
            )
            status$iucn_level_code <- mapply(
              function(x, y) {
                as.character(with_i18(x, y))
              },
              x = status$english_name,
              y = status$iucn_level_code,
              SIMPLIFY = FALSE,
              USE.NAMES = FALSE
            ) %>% unlist()
            as.character(tags$ul(
              HTML(
                paste0(
                  "<li>",
                  status$iucn_classification_code,
                  " - ",
                  status$iucn_level_code,
                  "</li>",
                  collapse = " "
                )
              )
            ))
          }
        )
      )
    })
  })
}

## To be copied in the UI
# mod_first_ui("first_ui_1")

## To be copied in the server
# mod_first_server("first_ui_1")