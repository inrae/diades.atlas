#' fourth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fourth_ui <- function(id){
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
        w3css::w3_quarter(
          w3_hover_button(
            "Show Conservation status" %>% with_i18("show-conservation-status"),
            content = tagList(
              tags$div(
                id = ns("conservation_status"),
                "lorem_ipsum"
              )
            ),
            content_style = "width:25em"
          )
        ),
        # w3css::w3_quarter(
        #   w3_hover_button(
        #     "Show Source" %>% with_i18("show-source"),
        #     content = tagList(
        #       tags$div(
        #         id = ns("conservation_source"),
        #         "lorem_ipsum"
        #       )
        #     ),
        #     content_style = "width:25em"
        #   )
        # ),
        w3css::w3_quarter(
          w3_hover_button(
            "Change Map geometry", # %>% with_i18("show-conservation-status"),
            content = tagList(
              tags$div(
                id = ns("square_or_division"),
                w3css::w3_radioButton(
                  ns("square_or_division"), 
                  NULL,
                  choices = c("Division", "Rectangle")
                )
              )
            ),
            content_style = "width:25em"
          )
        ),
        w3css::w3_quarter()
        
      )
    ),
    w3css::w3_col(
      hr()
    ),
    container(
      h4("Catch and bycatch at sea") %>% with_i18("map-bycatch"),
      #plotOutput(ns("raster"), click = ns("map_click"))
      tmap::tmapOutput(ns("raster"), width = "80%") 
    )
  )
  
}

#' fourth Server Functions
#' @import tmap
#' @noRd 
mod_fourth_server <- function(id, r = r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # output$raster <- renderImage({
    #   # input$species
    #   # When input$n is 1, filename is ./images/image1.jpeg
    #   rasterimg <- system.file("app/www/raster_crop.jpg", package = "diades.atlas")
    #   # Return a list containing the filename
    #   list(src = normalizePath(rasterimg),
    #        height = 350)
    # }, deleteFile = FALSE) # Do not delete inside the package installation
    
    loco <- reactiveValues(
      species = NULL
    )

    mod_species_server(
      "species_ui_1", 
      r = loco
    )
    
    output$raster <- tmap::renderTmap({
      req(loco$species)
      tm_shape(World) +
        tm_polygons(loco$species )
    })
    
    observeEvent( loco$species , {
      req(loco$species)
      species_id <- get_active_species() %>%
        dplyr::filter(latin_name == loco$species) %>%
        dplyr::pull(species_id)
      
      golem::invoke_js(
        "changeinnerhtmlwithid", list(
          id = ns("conservation_status"), 
          content = {
            HTML(get_conservation_status(
              species_id,
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
