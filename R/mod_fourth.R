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
    quarter(
      h3("Select a Species") %>% with_i18("select-species"),
      selectInput(
        ns("species"), 
        NULL, 
        choices = paste(
          "Espece", letters[1:11]
        )
      )
    ), 
    half(
      h4("Catch and bycatch at sea") %>% with_i18("map-bycatch"),
      plotOutput(ns("raster"), click = ns("map_click"))
    ), 
    quarter(
      uiOutput(ns("source"))
    )
  )
}

#' fourth Server Functions
#'
#' @noRd 
mod_fourth_server <- function(id, r = r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$raster <- renderImage({
      # input$species
      # When input$n is 1, filename is ./images/image1.jpeg
      rasterimg <- system.file("app/www/raster_crop.jpg", package = "diades.atlas")
      # Return a list containing the filename
      list(src = normalizePath(rasterimg),
           height = 350)
    }, deleteFile = FALSE) # Do not delete inside the package installation
    
    output$source <- renderUI({
      input$map_click
      tagList(
        h3("Source") %>% with_i18("h3-source"),
        tags$div(
          shinipsum::random_text(nwords = 1000) %>%
            strsplit(" ") %>%
            .[[1]] %>%
            sample(50) %>% 
            paste(collapse = " ")
        )
      )
    })
  })
}

## To be copied in the UI
# mod_fourth_ui("fourth_ui_1")

## To be copied in the server
# mod_fourth_server("fourth_ui_1")
