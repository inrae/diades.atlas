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
      plotOutput(ns("map"), click = ns("map_click"))
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
    output$map <- renderPlot({
      input$species
      ggplot(map_data("france"), aes(long,lat, group=group)) +
        geom_polygon() +
        geom_polygon(
          data = map_data("france") %>%
            dplyr::filter(region %in% sample(
              unique(map_data("france")$region), 
              3
            )), 
          aes(fill = region)
        ) +
        coord_map() +
        theme_void() + 
        guides(
          fill = FALSE
        ) 
    })
    
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
