#' second UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_second_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("second", class = "page_caption") %>% with_i18("title-second"), 
    container(
      third(
        h3("Select a Species") %>% with_i18("select-species"),
        selectInput(
          ns("species"), 
          NULL , 
          choices = paste(
            "Espece", letters[1:11]
          )
        )
      ), 
      third(
        h3("Select a Scenario") %>% with_i18("select-scenario"),
        selectInput(
          ns("scenario"), 
          NULL, 
          choices = paste(
            "scenario", letters[1:11]
          )
        )
      ), 
      third(
        h3("Choose a Daterange") %>% with_i18("select-daterange"),
        sliderInput(
          ns("date"), 
          NULL, 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        )
      )
    ), 
    container(
      half(
        plotOutput(ns("map"))
      ), 
      half(
        plotOutput(ns("prediction"))
      )
    )
  )
}

#' second Server Functions
#'
#' @noRd 
mod_second_server <- function(id, r = r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$map <- renderPlot({
      input$species
      input$scenario
      input$date
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
    
    output$prediction <- renderPlot({
      input$species
      input$scenario
      input$date
      shinipsum::random_ggplot(type = "line")
      
    })
    
  })
}

## To be copied in the UI
# mod_second_ui("second_ui_1")

## To be copied in the server
# mod_second_server("second_ui_1")
