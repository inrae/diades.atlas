#' first UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_first_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Situation in present time", class = "page_caption") %>% with_i18("title-first"), 
    container(
      tags$div(
        class = "w3-col m5",
        h3("Case studies")  %>% with_i18("h3-case-studies"),
        plotOutput(ns("plota"))
      ), 
      tags$div(
        class = "w3-col m2",
        h3("Species") %>% with_i18("h3-species"),
        radioButtons(
          ns("buttons"), 
          label = NULL, 
          choices = paste(
            "Espece", letters[1:11]
          )
        )
      ),
      tags$div(
        class = "w3-col m5",
        h3("Ecosystem services")  %>% with_i18("h3-ecosystem"),
        DT::dataTableOutput(ns("dt"))
      )
    )
  )
}

#' first Server Functions
#' @import ggplot2
#' @noRd 
mod_first_server <- function(id, r = r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plota <- renderPlot({
      input$buttons
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

    output$dt <- DT::renderDT({
      input$buttons
      shinipsum::random_table(5, 3)
    },
    options = list(
      language = get_dt_lg(r$lg)
    ))
  })
}

## To be copied in the UI
# mod_first_ui("first_ui_1")

## To be copied in the server
# mod_first_server("first_ui_1")
