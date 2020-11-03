#' third UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_third_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("third", class = "page_caption") %>% with_i18("titles-third"), 
    container(
      quarter(
        h3("Define anthropogenic mortalities") %>% with_i18("h3-anthropogenic"),
        sliderInput(
          ns("ir"), 
          "IR", 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        ),
        sliderInput(
          ns("uk"), 
          "UK", 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        ),
        sliderInput(
          ns("fr"), 
          "FR", 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        ),
        sliderInput(
          ns("es"), 
          "ES", 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        ),
        sliderInput(
          ns("po"), 
          "PO", 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        ) 
      ),
      quarter(
        h3("Select a Species") %>% with_i18("select-species"),
        selectInput(
          ns("species"), 
          NULL, 
          choices = paste(
            "Species", letters[1:11]
          )
        ),
        h3("Choose a Scenario") %>% with_i18("select-scenario"),
        selectInput(
          ns("scenario"), 
          NULL, 
          choices = paste(
            "Scenario", letters[1:11]
          )
        ), 
        h3("Chose a daterange") %>% with_i18("select-daterange"),
        sliderInput(
          ns("date"), 
          NULL, 
          min = 1950, 
          max = 2100, 
          value = c(1950, 2100)
        ),
        h3("Run the simulation") %>% with_i18("h3-run-simulation"),
        actionButton(
          ns("go"), 
          with_i18("Launch", "button-launch")
        )
      ),
      quarter(
        plotOutput(ns("plot"))
      ), 
      quarter(
        DT::dataTableOutput(ns("dt"))
      )
    )
  )
}

#' third Server Functions
#'
#' @noRd 
mod_third_server <- function(id, r = r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      input$go
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
      input$go
      shinipsum::random_table(5, 3)
    },
    options = list(
      language = get_dt_lg(r$lg)
    ))
    
  })
}

## To be copied in the UI
# mod_third_ui("third_ui_1")

## To be copied in the server
# mod_third_server("third_ui_1")
