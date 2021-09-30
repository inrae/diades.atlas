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
      w3css::w3_quarter(
        tagList(
          mod_species_ui(ns("species_ui_1"))
        )
      ), 
      w3css::w3_quarter(
        w3_hover_button(
          "Select a scenario",
          content = w3css::w3_radioButton(
            ns("scenario"), 
            "Scenario", 
            choices = c("RCP 8.5", "The other one")
          ),
          content_style = "width:25em", 
          button_id = ns("scenario_hover")
        )
      ), 
      w3css::w3_quarter(
        w3_hover_button(
          "Select a date",
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
        )
      ), 
      w3css::w3_quarter(
        w3css::w3_actionButton(
          ns("display"),
          "Show results" %>% with_i18("show-result"), 
          class = "w3-border"
        )
      )
    ),
    w3css::w3_col(
      hr()
    ), 
    container(
      w3css::w3_half(
        h4("Abundance in river basins") %>% with_i18("map-abundance"),
        plotOutput(ns("map"))
      ), 
      w3css::w3_half(
        h4("Evolution of abundance") %>% with_i18("plot-evolution"),
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
    
    loco <- reactiveValues(
      species = NULL
    )
    
    mod_species_server(
      "species_ui_1", 
      r = loco
    )
    
    output$map <- renderPlot({
      input$display
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
    
    observeEvent( input$scenario , {
      golem::invoke_js(
        "changeinnerhtmlwithid",
        list(
          id = ns("scenario_hover"), 
          content = scenario_hover_content( input$scenario )
        )
      )
    })
    
    observeEvent( input$date , {
      golem::invoke_js(
        "changeinnerhtmlwithid",
        list(
          id = ns("date_hover"), 
          content = date_hover_content( input$date )
        )
      )
    })
    
    
  })
}

## To be copied in the UI
# mod_second_ui("second_ui_1")

## To be copied in the server
# mod_second_server("second_ui_1")
