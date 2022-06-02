#' fourth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
mod_fourth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1(
      with_i18("fourth", "title-fourth"),
      w3_help_button(
        "fourth",
        "fourth_title_help"
      ),
      class = "page_caption"
    ),
    container(
      w3css::w3_quarter(
        tagList(
          mod_species_ui(ns("species_ui_1"))
        )
      ),
      w3css::w3_quarter(
        tags$span(
          w3_hover_button(
            "Select a scenario" %>% with_i18("select-scenario"),
            content = w3css::w3_radioButton(
              ns("scenario"),
              "Scenario",
              choices = c(
                "RCP 8.5" = "rcp85",
                "RCP 4.5" = "rcp45"
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
            "Define anthropogenic mortalities" %>% with_i18("h3-anthropogenic"),
            content = container(
              tagList(
                helpText(HTML(
                  "<ul>
                    <li>Double click on a cell to edit</li>
                    <li>Press Esc/Ech or click here to accept your value</li>
                   </ul>")),
                DTOutput(ns("mortalities"))
              )
            ),
            content_style = "width:25em;overflow: auto;max-height: 500px;",
            button_id = ns("scenario_hover")
          ),
          w3_help_button(
            "Define the anthropogenic mortalities",
            "define_anthropo_help"
          )
        )
      ),
      w3css::w3_quarter(
        tags$span(
          w3css::w3_actionButton(
            ns("launch_simu"),
            "Run the simulation" %>% with_i18("h3-run-simulation"),
            class = "w3-border"
          ),
          w3_help_button(
            "Launch the simulation",
            "run_simulation_help"
          )
        )
      ),
      w3css::w3_col(
        hr()
      ),
      container(
        w3css::w3_col(
          w3css::w3_quarter(
            tags$span(
              w3_hover_button(
                "Select a date" %>% with_i18("select-daterange-simu"),
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
                "Select a date range",
                "choose_a_daterange_simu_help"
              )
            )
          )
        ),
        w3css::w3_half(
          h4(
            with_i18("Abundance in river basins", "map-abundance"),
            w3_help_button(
              "Predicted abundance map:",
              "prediction_map_abundance_help"
            )
          ),
          plotOutput(ns("map"))
        ),
        w3css::w3_half(
          h4(
            with_i18("Evolution of abundance", "plot-evolution"),
            w3_help_button(
              "Predicted abundance evolution:",
              "prediction_plot_abundance_help"
            )
          ),
          plotOutput(ns("prediction"))
        )
      )
    )
  )
}

#' fourth Server Functions
#'
#' @noRd
#' @import maps
#' @importFrom DT renderDT
#' @importFrom utils getFromNamespace
mod_fourth_server <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    loco <- reactiveValues(
      species = NULL,
      mortalities = data.frame(
        country = golem::get_golem_options('countries_mortalities_list'),
        yearsimubegin = rep(0.1, length(golem::get_golem_options('countries_mortalities_list'))),
        yearsimuend = rep(0.1, length(golem::get_golem_options('countries_mortalities_list')))
      ),
      data_simulation = get_data_simulation(conn_eurodiad),
      results = NULL
    )
    
    mod_species_server(
      "species_ui_1",
      r = loco
    )
    
    # Mortalities inputs ----
    # Client side processing
    options(DT.options = list(pageLength = 10))
    output$mortalities <- renderDT({
      cat_where(whereami::whereami())
      
      table_mort <- loco$mortalities %>% 
        setNames(
          c(
            get_translation_entry(entry = 'country', lg = r$lg),
            get_translation_entry(entry = 'yearsimubegin', lg = r$lg),
            get_translation_entry(entry = 'yearsimuend', lg = r$lg)
          )
        )
      
      table_mort
    },
    options = list(
      pageLength = length(golem::get_golem_options('countries_mortalities_list')),
      dom = 't'
    ),
    rownames = FALSE,
    selection = 'none', 
    editable = list(target = "cell", disable = list(columns = c(0)))
    )
    
    # Get new inputs
    # edit a cell
    observeEvent(input$mortalities_cell_edit, {
      # browser()
      # Index comes from JS that start from zero on columns
      # And rownames = FALSE, need to add 1 to 'col'
      mortalities_cell_edit <- input$mortalities_cell_edit
      mortalities_cell_edit[,'col'] <- mortalities_cell_edit[,'col'] + 1
      data_edited <- DT::editData(loco$mortalities, mortalities_cell_edit, 'mortalities')
      loco$mortalities <- data_edited
    })
    
    # Run simulations ----
    observeEvent(input$launch_simu, {
      # loco$data_simulation
      # countries <- golem::get_golem_options('countries_mortalities_list')
      # loco$mortalities
      # data_hsi_nmax <- data_simulation[["data_hsi_nmax"]]
      anthropogenic_mortality <- expand_anthropogenic_mortality(
        loco$data_simulation[["data_hsi_nmax"]], loco$mortalities)
      # spc <- golem::get_golem_options("species_list")
      
      # browser()
      
      # runSimulation ----
      shiny::withProgress(
        message = 'Run Simulation', value = 0, 
        session = session, {
          loco$results <- runSimulation(
            selected_latin_name = loco$species, #selected_latin_name, 
            scenario = input$scenario,
            hydiad_parameter = loco$data_simulation[["hydiad_parameter"]], # 11 rows
            # Smaller for example
            anthropogenic_mortality = anthropogenic_mortality, #%>% filter(year <= 1955), # 1800 rows
            catchment_surface = loco$data_simulation[["catchment_surface"]], # 134 rows
            data_hsi_nmax = loco$data_simulation[["data_hsi_nmax"]], # 663300 rows
            data_ni0 = loco$data_simulation[["data_ni0"]], # 4422 rows
            outlet_distance = loco$data_simulation[["outlet_distance"]], # 18225 rows
            verbose = TRUE
          )
        })
    }, ignoreInit = TRUE)
    
    # Show results ----
    output$map <- renderPlot({
      input$launch_simu
      the_data <- loco$mortalities
      names(the_data) <- c("X1", "X2", "X3")
      ggplot(the_data) +
        aes(X2, X3) +
        geom_point() +
        # ggplot(map_data("france"), aes(long, lat, group = group)) +
        #   geom_polygon() +
        #   geom_polygon(
        #     data = map_data("france") %>%
        #       dplyr::filter(region %in% sample(
        #         unique(map_data("france")$region),
        #         3
        #       )),
        #     aes(fill = region)
        #   ) +
        # coord_map() +
      theme_classic() +
        guides(
          fill = "none"
        )
    })
    
    output$prediction <- renderPlot({
      input$launch_simu
      p1 <- shinipsum::random_ggplot(type = "line")
      p2 <- shinipsum::random_ggplot(type = "line")
      getFromNamespace("/.ggplot", "patchwork")(
        p1, p2
      )
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
# mod_fourth_ui("fourth_ui_1")

## To be copied in the server
# mod_fourth_server("fourth_ui_1")
