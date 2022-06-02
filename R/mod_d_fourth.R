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
                    min = 1951,
                    max = 2100,
                    value = 1951, 
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
          uiOutput(ns("selection_map")),
          leafletOutput(ns("plot"), height = 600)
        ),
        w3css::w3_half(
          h4(
            with_i18("Evolution of abundance", "plot-evolution"),
            w3_help_button(
              "Predicted abundance evolution:",
              "prediction_plot_abundance_help"
            )
          ),
          uiOutput(ns("selection_plot")),
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
      data_simulation = get_data_simulation(get_con(session)),
      results = NULL,
      model_res_filtered = NULL,
      trigger_graphs = 0,
      selected_bv_id = NULL,
      bind_event = 0
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
      cli::cat_rule("observeEvent(input$launch_simu")
      
      golem::invoke_js("disable", paste0("#", ns("launch_simu")))
      # loco$data_simulation
      # countries <- golem::get_golem_options('countries_mortalities_list')
      # loco$mortalities
      # data_hsi_nmax <- data_simulation[["data_hsi_nmax"]]
      anthropogenic_mortality <- expand_anthropogenic_mortality(
        loco$data_simulation[["data_hsi_nmax"]], loco$mortalities)
      # spc <- golem::get_golem_options("species_list")
      
      showModal(
        modalDialog(
          tagList(
            tags$h2("Your simulation will be ready in a few seconds") %>% with_i18("wait_simu")
          ),
          easyClose = TRUE, size = 'm')
      )
      
      # modal()
      # runSimulation ----
      shiny::withProgress(
        message = 'Run Simulation', value = 0, 
        session = session, {
          results <- runSimulation(
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
      
      Nit_list <- get_model_nit(results)
      
      loco$model_res_filtered <- nit_feature_species(
        Nit_list = Nit_list,
        reference_results = loco$data_simulation[["reference_results"]] %>% 
          filter(climatic_scenario == !!input$scenario),
        selected_latin_name = loco$species) %>% 
        left_join(loco$data_simulation[["data_catchment"]] %>% collect(),
                  by = "basin_name")
      
      if (!is.null(loco$model_res_filtered)) {
        loco$trigger_graphs <- loco$trigger_graphs + 1
      }
      
      removeModal()
      
      golem::invoke_js("reable", paste0("#", ns("launch_simu")))
    }, ignoreInit = TRUE)
    
    # Show results ----
    observeEvent(list(loco$trigger_graphs, input$date, r$lg), {
      req(loco$model_res_filtered)
      cli::cat_rule("observeEvent(list(loco$trigger_graphs)")
      
      model_res <- loco$model_res_filtered %>% 
        filter(source == "simul")
      
      # Same as mod_c_third ----
      if (nrow(model_res) == 0) {
        shiny::showNotification(
          h1("No result for this species"),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Get selection on BV 
      loco$bv_df <- get_bv_geoms(
        unique(model_res$basin_id),
        lg = r$lg,
        session
      )
      if (is.null(loco$selected_bv_id)) {
        loco$selected_bv_id <- sample(
          unique(model_res$basin_id),
          1
        )
      }
      loco$selected_bv_name <- tbl(get_con(session), "basin") %>%
        filter(basin_id == !!loco$selected_bv_id) %>%
        mutate(basin_name = diadesatlas.translate(basin_name, !!r$lg)) %>%
        collect()
      
      # Create leaflet
      loco$leaflet <- draw_bv_leaflet(
        bv_df = loco$bv_df,
        model_res = model_res,
        year = input$date
      )
      
      # Get predictions for one BV
      loco$prediction <- loco$model_res_filtered %>% 
        filter(basin_id == loco$selected_bv_id) %>% 
        plot_nit(selected_year = input$date,
                 lg = r$lg,
                 withNitStandardisation = FALSE,
                 with_colour_source = "source")
      
      # Create information
      loco$ui_summary <- create_ui_summary_html(
        species = loco$species,
        date = input$date,
        basin_name = loco$selected_bv_name$basin_name,
        country = loco$selected_bv_name$country
      )
      # end of same ----
      
    }, ignoreInit = TRUE)
    

    
    # If click on the map or change lang
    observeEvent(list(input$plot_shape_click), {
      # req(loco$trigger_graphs)
      req(loco$model_res_filtered)
      cli::cat_rule("observeEvent(list(input$plot_shape_click, r$lg)")
      
      loco$selected_bv_id <- input$plot_shape_click$id
      loco$selected_bv_name <- tbl(get_con(session), "basin") %>%
        filter(basin_id == !!input$plot_shape_click$id) %>%
        mutate(basin_name = diadesatlas.translate(basin_name, !!r$lg)) %>%
        collect()
      
      # Update Nit predictions
      loco$prediction <- loco$model_res_filtered %>% 
        filter(basin_id == loco$selected_bv_id) %>% 
        plot_nit(selected_year = input$date,
                 lg = r$lg,
                 withNitStandardisation = FALSE,
                 with_colour_source = "source")
      
      # Update information
      loco$ui_summary <- create_ui_summary_html(
        species = loco$species,
        date = input$date,
        basin_name = loco$selected_bv_name$basin_name,
        country = loco$selected_bv_name$country
      )
      
    }, ignoreInit = TRUE)
    
    # Show summary ====
    output$selection_map <- renderUI({
      golem::invoke_js("localize", TRUE)
      if (is.null(loco$ui_summary)) {
        # HTML("Please launch a simulation")
        HTML("<b><span data-i18n='launch_simu_please'>Please Launch Simulation</span></b>")
      } else {
        loco$ui_summary
      }
      # req(loco$ui_summary)
    })   
    output$selection_plot <- renderUI({
      golem::invoke_js("localize", TRUE)
      req(loco$ui_summary)
    })
    
    # Leaflet ====
    # Same as mod_c_third ----
    output$plot <- renderLeaflet({
      cli::cat_rule("output$plot <- renderLeaflet")
      loco$bind_event <- rnorm(10000)
      loco$leaflet
    })
    
    observeEvent(loco$bind_event,
                 {
                   req(loco$bind_event)
                   cli::cat_rule("bind_event")
                   golem::invoke_js("bindleaflettab3", list(id = ns("plot"), ns = loco$bind_event))
                 },
                 ignoreInit = TRUE
    )
    # end of same ----
    
    # Predictions ----
    output$prediction <- renderPlot({
      loco$prediction
    })
    
    # UI dropdown menus ----
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
