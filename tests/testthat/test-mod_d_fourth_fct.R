# Create dput when new database arise from 
# source(here::here("data-raw/altas_simulation.R"))

# compute_nmax_eh1 ----
test_that("compute_nmax_eh1 works", {
  # dput(models, file = "tests/testthat/models_dput")
  if (FALSE) {
    # To debug tests only
    extendedNit_entry_small <- eval(parse(file = "tests/testthat/extendedNit_dput"))
  }
  extendedNit_entry_small <- eval(parse(file = "extendedNit_dput"))
  
  results_model_small <- compute_nmax_eh1(model = "cnrmcm5",
                                          scenario = "rcp85",
                                          extendedNit = extendedNit_entry_small)
  # Create expected
  if (FALSE) {
    # To debug tests only
    # dput(results_model_small, file = "tests/testthat/results_model_small_dput")
    rms_expected <- eval(parse(file = "tests/testthat/results_model_small_dput"))
  }
  rms_expected <- eval(parse(file = "results_model_small_dput"))
  expect_equal(results_model_small, rms_expected)
})

# runSimulation ----
# Generate expected dput for the same set 
# of species and basin in data-raw/altas_simulation.R
test_that("runSimulation works", {
  
  skip_if_not_connectable(session_globale)
  
  conn_eurodiad <- get_con(session_globale)
  
  # Use case
  selected_latin_name = "Alosa alosa"
  basin <- 'Adour'
  
  # get_data_simulation - Test inputs ----
  data_simulation <- get_data_simulation(conn_eurodiad)
  data_catchment <- data_simulation[["data_catchment"]]
  #' @description data_catchment exists
  expect_false(is.null(data_catchment))
  expect_equal(ncol(data_catchment), 5)
  expect_equal(count(data_catchment) %>% pull(), 135)
  expect_equal(ncol(head(data_catchment) %>% collect()), 5)
  #' @description outlet_distance exists
  outlet_distance <- data_simulation[["outlet_distance"]]
  expect_false(is.null(outlet_distance))
  expect_equal(ncol(outlet_distance), 5)
  expect_equal(count(outlet_distance) %>% pull(), 18225)
  #' @description hydiad_parameter exists
  hydiad_parameter <- data_simulation[["hydiad_parameter"]]
  expect_false(is.null(hydiad_parameter))
  expect_equal(ncol(hydiad_parameter), 18)
  expect_equal(count(hydiad_parameter) %>% pull(), 11)
  expect_true(all(c("latin_name", "Lname") %in% colnames(hydiad_parameter)))
  #' @description data_hsi_nmax exists
  data_hsi_nmax <- data_simulation[["data_hsi_nmax"]]
  expect_false(is.null(data_hsi_nmax))
  expect_equal(ncol(data_hsi_nmax), 9)
  expect_equal(data_hsi_nmax %>% filter(climatic_scenario == "rcp85") %>% count() %>% pull(), 663300)
  expect_equal(data_hsi_nmax %>% count() %>% pull(), 1326600)
  #' @description reference_results exists
  reference_results <- data_simulation[["reference_results"]]
  expect_false(is.null(reference_results))
  expect_equal(ncol(reference_results), 7)
  expect_equal(reference_results %>% filter(climatic_scenario == "rcp85") %>% count() %>% pull(), 663300)
  expect_equal(reference_results %>% count() %>% pull(), 1326600)
  #' @description data_ni0 exists
  data_ni0 <- data_simulation[["data_ni0"]]
  expect_false(is.null(data_ni0))
  expect_equal(ncol(data_ni0), 9)
  expect_equal(data_ni0 %>% filter(climatic_scenario == "rcp85") %>% count() %>% pull(), 4422)
  expect_equal(count(data_ni0) %>% pull(), 8844)
  #' @description catchment_surface exists
  catchment_surface <- data_simulation[["catchment_surface"]]
  expect_false(is.null(catchment_surface))
  expect_equal(ncol(catchment_surface), 2)
  expect_equal(count(catchment_surface) %>% pull(), 134)
  cs_object <- catchment_surface %>% collect() %>% arrange(basin_name)
  if (FALSE) {
    # to debug tests only
    cs_expected <- eval(parse(
      file = "tests/testthat/catchment_surface_dput")) %>%
      arrange(basin_name)
  }
  cs_expected <- eval(parse(file = "catchment_surface_dput")) %>% arrange(basin_name)
  expect_equal(cs_object, cs_expected)
  
  # generate_datasets ----
  datasets <- generate_datasets(con = conn_eurodiad)
  countries <- datasets[["countries_mortalities_list"]]
  
  # Example of mortalities table ----
  # France only
  mortalities <- tibble::tibble(
    # country = golem::get_golem_options('countries_mortalities_list'),
    country = datasets[["countries_mortalities_list"]],
    mortsimperiod1 = 
      case_when(
        country == "France" ~ 0,
        TRUE ~ 0
      ),
    # rep(-log(.5), length(datasets[["countries_mortalities_list"]])),
    mortsimperiod2 = 
      case_when(
        country == "France" ~ -log(.5),
        TRUE ~ 0
      ),
    # rep(-log(.75), length(datasets[["countries_mortalities_list"]]))
    mortsimperiod3 = 
      case_when(
        country == "France" ~ -log(.75),
        TRUE ~ 0
      )
  )
  
  # expand_anthropogenic_mortality works ----
  anthropogenic_mortality <- expand_anthropogenic_mortality(
    data_hsi_nmax, mortalities)
  am_object <- anthropogenic_mortality %>% 
    select(year, country, h1, h2) %>% 
    arrange(year, country)
  if (FALSE) {
    # For tests debug only
    am_expected <- eval(parse(file = "tests/testthat/anthropogenic_mortality_dput")) %>%
      arrange(year, country)
  }
  am_expected <- eval(parse(file = "anthropogenic_mortality_dput")) %>% 
    arrange(year, country)
  
  expect_equal(am_object, am_expected)
  expect_equal(am_object[["h1"]], am_expected[["h1"]])
  
  # #' @description anthropogenic_mortality has all countries and 6 columns
  # expect_equal(length(sort(unique(anthropogenic_mortality[["country"]]))),
  #              length(sort(countries)))
  # 
  
  # runSimulation ----
  results <- runSimulation(
    selected_latin_name, 
    data_simulation[["hydiad_parameter"]], # 11 rows
    # Smaller for example
    anthropogenic_mortality, #%>% filter(year <= 1955), # 1800 rows
    data_simulation[["catchment_surface"]], # 134 rows
    data_simulation[["data_hsi_nmax"]], # 663300 rows
    data_simulation[["data_ni0"]], # 4422 rows
    data_simulation[["outlet_distance"]], # 18225 rows
    verbose = TRUE,
    scenario = "rcp85"
  )
  
  if (FALSE) {
    # for debug tests only
    utils::unzip(zipfile = "tests/testthat/results_pml_dput.zip")
    resultsPML <- eval(parse(file = "tests/testthat/results_pml_dput"))
    file.remove("tests/testthat/results_pml_dput")
  }
  
  dirzip <- tempfile()
  utils::unzip(zipfile = "results_pml_dput.zip", exdir = dirzip, junkpaths = TRUE)
  resultsPML <- eval(parse(file = file.path(dirzip, "results_pml_dput")))
  file.remove(file.path(dirzip, "results_pml_dput"))
  
  expect_equal(results[["param"]][["hydiad_parameter"]], 
               resultsPML[["param"]][["hydiad_parameter"]])
  expect_equal(results, resultsPML)
  
  # Get NIT ----
  Nit_list <- get_model_nit(results) 
  
  # nit_feature ----
  model_nit_outputs <- nit_feature(Nit_list)
  if (FALSE) {
    # for debug tests only
    mno_expected <- eval(parse(file = "tests/testthat/model_nit_outputs_dput")) %>% 
      ungroup()
  }
  mno_expected <- eval(parse(file = "model_nit_outputs_dput")) %>% 
    ungroup()
  expect_equal(model_nit_outputs, mno_expected)
  
  # nit_feature_species ---- only for the simulated climatic_scenario
  model_res_filtered <- nit_feature_species(
    Nit_list = Nit_list,
    reference_results = reference_results %>% filter(climatic_scenario == "rcp85"),
    selected_latin_name = selected_latin_name) %>% 
    filter(basin_name == basin)
  
  mrf_object <- model_res_filtered %>% 
    arrange(basin_name, year)
  
  if (FALSE) {
    # for debug test only
    mrf_expected <- eval(parse(file = "tests/testthat/model_res_filtered_dput")) %>% 
      rename(
        nit_min = min,
        nit_max = max,
        nit_mean = mean,
        nit_movingavg = rolling_mean
      ) %>% 
      arrange(basin_name, year) %>% 
      ungroup()
  }
  mrf_expected <- eval(parse(file = "model_res_filtered_dput")) %>% 
    rename(
      nit_min = min,
      nit_max = max,
      nit_mean = mean,
      nit_movingavg = rolling_mean
    ) %>% 
    arrange(basin_name, year) %>% 
    ungroup()
  
  expect_equivalent(mrf_object, mrf_expected)
  # waldo::compare(mrf_object, mrf_expected)

  # runSimulation with rcp45 works without errors ----
  expect_error(
    runSimulation(
      selected_latin_name, 
      data_simulation[["hydiad_parameter"]], # 11 rows
      # Smaller for example
      anthropogenic_mortality, #%>% filter(year <= 1955), # 1800 rows
      data_simulation[["catchment_surface"]], # 134 rows
      data_simulation[["data_hsi_nmax"]], # 663300 rows
      data_simulation[["data_ni0"]], # 4422 rows
      data_simulation[["outlet_distance"]], # 18225 rows
      verbose = TRUE,
      scenario = "rcp45"
    ),
    regexp = NA)
  
  # Test for Chelon ramada cf. issue #137 ----
  selected_latin_name <- "Chelon ramada"
  
  expect_error(
    runSimulation(
      selected_latin_name, 
      data_simulation[["hydiad_parameter"]],
      # Smaller for example
      anthropogenic_mortality, 
      data_simulation[["catchment_surface"]], 
      data_simulation[["data_hsi_nmax"]],
      data_simulation[["data_ni0"]], 
      data_simulation[["outlet_distance"]], 
      verbose = TRUE,
      scenario = "rcp45"
    ),
    regexp = NA)
})


# slugify ----
test_that("slugify works", {
  expect_equal(slugify("United  Kingdom"), "united-kingdom")
})

# multi_sliders ----
test_that("multi_sliders works", {
  skip_if_not_connectable(session_globale)
  ns <- session_globale$ns
  output <- multi_sliders(ns = ns, countries = c("France", "United Kingdom"))
  
  # long output ----
  expected_output <-
    '<div class="form-group shiny-input-container">
  <label class="control-label" id="mock-session-period1-france-label" for="mock-session-period1-france">
    <span data-i18n="france">France</span>
  </label>
  <input class="js-range-slider" id="mock-session-period1-france" data-skin="shiny" data-min="0.1" data-max="2" data-from="0.2" data-step="0.02" data-grid="true" data-grid-num="9.5" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
</div>
<div class="form-group shiny-input-container">
  <label class="control-label" id="mock-session-period1-united-kingdom-label" for="mock-session-period1-united-kingdom">
    <span data-i18n="united-kingdom">United Kingdom</span>
  </label>
  <input class="js-range-slider" id="mock-session-period1-united-kingdom" data-skin="shiny" data-min="0.1" data-max="2" data-from="0.2" data-step="0.02" data-grid="true" data-grid-num="9.5" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
</div>'
  # end of long output ----
  expect_equal(as.character(output), expected_output)
})
