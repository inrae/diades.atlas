# compute_nmax_eh1 ----
test_that("compute_nmax_eh1 works", {
  # dput(models, file = "tests/testthat/models_dput")
  # extendedNit_entry_small <- eval(parse(file = "tests/testthat/extendedNit_dput"))
  extendedNit_entry_small <- eval(parse(file = "extendedNit_dput"))
  
  results_model_small <- compute_nmax_eh1(model = "cnrmcm5", extendedNit = extendedNit_entry_small)
  # Create expected
  # dput(results_model_small, file = "tests/testthat/results_model_small_dput")
  # rms_expected <- eval(parse(file = "tests/testthat/results_model_small_dput"))
  rms_expected <- eval(parse(file = "results_model_small_dput"))
  expect_equal(results_model_small, rms_expected)
})

# runSimulation ----
test_that("runSimulation works", {
  
  skip_if_not_connectable(session_globale)
  
  conn_eurodiad <- get_con(session_globale)
  
  # Use case
  selected_latin_name = "Alosa alosa"
  
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
  expect_equal(count(data_hsi_nmax) %>% pull(), 663300)
  #' @description reference_results exists
  reference_results <- data_simulation[["reference_results"]]
  expect_false(is.null(reference_results))
  expect_equal(ncol(reference_results), 7)
  expect_equal(count(reference_results) %>% pull(), 663300)
  #' @description data_ni0 exists
  data_ni0 <- data_simulation[["data_ni0"]]
  expect_false(is.null(data_ni0))
  expect_equal(ncol(data_ni0), 9)
  expect_equal(count(data_ni0) %>% pull(), 4422)
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
    yearsimubegin = 
      case_when(
        country == "France" ~ -log(.5),
        TRUE ~ 0
      ),
      # rep(-log(.5), length(datasets[["countries_mortalities_list"]])),
    yearsimuend = 
      case_when(
        country == "France" ~ -log(.75),
        TRUE ~ 0
      )
      # rep(-log(.75), length(datasets[["countries_mortalities_list"]]))
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
    verbose = TRUE
  )
  
  if (FALSE) {
    # for  debug tests only
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
