test_that("create_ui_summary_html works", {
  expect_equal(
    as.character(
      create_ui_summary_html(species = "Alosa alosa", date = "2020",
                             basin_name = "Loire", country = "France")
    ),
    "<span data-i18n='alosa-alosa'>Alosa alosa</span> / 2020 / Loire - France"
  ) 
})

# get_hybrid_model & plot_hsi_nit ----
# Example with models
# se <- new.env()
# conn_eurodiad <- connect(session = se)

# load data included in `get_hybrid_model()`
test_that("get_hybrid_model & plot_hsi_nit work", {
  skip_if_not_connectable(session_globale)
  
  model_res <- get_hybrid_model(
    species_id = c(6), scenario = 'rcp85',
    lg = "fr",
    session = session_globale)
  
  expect_true(all(model_res[["latin_name"]] == "Alosa alosa"))
  expect_true(all(model_res[["climatic_scenario"]] == "rcp85"))
  
  gg <- plot_hsi_nit(model_res, selected_year = 2000,
               selected_bv = c(14), withNitStandardisation = FALSE)
  
  expect_is(gg, "patchwork")
  
})

# DBI::dbDisconnect(conn_eurodiad)
