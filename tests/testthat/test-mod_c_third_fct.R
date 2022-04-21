test_that("create_ui_summary_html works", {
 expect_equal(
   as.character(
     create_ui_summary_html(species = "Alosa alosa", date = "2020",
                            basin_name = "Loire", country = "France")
   ),
   "<span data-i18n='alosa-alosa'>Alosa alosa</span> / 2020 / Loire - France"
 ) 
})

