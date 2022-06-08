# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Sys.setenv(
#   "GOLEM_CONFIG_ACTIVE" = "dev"
# )

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

cli::cat_rule("app.R")

se <- new.env()
connect(se)

datasets <- generate_datasets(
  get_con(
    se
  )
)

# Run future
cli::cat_line("Start multisession")
library(promises)
library(future)
plan(multisession(workers = min(availableCores()-1, 8)))

# Run the application
run_app(
  species_list = datasets[["species_list"]],
  countries_mortalities_list = datasets[["countries_mortalities_list"]],
  dataCatchment = datasets[["dataCatchment"]],
  catchment_geom = datasets[["catchment_geom"]],
  dataALL = datasets[["dataALL"]],
  ices_geom = datasets[["ices_geom"]],
  help_bubble_entries = get_help_bubble_entries()
)