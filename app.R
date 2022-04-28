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
library(zeallot)
c(
  dataCatchment,
  catchment_geom,
  dataALL,
  ices_geom,
  species_list
) %<-% generate_datasets(
  get_con(
    se
  )
)

# Run the application
run_app(
  species_list = species_list,
  dataCatchment = dataCatchment,
  catchment_geom = catchment_geom,
  dataALL = dataALL,
  ices_geom = ices_geom,
  help_bubble_entries = get_help_bubble_entries()
)