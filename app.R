# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE )

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

# En dev, effacer la base Mongo pour éviter de voir les problèmes de graph rester !!!
if (isTRUE(getOption("golem.app.prod"))) {
  Sys.setenv(
    "R_CONFIG_ACTIVE" = "dev"
  )
  
  fake_session <- new.env()
  diades.atlas:::launch_mongo(session = fake_session)
  fake_session$userData$mongo_cache$reset()
  # fake_session$userData$mongo_cache$remove()
  rm(fake_session)
}

# Run the application
run_app(      
  species_list = species_list,
  dataCatchment = dataCatchment, 
  catchment_geom = catchment_geom, 
  dataALL = dataALL, 
  ices_geom = ices_geom
)
