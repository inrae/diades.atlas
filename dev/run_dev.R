# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
options(shiny.port = httpuv::randomPort())
# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

Sys.setenv("POSTGRES_USER" = "diadesatlas_r")
Sys.setenv("POSTGRES_PASS" = "diadesPassword")

# En dev, effacer la base Mongo pour éviter de voir les problèmes de graph rester !!!
local({
  fake_session <- new.env()
  diades.atlas:::launch_mongo(session = fake_session)
  fake_session$userData$mongo_cache$reset()
})

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
  ices_geom = ices_geom
)