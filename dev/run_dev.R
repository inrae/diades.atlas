# Set options here

# App mod dev/prod. To set dev or production mod modify both lines:
Sys.setenv("GOLEM_CONFIG_ACTIVE" = "dev") # "dev"/"production"
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Allocate random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Set environment variable for postgis db
Sys.setenv("POSTGRES_USER" = "diadesatlas_owner")
Sys.setenv("POSTGRES_PASS" = "thinkrpassword")

# En dev, effacer la base Mongo pour éviter de voir les problèmes de graph rester !!!
withr::with_envvar(
  c("GOLEM_CONFIG_ACTIVE" = "dev"),
  {
    fake_session <- new.env()
    launch_mongo(session = fake_session)
    fake_session$userData$mongo_cache$reset()
  }
)

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
