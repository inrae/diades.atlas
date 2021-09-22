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

se <- new.env()
connect(se)

# Run the application
run_app(species_list = get_active_species(se))
