# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

cli::cat_rule("app.R")

se <- new.env()
connect(se)

# Run the application
run_app(species_list = get_active_species(se))
