# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

# Hide files
usethis::use_build_ignore("manifest.json")
usethis::use_git_ignore("dev/00-fill-database.html")
usethis::use_git_ignore(".Renviron")
usethis::use_build_ignore(".Renviron")
usethis::use_git_ignore("data-raw/translation.Rmd")
usethis::use_git_ignore("dev/translation.html")
usethis::use_git_ignore("dev/data-docker/")

## Dependencies ----
## Add one line by package you want to add as dependency
# remotes::install_github("miraisolutions/compareWith@feature/46-compare-git")

# Before sending to git external server
# _deps
# renv::install("ThinkR-open/attachment")
# [-] 7 package(s) removed: ggplot2, magrittr, maps, RPostgres, shinythemes, zeallot, dbplyr.

attachment::att_amend_desc(
  extra.suggests = c("pkgload", "DiagrammeR",
                     "DiagrammeRsvg", "dbplyr", "stringi", 
                     "rlang" # why ?
  )
)
checkhelper::print_globals()

# _renv ----
custom_packages <- setdiff(
  c(
    attachment::att_from_description(),
    attachment::att_from_rmds("data-raw"),
    "renv",
    "devtools", "roxygen2", "usethis",
    "testthat", "covr", "attachment",
    "pkgdown"
  ), "diades.atlas"
)

# _check
devtools::check()
# _snapshot when check ok
renv::snapshot(packages = custom_packages)

# After pull and/or rebase
renv::restore()

# Force installation from source of packages that need compilation
packages <- c(
  tmap = "3.3.2",
  lwgeom = "0.2.8",
  V8 = "3.4.2",
  testthat = "3.1.0",
  rgeos = "0.5-5",
  jqr = "1.2.1",
  NULL
)

install_version_from_source <- function(
  version,
  package
) {
  remotes::install_version(
    version = version,
    package = package,
    repos = getOption("repos")["CRAN"],
    type = "source",
    upgrade = "never"
  )
}
purrr::imap(packages, install_version_from_source)


# Remettre à zéro la bdd de cache
withr::with_envvar(
  c("GOLEM_CONFIG_ACTIVE" = "dev"),
  {
    fake_session <- new.env()
    launch_mongo(session = fake_session)
    fake_session$userData$mongo_cache$reset()
  }
)


## Add data for reprex
usethis::use_data_raw("World")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "first") # Name of the module
golem::add_module(name = "second") # Name of the module
golem::add_module(name = "third") # Name of the module
golem::add_module(name = "fourth") # Name of the module
golem::add_module(name = "species") # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("db")
golem::add_utils("helpers")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")

golem::use_external_js_file("https://bossanova.uk/jspreadsheet/v4/jexcel.js", "jexcel.js")
golem::use_external_js_file("https://jsuites.net/v4/jsuites.js", "jsuites.js")
golem::use_external_js_file("https://cdnjs.cloudflare.com/ajax/libs/notify/0.4.2/notify.js", "notify.js")
golem::use_external_css_file("https://jsuites.net/v4/jsuites.css", "jsuites.css")
golem::use_external_css_file("https://bossanova.uk/jspreadsheet/v4/jexcel.css", "jexcel.css")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "db_to_json", open = FALSE)
usethis::use_data_raw(name = "frontiers", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("fct_db")
golem::use_recommended_tests(spellcheck = FALSE)

# Documentation

## Vignette ----
usethis::use_vignette("aa-data-exploration") # moved to "data-raw/"
usethis::use_vignette("aa-data-exploration-compiled")
devtools::build_vignettes()

## Code Coverage ----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
# usethis::use_github_action("pkgdown")
# usethis::use_github_action("test-coverage")
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# Add action for PR
# usethis::use_github_action_pr_commands()

# Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()
#
# # AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()
#
# # Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")