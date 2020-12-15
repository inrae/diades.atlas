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

## Dependencies ----
## Add one line by package you want to add as dependency
#usethis::use_package( "thinkr" )

# Before sending to git external server
# _deps
attachment::att_amend_desc(extra.suggests = "pkgload")
# _renv
custom_packages <- c(attachment::att_from_description(),
                     "renv",
                     "devtools", "roxygen2", "usethis",
                     "testthat", "covr", "attachment",
                     "pkgdown")
# _check
devtools::check()
# _snapshot when check ok
renv::snapshot(packages = custom_packages)


# After pull and/or rebase
renv::restore()


## Add data for reprex
usethis::use_data_raw()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "first" ) # Name of the module
golem::add_module( name = "second" ) # Name of the module
golem::add_module( name = "third" ) # Name of the module
golem::add_module( name = "fourth" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "ui" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("aa-data-exploration")
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
usethis::use_github_action("pkgdown") 
usethis::use_github_action("test-coverage") 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
# usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

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

