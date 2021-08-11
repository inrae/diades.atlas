
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {diades.atlas}

<!-- badges: start -->

[![R build
status](https://github.com/inrae/diades.atlas/workflows/R-CMD-check-docker-renv/badge.svg)](https://github.com/inrae/diades.atlas/actions)
[![Codecov test
coverage](https://codecov.io/gh/inrae/diades.atlas/branch/main/graph/badge.svg)](https://codecov.io/gh/inrae/diades.atlas?branch=main)
<!-- badges: end -->

A Shiny application to explore data.

# Installation

``` r
remotes::install_github('inrae/diades.atlas')
```

# Websites for validations :

-   Package documentation : <https://inrae.github.io/diades.atlas/>
-   Package documentation (development version) :
    <https://inrae.github.io/diades.atlas/dev/>
-   Shiny App : <https://connect.thinkr.fr/diadesatlasui18n/>

# Backend requirement

The apps needs to connect to a db:

``` r
## Launch MySQL container ----
db_docker <- "kartoza/postgis:13.0"
persistant_db <- fs::path_abs(here::here("data-raw/db"))
fs::dir_create(persistant_db)
usethis::use_git_ignore("data-raw/db")

golem::amend_golem_config(
  "POSTGRES_DBNAME",
  "diades"
)
golem::amend_golem_config(
  "POSTGRES_HOST",
  "localhost"
)
golem::amend_golem_config(
  "POSTGRES_PORT",
  5432
)
Sys.setenv("POSTGRES_USER" = "diadesatlas_owner")
Sys.setenv("POSTGRES_PASS" = "thinkrpassword")
```

For dev purpose

``` r
# Databases container
## Pull image
pkgload::load_all()
system(paste("docker pull", db_docker))

dump_path <- normalizePath("~/diades.atlas.data/")

system(
  paste0(
    'docker run --rm',
    ' --name=postgis -d ',
    ' -p ',get_golem_config("POSTGRES_PORT"),':5432 ',
    ' -e POSTGRES_USER=',Sys.getenv("POSTGRES_USER","diadesatlas_owner"),
    ' -e POSTGRES_PASS=',Sys.getenv("POSTGRES_PASS","thinkrpassword"),
    ' -e POSTGRES_DBNAME=', get_golem_config("POSTGRES_DBNAME"),
    ' -v "', persistant_db, ':/var/lib/postgresql" ',
    ' -v "', dump_path, ':/home/postgis/data" ',
    db_docker
  )
)

# integrate the dataset 

# Killing the db
system(
  "docker kill postgis"
)
```

# Dev - vignettes

Vignettes are compiled manually by developers. Raw vignettes are stored
in “data-raw”. Instructions to compile them are in the vignette itself.

However, they can all be prepared from this script:

``` r
remotes::install_local(upgrade = "never")

all_vignettes <- c("aa-data-exploration-and-preparation",
                   "bb-page1-catch-bycatch",
                   "bc-page2-present",
                   "bd-page3-climate-change",
                   "be-page4-future")

for (vignette_name in all_vignettes) {
  # vignette_name <- "aa-data-exploration-and-preparation"
  vignette_file <- paste0(vignette_name, ".Rmd")
  
  rmarkdown::render(
    input = here::here(file.path("data-raw", vignette_file)),
    output_format = "rmarkdown::html_vignette",
    output_options = list(toc = TRUE),
    output_file = here::here(file.path("vignettes", vignette_file))
  )
  
  # Add header for title
  lines <- readLines(here::here(file.path("vignettes", vignette_file)))
  
  cat(
    glue::glue('---
title: ".{vignette_name}."
output: rmarkdown::html_vignette
vignette: >
  %\\VignetteIndexEntry{.{vignette_name}.}
  %\\VignetteEngine{knitr::rmarkdown}
  %\\VignetteEncoding{UTF-8}
---
', .open = ".{", .close = "}."),
lines,
sep = "\n", 
file = here::here(file.path("vignettes", vignette_file))
  )
}
```
