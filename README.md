
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

# Translation

Les instructions pour préparer la traduction sont dans
“dev/translation.Rmd”.  
Trois modes de traduction cohabitent:

-   Traductions présentes dans la base de données
-   Traductions à compléter dans des fichiers Markdown sous forme de
    Pull Request sur le projet
-   Traductions à compléter dans un fichier Google Sheet partagé

Dans tous les cas, les développeurs devront

-   avoir accès à la base de données PostGis (Voir “Backend requirement”
    ci-dessous)
-   suivre le contenu de “dev/translation.Rmd” lors de chaque mises à
    jour.

# Exploration of database out of Shiny application

You can use this package without opening the Shiny application.  
See vignette “00-exploration-of-data”

# Put in Production

Update Docker for deployment:
<https://gitlab.irstea.fr/diades/diades.atlas.deploy/>

# Backend requirement

## PostGis

To update the translations or to run the shiny application locally, the
PostGis database is required.

To be able to connect to a PostGis instance the user needs to define
some golem config parameters and environment variables.

### golem config parameters

Those are stored in `inst/golem-config.yml` and can be modified as such:

``` r
# database name
golem::amend_golem_config(
  "POSTGRES_DBNAME",
  "diades" # Changer ici le nom de la base de données
)
# database host URL
golem::amend_golem_config(
  "POSTGRES_HOST",
  "localhost"  # Changer ici l'URL de la base
)
# database port access
golem::amend_golem_config(
  "POSTGRES_PORT",
  "5432" # Changer ici le port
)
```

**If you change these lines to set the real values, be sure to set them
back with the examples below before commit**

### Environment variables

The POSTGRES_USER and POSTGRES_PASS **must not be versioned** with the
app.

To avoid setting those environment variables every time you want to run
the app, they should be saved in a local .Renviron file. To define one
for your own copy of the project run this command:

``` r
usethis::edit_r_environ(scope = "project")
```

and add the following lines

``` r
# database username with Read access
POSTGRES_USER=diadesatlas_owner # Changer ici le username
# database password for the user
POSTGRES_PASS=thinkrpassword # Changer ici le password
```

Again make sure not to track this project .Renviron with git.

### Dev - ThinkR PostGis DB

If you do not have direct access to the database, a copy can be used for
development. Please refer to the doc in the {diadesdata} repo on
ThinkR’s forge (restricted access): Section “Pull and Use”.

### Checks

Verify connexion to database works  
*Whether you are connected to the development or the production
database, this should work correctly*

``` r
pkgload::load_all()
session <- new.env()
connect(session)
con <- get_con(session)

DBI::dbListTables(con)
DBI::dbDisconnect(con)
```

## Dev - Mongo

To run the Shiny application locally, you need a Mongo database. For
dev, you need to run the following on your machine:

    docker run --name=mongo --rm -p 2811:27017 -e MONGO_INITDB_ROOT_USERNAME=Colin -e MONGO_INITDB_ROOT_PASSWORD=AsAboveSoBelow789123 mongo:4.0

Stop it at the end of your development process

    docker kill mongo

# Dev - update and include vignettes in the package

Vignettes are compiled manually by developers. Raw vignettes are stored
in “data-raw”. Instructions to compile them are in the vignette itself.

However, they can all be prepared from this script:

``` r
remotes::install_local(upgrade = "never", force = TRUE)

file.copy(here::here("dev", "translation.Rmd"),
          here::here("data-raw", "translation.Rmd"))

all_vignettes <- c(
  "aa-a-exploration_data",
  "aa-data-exploration-and-preparation",
  "bb-page1-catch-bycatch",
  "bc-page2-present",
  "bd-page3-climate-change",
  "be-page4-future", 
  "translation")

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
file.remove(here::here("data-raw", "translation.Rmd"))
```
