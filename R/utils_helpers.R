#' @importFrom utils read.csv
build_language_json <- function(session = shiny::getDefaultReactiveDomain()) {
  lg <- dplyr::bind_rows(
    read.csv(
      app_sys("translation.csv")
    ),
    read.csv(
      app_sys("translation_species.csv")
    ),
    read.csv(
      app_sys("translation_iucn.csv")
    ),
    read.csv(
      app_sys("translation_abundance_level.csv")
    ),
    read.csv(
      app_sys("translation_v_ecosystemic_services.csv") 
    ),
    read.csv(
      app_sys("translation_help.csv")
    )
  )

  build_entry <- function(subset) {
    x <- list(
      translation = as.list(lg[[subset]])
    )
    names(x$translation) <- lg$entry
    x
  }

  list(
    en = build_entry("en"),
    fr = build_entry("fr")
  ) %>% jsonlite::toJSON(auto_unbox = TRUE)
}

with_multilg <- function(fun, i18n, default) {
  purrr::partial(
    fun,
    label = with_i18(
      tags$span(
        default
      ),
      i18n
    )
  )
}

get_dt_lg <- function(lg) {
  list(
    url = switch(lg,
      en = "//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json",
      fr = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"
    )
  )
}

#' Title
#'
#' @param con The DB connection object
#'
#' @return
#' @export
#'
generate_datasets <- function(con) {
  cli::cat_rule("generate_datasets")
  dataCatchment <- DBI::dbReadTable(
    con,
    "v_abundance"
  ) %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~abundance_level_id, ~abundance_interpretation,
        1, "Not recorded in the period",
        2, "Occasional vagrants",
        3, "Functional populations",
        4, "Abundant functional populations"
      ) %>%
        dplyr::mutate(abundance_interpretation = factor(abundance_interpretation,
          levels = .$abundance_interpretation
        )),
      by = "abundance_level_id"
    )

  catchment_geom <- sf::st_read(
    con,
    query =   "SELECT * FROM diadesatlas.v_basin vb"
  ) %>%
    rmapshaper::ms_simplify()

  dataALL <- DBI::dbGetQuery(
    con,
    "SELECT * from diadesatlas.v_species_ices_occurence vsio "
  ) %>%
    # tibble() %>%
    dplyr::mutate(nb_occurence = as.integer(nb_occurence))

  ices_geom <- sf::st_read(
    con,
    query = "SELECT * FROM diadesatlas.v_ices_geom;"
  ) %>%
    # sf::st_transform("+proj=eqearth +wktext") %>%
    sf::st_transform("+proj=wintri") %>%
    rmapshaper::ms_simplify()

  species_order <- c(
    "Alosa alosa",
    "Alosa fallax",
    "Petromyzon marinus",
    "Lampetra fluviatilis",
    "Salmo salar",
    "Salmo trutta",
    "Acipenser sturio",
    "Osmerus eperlanus",
    "Anguilla anguilla",
    "Chelon ramada",
    "Platichthys flesus"
  )

  species_list <- DBI::dbGetQuery(
    con,
    "SELECT *, diadesatlas.translate(english_name, 'fr') AS french_name from diadesatlas.species WHERE active=TRUE"
  )

  species_list <- species_list[
    match(
      species_order,
      species_list$latin_name
    ),
  ]
  return(
    list(
      dataCatchment = dataCatchment,
      catchment_geom = catchment_geom,
      dataALL = dataALL,
      ices_geom = ices_geom,
      species_list = species_list
    )
  )
}