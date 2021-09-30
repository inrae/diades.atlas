#' @importFrom utils read.csv
build_language_json <- function(){
  lg <- read.csv(
    app_sys("translation.csv")
  )
  
  build_entry <- function(subset){
    x <- list(
      translation = as.list(lg[[subset]])
    )
    names(x$translation) <- lg$entry
    x
  }
  
  list(
    en = build_entry("en"),
    fr = build_entry("fr")
  ) %>% jsonlite::toJSON(auto_unbox =  TRUE)
  
}

with_multilg <- function(fun, i18n, default){
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

get_dt_lg <- function(lg){
  list(
    url = switch(
      lg, 
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
generate_datasets <- function(
  con
){
  cli::cat_rule("generate_datasets")
  dataCatchment <-  DBI::dbReadTable(
    con,  
    "v_abundance"
  ) %>% 
    dplyr::inner_join(
      dplyr::tribble(~abundance_level_id, ~abundance_interpretation,
              1, 'Not recorded in the period',
              2, 'Occasional vagrants',
              3, 'Functional populations',
              4, 'Abundant functional populations') %>% 
        dplyr::mutate(abundance_interpretation = factor(abundance_interpretation,
                                                 levels = .$abundance_interpretation)), 
      by = "abundance_level_id")
  
  catchment_geom <- sf::st_read(
    con, 
    query =   "SELECT * FROM diadesatlas.v_basin vb"
  ) %>%
    rmapshaper::ms_simplify()
  
  dataALL <- DBI::dbGetQuery(
    con, 
    "SELECT * from diadesatlas.v_species_ices_occurence vsio "
  ) %>% 
    #tibble() %>% 
    dplyr::mutate(nb_occurence = as.integer(nb_occurence))
  
  ices_geom <- sf::st_read(
    con, 
    query = "SELECT * FROM diadesatlas.v_ices_geom;"
  ) %>% 
    # sf::st_transform("+proj=eqearth +wktext") %>%
    sf::st_transform("+proj=wintri") %>%
    rmapshaper::ms_simplify()
  
  species_list <- con %>%
    dplyr::tbl( "species" ) %>% 
    dplyr::filter(active) %>%
    dplyr::collect()
  
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