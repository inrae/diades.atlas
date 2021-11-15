#' Get Active species
#'
#' @param session The Shiny Session object
#'
#' @return a tbl with active species
#' @export
#'
#' @examples
#' if (interactive()) {
#'   get_active_species()
#' }
get_active_species <- function(session = shiny::getDefaultReactiveDomain()) {
  get_con(
    session = session
  ) %>%
    dplyr::tbl("species") %>%
    dplyr::filter(active) %>%
    dplyr::collect()
}

#' Title
#'
#' @param species_id Id of the species to look for.
#' @param con The Connection object
#'
#' @return
#' @export
#'
#' @examples
#' if (interactive()) {
#'   get_conservation_status()
#' }
get_conservation_status <- function(species_id,
                                    con) {
  sql <- "select species_id, diadesatlas.translate(english_name, 'fr') as fish_name,
array_to_string (
array_agg(
diadesatlas.translate(iucn_classification_code, 'fr') || ': ' || diadesatlas.translate(iucn_level_name,'fr'))
, '<br>')
from diadesatlas.v_iucn
where species_id = ?id
group by species_id, fish_name
;"
  DBI::dbGetQuery(
    con,
    DBI::sqlInterpolate(con, sql, id = species_id)
  )
}