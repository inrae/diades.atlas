#' Get Active species
#'
#' @param session The Shiny Session object
#'
#' @return a tbl with active species
#' @export
#'
#' @examples
#' if (interactive()){
#'   get_active_species()
#' }
get_active_species <- function(
  session = shiny::getDefaultReactiveDomain()
){
  get_con(
    session = session
  ) %>% 
    dplyr::tbl( "species" ) %>% 
    dplyr::filter(active) %>%
    dplyr::collect()
}