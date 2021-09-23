#' db 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @param session The Shiny Session object
#'
#' @export
connect <- function(
  session = shiny::getDefaultReactiveDomain()
){
  
  session$userData$con <- dbConnect(
    Postgres(), 
    host = Sys.getenv("POSTGRES_HOST", get_golem_config("POSTGRES_HOST")),
    dbname = Sys.getenv("POSTGRES_DBNAME", get_golem_config("POSTGRES_DBNAME")),
    port = Sys.getenv("POSTGRES_PORT", get_golem_config("POSTGRES_PORT")),
    user = Sys.getenv("POSTGRES_USER", "diadesatlas_r"),
    password = Sys.getenv("POSTGRES_PASS", "diadesPassword"),
    options="-c search_path=diadesatlas"
  )
}

#' Title
#'
#' @param session Shiny session object
#'
#' @return
#' @export
#'
#' @examples
get_con <- function(
  session = shiny::getDefaultReactiveDomain()
){
  session$userData$con
}
