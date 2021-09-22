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
    host = get_golem_config("POSTGRES_HOST"),
    dbname = get_golem_config("POSTGRES_DBNAME"),
    port = get_golem_config("POSTGRES_PORT"),
    user = Sys.getenv("POSTGRES_USER", "diadesatlas_r"),
    password = Sys.getenv("POSTGRES_PASS", "diadesPassword"),
    options="-c search_path=diadesatlas"
  )
}

get_con <- function(
  session = shiny::getDefaultReactiveDomain()
){
  session$userData$con
}
