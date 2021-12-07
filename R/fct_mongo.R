#' mongo
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom bank cache_mongo
#' @import mongolite
launch_mongo <- function(session = getDefaultReactiveDomain()) {
    URI <- sprintf(
        "mongodb://%s:%s@%s:%s",
        Sys.getenv("MONGO_INITDB_ROOT_USERNAME", "Colin"),
        Sys.getenv("MONGO_INITDB_ROOT_PASSWORD", "AsAboveSoBelow789123"),
        Sys.getenv("MONGOURL", get_golem_config("mongourl")),
        Sys.getenv("MONGOPORT", get_golem_config("mongoport"))
    )
    bank::cache_mongo$new(
        db = get_golem_config("mongodb"),
        url = URI,
        prefix = "diades"
    )
}
# get_mongo <- function(session = getDefaultReactiveDomain()) {
#     session$userData$mongo_cache
# }