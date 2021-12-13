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
    session$userData$mongo_cache <- bank::cache_mongo$new(
        db = get_golem_config("mongodb"),
        url = URI,
        prefix = "diades"
    )

    session$userData$data_ocean_m <- memoise::memoise(
        data_ocean,
        cache = session$userData$mongo_cache
    )

    session$userData$tm_ocean_m <- memoise::memoise(
        tm_ocean,
        cache = session$userData$mongo_cache
    )

    session$userData$data_continent_m <- memoise::memoise(
        data_continent,
        cache = session$userData$mongo_cache
    )
    session$userData$tm_catchmment_m <- memoise::memoise(
        tm_catchmment,
        cache = session$userData$mongo_cache
    )
}
get_data_ocean_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$data_ocean_m
}
get_tm_ocean_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$tm_ocean_m
}
get_data_continent_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$data_continent_m
}
get_tm_catchmment_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$tm_catchmment_m
}