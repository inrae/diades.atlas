#' mongo
#'
#' @description A fct function
#'
#' @param session Shiny session
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bank cache_mongo
#' @import mongolite
#' @export
launch_mongo <- function(session = getDefaultReactiveDomain()) {
    URI <- sprintf(
        "mongodb://%s:%s@%s:%s",
        Sys.getenv("MONGO_INITDB_ROOT_USERNAME", "Colin"),
        Sys.getenv("MONGO_INITDB_ROOT_PASSWORD", "AsAboveSoBelow789123"),
        Sys.getenv("MONGOURL", get_golem_config("mongourl")),
        Sys.getenv("MONGOPORT", get_golem_config("mongoport"))
    )
    tryCatch(
    session$userData$mongo_cache <- bank::cache_mongo$new(
        db = get_golem_config("mongodb"),
        url = URI,
        prefix = "diades"
    # ), error = function(e) stop("MongoDB has not been started or does not exist")
    ), error = function(e) message("MongoDB has not been started or does not exist")
    )
    
    if (is.null(session$userData$mongo_cache)) {
      session$userData$mongo_cache <- cachem::cache_mem(max_size = 1024 * 1024^2)
      message("Using local RAM memory for this session")
    } else {
      message("Connection to Mongo is successful")
    }

    session$userData$data_ocean_m <- memoise::memoise(
        data_ocean,
        cache = session$userData$mongo_cache
    )

    session$userData$tm_ocean_m <- memoise::memoise(
        tm_ocean,
        cache = session$userData$mongo_cache
    )
   
    session$userData$tm_positive_catch_m <- memoise::memoise(
      tm_positive_catch,
      cache = session$userData$mongo_cache
    )
    
    session$userData$tm_aquamaps_m <- memoise::memoise(
      tm_aquamaps,
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
    session$userData$tm_ices_division_m <- memoise::memoise(
      tm_ices_division,
      cache = session$userData$mongo_cache
    ) 
    session$userData$tm_ices_rectangle_m <- memoise::memoise(
      tm_ices_rectangle,
      cache = session$userData$mongo_cache
    ) 
}
get_data_ocean_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$data_ocean_m
}
get_tm_ocean_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$tm_ocean_m
}
get_tm_ices_division_m <- function(session = getDefaultReactiveDomain()) {
  session$userData$tm_ices_division_m
}
get_tm_ices_rectangle_m <- function(session = getDefaultReactiveDomain()) {
  session$userData$tm_ices_rectangle_m
}
get_tm_positive_catch_m <- function(session = getDefaultReactiveDomain()) {
  session$userData$tm_positive_catch_m
}
get_tm_aquamaps_m <- function(session = getDefaultReactiveDomain()) {
  session$userData$tm_aquamaps_m
}
get_data_continent_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$data_continent_m
}
get_tm_catchmment_m <- function(session = getDefaultReactiveDomain()) {
    session$userData$tm_catchmment_m
}
