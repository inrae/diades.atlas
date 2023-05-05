#' @importFrom dplyr filter inner_join mutate
#' @noRd
data_ocean <- function(ices_geom,
                       spatial_type,
                       dataALL,
                       species_latin_name) {
  ices_geom %>%
    filter(ices_type == spatial_type) %>%
    inner_join(
      dataALL %>%
        select(-c(icesname, division_name)) %>%
        filter(latin_name == species_latin_name),
      by = c("ices_type", "gid")
    ) %>%
    mutate(prevalence = cut(
      nb_occurence,
      breaks = c(-Inf, 0, 3, 6, 9, 12, 15),
      labels = c(
        "Not recorded in the period" %>% with_i18("absent") %>% as.character(),
        "[1, 3]",
        "[4, 6]",
        "[7, 9]",
        "[10, 12]",
        "[13, 15]"
      )
    ))
}

#' @importFrom tmap tm_shape tm_polygons
#' @noRd
tm_ocean <- function(dataOcean,
                     title,
                     yearStart,
                     yearEnd) {
  tm_shape(dataOcean, bbox = bbox) +
    tm_polygons(
      "prevalence",
      title = paste0(title, "\n(", yearStart, "-", yearEnd, ")"),
      palette = c("#F7FBFF", "#C6DBEF", "#9ECAE1", "#4292C6", "#08519C", "#08306B"),
      n = 6,
      border.col = "gray90",
      labels = c(
        "Not recorded in the period" %>% with_i18("absent") %>% as.character(),
        "[1, 3]",
        "[4, 6]",
        "[7, 9]",
        "[10, 12]",
        "[13, 15]"
      ),
      popup.vars = c('in division:' = 'division_name', 'prevalence:' = 'nb_occurence')
    )
}

#' @importFrom tmap tm_shape tm_borders
#' @noRd
tm_ices_division <- function(ices_division) {
  tm_shape(ices_division) +
    tm_borders(col = 'grey70', lwd = 1)
}

#' @importFrom dplyr left_join filter
#' @noRd
data_continent <- function(catchment_geom,
                           dataCatchment,
                           species_latin_name) {
  catchment_geom %>%
    left_join(
      dataCatchment %>%
        filter(
          latin_name == species_latin_name
        ),
      by = "basin_id"
    )
}

#' @importFrom tmap tm_shape tm_polygons
#' @noRd
tm_catchmment <- function(dataContinent) {
  tm_shape(dataContinent) +
    tm_polygons(
      "abundance_interpretation",
      title = "Status in river catchments (1951-2010)" %>% with_i18("status_in_river_catchments") %>% as.character(),
      # "<span data-i18n='status_in_river_catchments'>Status in river catchments (1951-2010)</span>",
      palette = c("#FEE0D2", "#FCBBA1", "#A50F15", "#67000D"),
      n = 4,
      textNA = "Missing" %>% with_i18("missing") %>% as.character(),
      labels = c(
        "Not recorded in the period" %>% with_i18("absent") %>% as.character(),
        "Occasional vagrants" %>% with_i18("rare") %>% as.character(),
        "Functional populations" %>% with_i18("common") %>% as.character(),
        "Abundant functional populations" %>% with_i18("abundant") %>% as.character()
      ),
      popup.vars = c("population status: " = "abundance_interpretation")
    )
}

# Do it once
bbox <- sf::st_bbox(c(xmin = -17.5, xmax = 19, ymax = 36, ymin = 62), crs = sf::st_crs(4326))


#' Title
#'
#' @param species_latin_name The latin name of the species
#' @param spatial_type Geom to use in the map
#' @param con The Connection object
#' @param yearStart,yearEnd date used
#' @param dataCatchment,catchment_geom,dataALL,ices_geom,ices_division  internal datasets
#' @param session The Shiny Session object
#'
#' @return A tmap object
#' @export
#' @import sf
#' @import tmap
#'
tm_draw <- function(species_latin_name,
                    spatial_type,
                    con,
                    yearStart = 2003,
                    yearEnd = 2017,
                    dataCatchment,
                    catchment_geom,
                    dataALL,
                    ices_geom,
                    ices_division,
                    session = shiny::getDefaultReactiveDomain()) {
  # =====================================================================================
  # ----------------------------------------- country frontier
  # -----------------------------------------ices division border
  tm_ices_division <- get_tm_ices_division_m(
    session = session
  )(
    ices_division
  )
  #--------------------------- data in ocean
  dataOcean <- get_data_ocean_m(
    session = session
  )(
    ices_geom,
    spatial_type,
    dataALL,
    species_latin_name
  )

  title <- "Annual prevalence in catches at sea" %>%
    with_i18("annual_prevalence") %>%
    as.character()

  tm_ocean <- get_tm_ocean_m(
    session = session
  )(
    dataOcean,
    title,
    yearStart,
    yearEnd
  )

  # -------------------------------------------------------- data in catchment
  dataContinent <- get_data_continent_m(
    session = session
  )(
    catchment_geom,
    dataCatchment,
    species_latin_name
  )

  tm_catchmment <- get_tm_catchmment_m(
    session = session
  )(dataContinent)

  # ------------------------------------------ display the map
  tm_graticules() +
    tm_ocean +
    tm_ices_division +
    tm_frontiers +
    tm_catchmment +
    tm_layout(
      main.title.fontface = 3,
      main.title.size = 0.8,
      main.title = species_latin_name, # suppress at the end
      # legend.position = c("right", "center"),
      legend.outside = TRUE
    )
}
