#' Title
#'
#' @param species_latin_name The latin name of the species
#' @param spatial_type Geom to use in the map
#' @param con The Connection object
#' @param yearStart,yearEnd date used
#' @param dataCatchment,catchment_geom,dataALL,ices_geom  internal datasets
#'
#' @return
#' @export
#' @import sf
#' @import tmap
#'
#' @examples
tm_draw <- function(
  species_latin_name,
  spatial_type, 
  con,
  yearStart = 2003,
  yearEnd = 2017, 
  dataCatchment, 
  catchment_geom, 
  dataALL, 
  ices_geom
){
  #=====================================================================================
  # spatial coverage
  bbox =  st_bbox(c(xmin = -17.5, xmax = 19, ymax = 36, ymin = 62), crs = st_crs(4326)) 
  # ----------------------------------------- country frontier
  
  #--------------------------- data in ocean
  dataOcean <- ices_geom %>% 
    filter(ices_type == spatial_type) %>% 
    inner_join(
      dataALL %>% 
        filter(latin_name == species_latin_name
        ),
      by = c('ices_type', 'gid')
    ) %>% 
    mutate(prevalence = cut(
      nb_occurence, 
      breaks = c(-Inf, 0, 3, 6, 9 ,12, 15),
      labels = c(
        "Not recorded in the period", 
        "[1, 3]",
        "[4, 6]",
        "[7, 9]",
        "[10, 12]",
        "[13, 15]"
      )
    )
    )
  
  
  tm_ocean <- tm_shape(dataOcean,  bbox = bbox) + 
    tm_polygons(
      'prevalence',  
      title = paste0('Annual prevalence in catches at sea \n(', yearStart, '-', yearEnd, ')'),
      palette = "Blues", 
      n = 6
    ) 
  
  # -------------------------------------------------------- data in catchment
  dataContinent <- catchment_geom %>% 
    left_join(
      dataCatchment  %>% 
        filter(
          latin_name == species_latin_name
        ),
      by = "basin_id"
    )
  
  tm_catchmment <- tm_shape(dataContinent) +  
    tm_polygons(
      "abundance_interpretation", 
      title = 'Status in river catchments (1951-2010)',
      palette = "Reds",  
      n = 4
    ) 
  #tm_text('abundance_level_id', size = .5) 
  
  # ------------------------------------------ display the map
  tm_graticules() + 
    tm_ocean  + 
    tm_frontiers +  
    tm_catchmment +
    tm_layout(main.title.fontface = 3,
              main.title.size = 0.8,
              main.title = species_latin_name, # suppress at the end
              legend.position = c("right", "center"),
              legend.outside = TRUE) 
  
}