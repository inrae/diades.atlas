## code to prepare `frontiers` dataset goes here
# fontierers on AA and north sea 

library(tidyverse)
library(rworldmap)
library(rworldxtra)
library(tmap)
library(sf)

worldMap <- getMap(resolution = "low") %>% st_as_sf()

states = c("Portugal", "Spain","France", "United Kingdom", "Ireland", "United Kingdom", "Belgium",
           "Netherlands", "Germany", "Denmark", "Sweden", "Norway", "Poland", "Switzerland","Italy",
           "Luxembourg"  , "Austria", "Finland", 'Greece',
           'Slovenia', 'Hungary' , 'Croatia', 'Slovakia', 'Czech Rep.', 'Bulgaria', 'Romania',
           'Albania', 'Bosnia and Herz.', 'Serbia', 'Montenegro', 'Macedonia', 'Kosovo',
           'Tunisia',  'Libya' ,  'Algeria' , 'Morocco' )


frontiers <- worldMap %>%  select(NAME, geometry) %>% filter(NAME %in% states)

bbox <- st_bbox(c(xmin = -17.5, xmax = 19, ymax = 36, ymin = 62), crs = st_crs(4326)) 
# world_map_crs <- "+proj=eqearth +wktext"
world_map_crs <- "+proj=wintri"

frontiers <- frontiers %>% 
  st_transform(world_map_crs) %>% 
  st_make_valid()
  # rmapshaper::ms_simplify()

usethis::use_data(frontiers, overwrite = TRUE)

S <- tm_shape(frontiers, bbox = bbox) +
  tm_polygons(alpha = 1, col = 'grey90')

usethis::use_data(tm_frontiers, overwrite = TRUE)
