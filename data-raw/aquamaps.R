## code to prepare `aquamaps` dataset goes here

# # installation de raquamapas and aquamaps data
# remotes::install_github("raquamaps/raquamaps", dependencies = TRUE)
# # 4: cachem  (1.0.7 -> 1.0.8) [CRAN]
# # 5: waldo   (0.4.0 -> 0.5.0) [CRAN]
# # 6: later   (1.3.0 -> 1.3.1) [CRAN]
# # 7: profvis (0.3.7 -> 0.3.8) [CRAN]
# remotes::install_github("raquamaps/aquamapsdata", dependencies = TRUE)
# # 4: later  (1.3.0 -> 1.3.1) [CRAN]
# # 5: cachem (1.0.7 -> 1.0.8) [CRAN]
# # 6: waldo  (0.4.0 -> 0.5.0) [CRAN]
# 
# library(raquamaps) %>% suppressPackageStartupMessages()
# library(aquamapsdata) %>% suppressPackageStartupMessages()
# # library(terra)
# library(stringr)
# # library(sf)
# library(tmap)
# 
# # initial run-once step required to install remote db locally
# download_db(force = TRUE)

# active the connection to the downloaded database:
default_db("sqlite")

rm( list = ls())
species_list <- c("Alosa alosa", "Alosa fallax",  "Salmo salar",  "Salmo trutta", 'Acipenser sturio', 'Lampetra fluviatilis', 'Petromyzon marinus', 'Osmerus eperlanus',
                  'Platichthys flesus', 'Chelon ramada', 'Anguilla anguilla')

occurence_threshold = 0.3

# spatial coverage
bbox =  sf::st_bbox(c(xmin = -17.5, xmax = 18, ymin = 36, ymax = 62), crs = st_crs(4326)) 

aquamaps_species  = list()
for (species in species_list) {
  species_key <- am_search_exact(Genus = str_split_fixed(species, " ", n = 2)[1], 
                                 Species = str_split_fixed(species, " ", n = 2)[2])$SpeciesID %>% 
    suppressMessages()
  species_suitable <- am_raster(species_key, resolution  = 0.5) %>% 
    terra::crop(bbox) 
  
  # presence above the occurence_threshold (usually 0.3)
  terra::values(species_suitable)  <-  
    ifelse(terra::values(species_suitable) < occurence_threshold, NA, 1)
  
####  DOES NOT WORK ####  
  # tm_distribution <- species_suitable %>% 
  #   terra::rast() %>% 
  #   terra::as.polygons(dissolve = TRUE) 
  # plot(tm_distribution)
  # 
  # tm_distribution %>% 
  #   tm_shape() +
  #   tm_polygons(col = "f0d97d", 
  #               alpha = 0.2,
  #               legend.show = FALSE)
  #   
########################################  
    aquamaps_species[species] <- list(species_suitable)
}

usethis::use_data(aquamaps_species, overwrite = TRUE)
