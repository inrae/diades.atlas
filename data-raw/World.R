## code to prepare `World` dataset goes here
data("World", package  = "tmap")
usethis::use_data(World, overwrite = TRUE)
rm(World)
