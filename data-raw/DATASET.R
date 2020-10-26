## code to prepare fake datasets for reprex

library(dplyr)
# All watersheds with fake coordinates ----
fake_watershed_data <- tibble(
  name = c("Mondego", "Minho", "Ulla", "Gipuzkoa rivers", "Gironde-Garonne-Dordogne", 
        "Loire", "BAWI Normandy-Brittany", "Frome-Tamar-Taff",
        "Barroe-Nore-Suir Waterford Bay"),
  lat = round(runif(9, 36.3, 57.6), 6),
  long = round(runif(9, -10, 3), 6)
)
# All species ----
fish <- c("tarpon", "saboga", "sea lamprey",
          "river lamprey", "eel", "salmon", 
          "sea trout", "European sturgeon", "red mullet", 
          "flounder", "smelt")

# Use codes to allow for translations ----
watershed_code <- paste0("shed-", tolower(gsub(" ", "", fake_watershed_data$name)))
species_code <- paste0("fish-", tolower(gsub(" ", "", fish)))
service_code <- paste0("service-", LETTERS[1:10])

nb_individuals <- 50

# Simulate services data ----
fake_services_data <- tibble(
  watershed_code = sample(x = watershed_code, size = nb_individuals, replace = TRUE),
  species_code = sample(species_code, nb_individuals, replace = TRUE),
  service_code = sample(service_code, nb_individuals, replace = TRUE),
  value = sample(1:10, nb_individuals, replace = TRUE)
) %>% 
  # Not twice the same line
  distinct() %>% 
  # Arrange for better read
  arrange(
    watershed_code, species_code, service_code
  )

# Store fake dataset for reprex
usethis::use_data(fake_watershed_data, overwrite = TRUE)
usethis::use_data(fake_services_data, overwrite = TRUE)

# Add or update documentation for fake datasets
# usethis::use_r("fake_watershed_data")
# usethis::use_r("fake_services_data")

