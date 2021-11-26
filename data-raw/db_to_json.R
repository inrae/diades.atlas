## code to prepare `db_to_json` dataset goes here

library(DBI)
library(dplyr)
library(dbplyr)
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "postgis",
  dbname = "diades",
  port = 5432,
  user = askpass::askpass("Please enter the user"),
  password = askpass::askpass("Please enter the password")
)

# List of case studies
casestudy <- tbl(con, in_schema("diadesatlas", "casestudy")) %>% collect()
# List of the corresponding basins
casestudy_basin <- tbl(con, in_schema("diadesatlas", "casestudy_basin")) %>% collect()
# Put them two together
cstd <- full_join(casestudy, casestudy_basin)
# We now have casestudy_id + casestudy_name + basin_id
cstd

# We'll use the basin_outlet, simplified geom, to draw on the map
qry <- "SELECT ST_Transform(diadesatlas.basin_outlet.simplified_geom, 4326) as geom, basin_id as basin_id FROM diadesatlas.basin_outlet"

# Query it as an sf object,
pols <- sf::st_read(con, query = qry, geom = "geom") %>%
  # keep only the basin which are in the case study,
  filter(basin_id %in% casestudy_basin$basin_id) %>%
  # Then add the info from cstd
  left_join(cstd) %>%
  filter(publishable)

# Save it as a geojson, so that we can reuse it on the leaflet map
pols %>%
  geojson::as.geojson() %>%
  geojson::geo_write("inst/casestudy.json")

# List of ecosystem services
ecosystems <- tbl(con, in_schema("diadesatlas", "ecosystem_service")) %>% collect()
# we'll only keep the lines where the species is present
ecosystems %>%
  filter(presence == 1, casestudy_id %in% casestudy$casestudy_id) %>%
  jsonlite::write_json("inst/ecosystems.json")

# Services
# Getting the services as is
services <- tbl(con, in_schema("diadesatlas", "cices")) %>% collect()
services %>%
  jsonlite::write_json("inst/services.json")

# Species
species <- tbl(con, in_schema("diadesatlas", "species")) %>% collect()
species %>%
  filter(species_id %in% ecosystems$species_id) %>%
  jsonlite::write_json("inst/species.json")