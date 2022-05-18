library(DBI)

library(tictoc)
# library(tidyverse)

# rm(list = ls())
connection_sql = TRUE


# connection to the data base
if (connection_sql)
  # conn_eurodiad <- dbConnect(RPostgres::Postgres(), dbname = 'eurodiad',
  #                            host = 'citerne.bordeaux.irstea.priv',
  #                            port = 5432, 
  #                            user = 'patrick.lambert',
  #                            password = rstudioapi::askForPassword("Database password"))
  conn_eurodiad <- connect()

# data upload ----

# ---------------------------------------------------------------------- #
## Catchment features ----
if (connection_sql) {
  data_catchment <- dbGetQuery(conn_eurodiad, "SELECT basin_id, basin_name, country, surface_area_drainage_basin as surface_area, ccm_area FROM diadesatlas.basin b
INNER JOIN diadesatlas.basin_outlet bo USING (basin_id);" ) %>%
    tibble()
  
  
  # write_rds(data_catchment, './data_input/data_catchment.rds')
} else {
  data_catchment <- read_rds('./data_input/data_catchment.rds')
}
# ---------------------------------------------------------------------- #
## Distances between catchment ----
if ( connection_sql) {
  outlet_distance = dbGetQuery(conn_eurodiad,"SELECT
	b.basin_name AS departure,
	od.departure AS departure_id,
	b2.basin_name AS arrival,
	od.arrival AS arrival_id,
	od.distance 
FROM
	diadesatlas.outlet_distance od
INNER JOIN diadesatlas.basin b ON
	(departure = b.basin_id)
INNER JOIN diadesatlas.basin b2 ON
	(arrival = b2.basin_id)
ORDER BY departure, distance ;") %>% 
   tibble() 
  
  # write_rds(outlet_distance, "./data_input/outletDistance.rds")
} else {
  outlet_distance <- read_rds( "./data_input/outletDistance.rds")
}

# ---------------------------------------------------------------------- #
# HyDiaD parameters ----
if (connection_sql) {
  hydiad_parameter <-  dbGetQuery(conn_eurodiad, "
                                 SELECT s.latin_name, s.local_name AS \"Lname\", h.* FROM diadesatlas.hydiadparameter h
INNER JOIN diadesatlas.species s USING (species_id);") %>%
    tibble() 
  
  # hydiad_parameter %>% write_rds("./data_input/HyDiaDParameter.rds")

} else {
  hydiad_parameter <- read_rds("./data_input/HyDiaDParameter.rds")
}


# ---------------------------------------------------------------------- #
## HSI  abd Nmax ----
if (connection_sql) {
  # a query to load HSI for only 8.5 scenario (which do not change between simulations)
  query = "SELECT s.latin_name, basin_id, basin_name, country, surface_area_drainage_basin as surface_area, year, climatic_scenario, climatic_model_code, hsi FROM diadesatlas.hybrid_model_result hmr
INNER JOIN diadesatlas.species s USING (species_id)
INNER JOIN diadesatlas.basin b USING (basin_id)
INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)
WHERE year > 0 AND climatic_scenario = 'rcp85'"

  data_hsi_nmax <- dbGetQuery(conn_eurodiad, query) %>%
    tibble() %>%
    # compute the maximum abundance (#) according to hsi,
    #   maximal density (Dmax) , catchment area (ccm_area)
    inner_join(hydiad_parameter %>%
                 select(latin_name, Dmax),
               by = 'latin_name') %>%
    mutate(Nmax = hsi * Dmax * surface_area) %>%
    select(-c(surface_area, Dmax))

  # write_rds(data_hsi_nmax, './data_input/data_hsi_Nmax.rds')

  rm(query)
} else {
  data_hsi_nmax <- read_rds('./data_input/data_hsi_Nmax.rds')
}


# No ccm_area for  Bou_Regreg,   Loukkos,   Oum_er_Rbia,  Sebou. use surface_area_drainage_basin

# reference results
if (connection_sql) {
  reference_results <- dbGetQuery(conn_eurodiad, 
                                 "SELECT s.latin_name, basin_id, basin_name, year, climatic_scenario, climatic_model_code, nit  FROM diadesatlas.hybrid_model_result hmr
INNER JOIN diadesatlas.species s USING (species_id)
INNER JOIN diadesatlas.basin b USING (basin_id)
INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)
WHERE year > 0 AND climatic_scenario = 'rcp85' 
ORDER BY latin_name, basin_id, climatic_model_code") %>%
    tibble()
  
  # write_rds(reference_results, './data_input/referenceResults.rds')
} else {
  reference_results <- read_rds('./data_input/referenceResults.rds')
}


## initial abundance in catchments ----
if (connection_sql) {
  data_ni0 <- dbGetQuery(conn_eurodiad, "SELECT s.latin_name, basin_id, basin_name, surface_area_drainage_basin as surface_area, year, climatic_scenario, climatic_model_code, nit, hsi  FROM diadesatlas.hybrid_model_result hmr
INNER JOIN diadesatlas.species s USING (species_id)
INNER JOIN diadesatlas.basin b USING (basin_id)
INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)
WHERE  climatic_scenario = 'rcp85'
AND year = 0
ORDER BY latin_name, basin_id, climatic_model_code") %>%
    tibble() %>% 
    inner_join(hydiad_parameter %>%
                 select(latin_name, Dmax),
               by = 'latin_name') %>%
    mutate(Nmax = hsi * Dmax * surface_area) %>%
    select(-c(surface_area, Dmax))

  # write_rds(data_ni0, './data_input/data_ni0.rds')
} else {
  data_ni0 <- read_rds('./data_input/data_ni0.rds')
}

#

