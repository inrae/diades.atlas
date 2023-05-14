library(DBI)

library(tictoc)
library(tidyverse)


# connection to the data base  conn_eurodiad <- connect()

# data upload ----

# ---------------------------------------------------------------------- #
## Catchment features ----
data_catchment <- dbGetQuery(conn_eurodiad, 
  "SELECT 
    basin_id, 
    basin_name, 
    country, 
    surface_area_drainage_basin as surface_area,  
    ccm_area 
  FROM 
    diadesatlas.basin b
  INNER JOIN 
    diadesatlas.basin_outlet bo 
      USING (basin_id);" ) %>%
    tibble()

# ---------------------------------------------------------------------- #
## Distances between catchment ----
outlet_distance = dbGetQuery(conn_eurodiad, 
"SELECT
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
  

# ---------------------------------------------------------------------- #
# HyDiaD parameters ----
hydiad_parameter <-  
    dbGetQuery(conn_eurodiad, 
    "SELECT 
      s.latin_name, 
      s.local_name AS \"Lname\", 
      h.* 
     FROM 
      diadesatlas.hydiadparameter h
    INNER JOIN 
      diadesatlas.species s 
        USING (species_id);") %>%
    tibble() 
}


# ---------------------------------------------------------------------- #
## HSI  abd Nmax ----
# a query to load HSI for only 8.5 scenario (which do not change between simulations)
query = 
  "SELECT 
    s.latin_name,
    basin_id, 
    basin_name, 
    country, 
    surface_area_drainage_basin as surface_area, 
    year, 
    climatic_scenario, 
    climatic_model_code, 
    hsi 
  FROM 
    diadesatlas.hybrid_model_result hmr
  INNER JOIN 
    diadesatlas.species s 
      USING (species_id)
  INNER JOIN 
      diadesatlas.basin b 
        USING (basin_id)
  INNER JOIN 
    diadesatlas.climatic_model cm 
      USING (climatic_model_id)
  WHERE 
    year > 0 
    AND climatic_scenario = 'rcp85'"

data_hsi_nmax <- dbGetQuery(conn_eurodiad, query) %>%
    tibble() %>%
    # compute the maximum abundance (#) according to hsi,
    #   maximal density (Dmax) , catchment area (ccm_area)
    inner_join(hydiad_parameter %>%
                 select(latin_name, Dmax),
               by = 'latin_name') %>%
    mutate(Nmax = hsi * Dmax * surface_area) %>%
    select(-c(surface_area, Dmax))

rm(query)


# No ccm_area for  Bou_Regreg,   Loukkos,   Oum_er_Rbia,  Sebou. use surface_area_drainage_basin

# reference results
reference_results <- dbGetQuery(conn_eurodiad, 
  "SELECT 
    s.latin_name, 
    basin_id, 
    basin_name, 
    year, 
    climatic_scenario, 
    climatic_model_code, 
    nit  
  FROM 
    diadesatlas.hybrid_model_result hmr
  INNER JOIN 
    diadesatlas.species s 
      USING (species_id)
  INNER JOIN 
    diadesatlas.basin b 
      USING (basin_id)
  INNER JOIN 
    diadesatlas.climatic_model cm 
      USING (climatic_model_id)
  WHERE 
    year > 0 AND 
    climatic_scenario = 'rcp85' 
  ORDER BY 
    latin_name, 
    basin_id, 
    climatic_model_code") %>%
    tibble()
  
## initial abundance in catchments ----
data_ni0 <- 
  dbGetQuery(conn_eurodiad, 
"SELECT 
  s.latin_name, 
  basin_id, 
  basin_name, 
  surface_area_drainage_basin as surface_area, 
  year, 
  climatic_scenario, 
  climatic_model_code, 
  nit, 
  hsi  
FROM 
  diadesatlas.hybrid_model_result hmr
INNER JOIN 
  diadesatlas.species s 
    USING (species_id)
INNER JOIN 
  diadesatlas.basin b 
    USING (basin_id)
INNER JOIN 
  diadesatlas.climatic_model cm 
    USING (climatic_model_id)
WHERE  
  climatic_scenario = 'rcp85'
  AND year = 0
ORDER BY 
  latin_name, 
  basin_id, 
  climatic_model_code") %>%
    tibble() %>% 
    inner_join(hydiad_parameter %>%
                 select(latin_name, Dmax),
               by = 'latin_name') %>%
    mutate(Nmax = hsi * Dmax * surface_area) %>%
    select(-c(surface_area, Dmax))
