#' Get data ready for simulations on page 4
#'
#' @param conn_eurodiad Connection to database
#' @importFrom dplyr tbl sql inner_join select mutate arrange filter distinct
#'
#' @return list of tables necessary for simulations
#' @export
#'
#' @examples
#' \dontrun{
#' data_simulation <- get_data_simulation(conn_eurodiad)
#' data_simulation[["data_catchment"]]
#' data_simulation[["outlet_distance"]]
#' data_simulation[["hydiad_parameter"]]
#' data_simulation[["data_hsi_nmax"]]
#' data_simulation[["reference_results"]]
#' data_simulation[["data_ni0"]]
#' }
get_data_simulation <- function(conn_eurodiad) {
  # Catchment features ----
  data_catchment <- tbl(conn_eurodiad, sql("SELECT basin_id, basin_name, country, surface_area_drainage_basin as surface_area, ccm_area FROM diadesatlas.basin b
INNER JOIN diadesatlas.basin_outlet bo USING (basin_id)" )) 
  
  # Distances between catchment ----
  outlet_distance <- tbl(conn_eurodiad, sql("SELECT
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
	(arrival = b2.basin_id)"))
  
  # HyDiaD parameters ----
  hydiad_parameter <- tbl(conn_eurodiad, sql("
                                 SELECT s.latin_name AS \"latin_name_s\", s.local_name AS \"Lname_s\", h.* 
                                 FROM diadesatlas.hydiadparameter h
                                 INNER JOIN diadesatlas.species s USING (species_id)")) %>% 
    rename(latin_name = latin_name_s,
           Lname = Lname_s)
  
  # HSI  and Nmax ----
  # a query to load HSI for only 8.5 scenario (which do not change between simulations)
  query <- "SELECT s.latin_name, basin_id, basin_name, country, surface_area_drainage_basin as surface_area, year, climatic_scenario, climatic_model_code, hsi 
  FROM diadesatlas.hybrid_model_result hmr
  INNER JOIN diadesatlas.species s USING (species_id)
  INNER JOIN diadesatlas.basin b USING (basin_id)
  INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)
  WHERE year > 0"# AND climatic_scenario = 'rcp85'"
  
  data_hsi_nmax <- tbl(conn_eurodiad, sql(query)) %>%
    # tibble() %>%
    # compute the maximum abundance of adults (#) according to hsi,
    #   maximal density of spawners (Dmax) , catchment area, 
    #   population growth rate in ideal condition (lambda_1)
    inner_join(hydiad_parameter %>%
                 select("latin_name", "Dmax", "lambda_1"),
               by = join_by(latin_name)) %>%
    mutate(Nmax = hsi * Dmax * surface_area * lambda_1) %>%
    select(-c(Dmax, lambda_1))
  
  
  # initial abundance in catchments ----
  # > cf: https://github.com/inrae/diades.atlas/issues/109
#   query <- "SELECT s.latin_name, basin_id, basin_name, surface_area_drainage_basin as surface_area, year, climatic_scenario, climatic_model_code, nit, hsi  FROM diadesatlas.hybrid_model_result hmr
# INNER JOIN diadesatlas.species s USING (species_id)
# INNER JOIN diadesatlas.basin b USING (basin_id)
# INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)"
# WHERE  climatic_scenario = 'rcp85'"
  # AND year = 0
  # ORDER BY latin_name, basin_id, climatic_model_code"
  
  # tbl(conn_eurodiad, "v_hybrid_model_mavg") %>% 
  #   filter(year == 0)
  
  # data_ni0 <- tbl(conn_eurodiad, sql(query)) %>%
  #   filter(year == 0) %>% 
  #   # arrange(latin_name, basin_id, climatic_model_code) %>% 
  #   inner_join(hydiad_parameter %>%
  #                select("latin_name", "Dmax", "lambda_1"),
  #              by = join_by(latin_name)) %>%
  #   mutate(Nmax = hsi * Dmax * surface_area * lambda_1) %>%
  #   select(-c(surface_area, Dmax, lambda_1))
  #   

  # initial conditions to populate the model
  # percentage of HSI and Nmax medians
  data_ni0 <- 
    data_hsi_nmax %>% 
    filter(between(year, 1951, 1960)) %>% 
    # group by all but year, hsi and Nmax
    group_by(across(-c(year, hsi, Nmax))) %>%  
    summarise(across(c(hsi, Nmax), ~median(.x, na.rm = TRUE)),
             .groups = 'drop') %>% 
    inner_join(hydiad_parameter %>%  select(latin_name, pctMedian),
               by = join_by(latin_name)) %>% 
    mutate(across(c(hsi, Nmax), ~.x * pctMedian)) %>% 
    mutate(year = 0, .before = hsi) %>% 
    select(-pctMedian)

  
  # catchment of the surface
  catchment_surface <- data_hsi_nmax %>% 
    distinct(basin_name, surface_area)
    # arrange(basin_name) 

  
  # reference results ----
  query <- "SELECT s.latin_name, basin_id, basin_name, year, climatic_scenario, climatic_model_code, nit  
  FROM diadesatlas.hybrid_model_result hmr
  INNER JOIN diadesatlas.species s USING (species_id)
  INNER JOIN diadesatlas.basin b USING (basin_id)
  INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)
  --WHERE year > 0
  "
  
  reference_results <- tbl(conn_eurodiad, sql(query))
  
  # Return ----
  res <- list(
    data_catchment = data_catchment,
    outlet_distance = outlet_distance,
    hydiad_parameter = hydiad_parameter,
    data_hsi_nmax = data_hsi_nmax,
    reference_results = reference_results,
    data_ni0 = data_ni0,
    catchment_surface = catchment_surface
  )
}