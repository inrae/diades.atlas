# ==== Run this to create the dput for unit tests ====
library(tictoc)
library(purrr)
# library(Rfast)
library(Matrix)
library(data.table)
# library(tidyverse)

rm(list = ls())

source('data-raw/preparation_atlas_simulation.R')

# data upload ----
# ---------------------------------------------------------------------- #
## HyDiaD parameter ----

hydiad_parameter  %>% 
  print()


# Anthropogenic mortality ----
# build from sliders in interface 
# here fake data
anthropogenic_mortality = 
  expand_grid(data_hsi_nmax %>% 
                collect() %>% 
                distinct(year),
              data_hsi_nmax %>% 
                collect() %>% 
                distinct(country)) %>% 
  mutate( h1 = 0, h2 = 0) %>% 
  mutate(h1 = ifelse(between(year, 2001, 2050) & country == 'France', 
                     -log(.5), h1),
         h1 = ifelse(between(year, 2051, 2100) & country == 'France', 
                     -log(.75), h1))

dput(anthropogenic_mortality, file = "tests/testthat/anthropogenic_mortality_dput")

# anthropogenic_mortality %>% 
#   filter(country == 'France') %>% 
#   ggplot(aes(x = year, y = exp(-h1))) +
#   geom_path()

#=> In get_data_simulation()
catchment_surface <- data_hsi_nmax %>%
  distinct(basin_name) %>%
  arrange(basin_name) %>%
  inner_join(data_catchment %>%  select(basin_name, surface_area),
             by = 'basin_name')
dput(catchment_surface, file = "tests/testthat/catchment_surface_dput")

# =========================================================================================
# run simulation ----
selected_latin_name = "Alosa alosa"

runSimulation_pml = function(selected_latin_name, hydiad_parameter, anthropogenic_mortality,
                         catchment_surface, data_hsi_nmax, data_ni0,  outlet_distance, verbose = FALSE) {
  if (verbose) tic()
  
  # --------------------------------------------------------------------------------------- #
  results = list()
  
  # ---------------------------------------------------------------------- #
  # Local variables ----
  ## ordered list of basin s----
  basins <- data_hsi_nmax %>% 
    collect() %>% 
    distinct(basin_name) %>%
    arrange(basin_name) %>% 
    pull(basin_name)
  
  ## list of models ----
  models <- data_hsi_nmax %>% 
    collect() %>% 
    distinct(climatic_model_code) %>%
    arrange(climatic_model_code) %>% 
    pull(climatic_model_code)
  
  ## HyDiaD parameters for the selected species ----
  parameter <- hydiad_parameter %>% 
    collect() %>% 
    filter(latin_name == selected_latin_name )
    # filter(latin_name_s == selected_latin_name )
  results[['param']][['hydiad_parameter']] <- parameter
  
  ##  cohorts in  spawner run (number and weights) ----
  nbCohorts <-  parameter$nbCohorts
  cohortWeight = matrix(rep(1/nbCohorts, nbCohorts), ncol = 1)
  
  ## generation time : number or years contributing to a spawner run ----
  generationtime <- floor(parameter$AgeFirstMat + (nbCohorts / 2))
  
  ## nb of years in the 'burn-in' period (let the model stabilizing after model population) ----
  burnin <- 10
  
  ## first year of simulation ----
  firstYear = min(data_hsi_nmax$year, na.rm = TRUE)
  # data_hsi_nmax %>% collect() %>% pull(year) %>% min(., na.rm = TRUE)
  
  # ---------------------------------------------------------------------- #
  # Local matrices ----
  ## the spawnerTo  that are half active in reproduction (Allee effect) ----
  spawnersTo_50 <-  parameter$lambda * parameter$Dmax * catchment_surface$surface_area
  
  ## update Nmax according to anthropogenic mortality eh1 = exp(-h1) ----
  Nit <- data_hsi_nmax %>% 
    collect() %>% 
    filter(latin_name == selected_latin_name) %>% 
    mutate(phase = 'simul') %>% 
    # update maximal abundance (Nmax) with anthropogenic mortality (h1)
    inner_join(anthropogenic_mortality %>% 
                 select(country, year, h1),
               by = c('country', 'year')) %>% 
    mutate(Nmax_eh1 = Nmax * exp(-h1), 
           Nit = 0)
  
  # anticipate simulation to initialise model and to burn-in run 
  anticipation <-  
    expand_grid( data_ni0 %>% 
                   collect() %>% 
                   filter(latin_name == selected_latin_name) %>%  
                   select(-c(year) ),
                 tibble(year = seq(firstYear - burnin - generationtime, firstYear - 1), 
                        phase = c(rep('initial', generationtime), rep('burnin', burnin)))) %>% 
    # initial value (HSI predict with 10-year average of environmental factors)
    mutate(Nmax_eh1 = Nmax ,
           Nit =  ifelse(phase == 'initial', Nmax, 0))
  
  extendedNit_PML <- extendedNit <- Nit %>% 
    bind_rows(anticipation) %>% 
    arrange(basin_id, climatic_model_code, year)
  
  # extendedNit %>% 
  #   filter(basin_name == "Aa", year == 1951) %>% 
  #   View()
  # 
  
  ## list years in simulation ----
  years <- extendedNit %>%  
    distinct(year, phase) %>%  
    arrange(year)
  results[['param']][['years']] <- years 
  
  if (verbose) toc()
  
  if (verbose) tic()
  # ------------------------------------------------------------------------------- #
  ## compute Nmax_eh1 matrix and prepare Nit matrix  ----
  resultsPM <- results[["model"]] <- lapply(models, function(model) {
    out = list()
    
    out[['HSI']] <- 
      extendedNit %>% 
      filter(climatic_model_code == model) %>% 
      pivot_wider(id_cols = basin_name,
                  names_from = year,
                  values_from = hsi) %>% 
      arrange(basin_name) %>% 
      column_to_rownames('basin_name') %>% 
      as.matrix()
    
    out[['Nmax_eh1']] <- 
      extendedNit %>% 
      filter(climatic_model_code == model) %>% 
      pivot_wider(id_cols = basin_name,
                  names_from = year,
                  values_from = Nmax_eh1) %>% 
      arrange(basin_name) %>% 
      column_to_rownames('basin_name') %>% 
      as.matrix()
    
    out[['Nit']] <- 
      extendedNit %>% 
      filter(climatic_model_code == model) %>% 
      pivot_wider(id_cols = basin_name,
                  names_from = year,
                  values_from = Nit) %>% 
      arrange(basin_name) %>% 
      column_to_rownames('basin_name') %>% 
      as.matrix() 
    
    out[['emigrants']] <- matrix(0, 
                                 nrow = nrow(out[['Nit']]),
                                 ncol = ncol(out[['Nit']]), 
                                 dimnames = list(rownames(out[['Nit']]), colnames(out[['Nit']])))
    
    out[['immigrants']] <- matrix(0, 
                                  nrow = nrow(out[['Nit']]),
                                  ncol = ncol(out[['Nit']]), 
                                  dimnames = list(rownames(out[['Nit']]), colnames(out[['Nit']])))
    
    out[['spawnersTo']] <- matrix(0, 
                                  nrow = nrow(out[['Nit']]),
                                  ncol = ncol(out[['Nit']]), 
                                  dimnames = list(rownames(out[['Nit']]), colnames(out[['Nit']])))
    
    
    return(out)
  } )
  
  names(results[["model"]]) <-  models
  # --------------------------------------------------------------------------------------- #
  ## r * exp(-h2) matrix ----
  
  # compute exp(-h2) 
  eh2_PML <- eh2 <-  extendedNit %>% distinct(year, basin_name, country) %>% 
    left_join(anthropogenic_mortality ,
              by = c('country', 'year')) %>% 
    select(-c(country, h1)) %>% 
    mutate(r_eh2 = exp(-h2)) %>% 
    pivot_wider(id_cols = basin_name,
                names_from = year,
                values_from = r_eh2) %>% 
    arrange(basin_name) %>% 
    column_to_rownames('basin_name') %>% 
    as.matrix() 
  # replace NA  (from the populate and burnin periods) with first-year value
  eh2[, as.character(min(years$year):(firstYear - 1))] <-  eh2[, as.character(firstYear)]
  # store r_eh2 in results
  results[["other"]] [['eh2']] <- eh2
  
  # ----------------------------------------------------------------------- #
  ## matrix of survival proportion between catchments among emigrants  ----
  # row: arrival basin
  # colum: departure basin
  survivingProportion_PML <- results[["other"]] [['survivingProportion']]  <- outlet_distance %>% 
    collect() %>% 
    # filter basins
    filter(departure %in% basins,
           arrival %in% basins) %>% 
    # Calculate the relative fraction of fish that would return to each according to the kernel function
    mutate(proportion =  exp(-(parameter$alpha * distance ^ parameter$beta))) %>%  
    # no fish 'accidentally stray' into their natal catchment when NatalStray is FALSE
    mutate(withNatalStray = parameter$withNatalStray, 
           proportion = ifelse(withNatalStray, proportion, ifelse(departure == arrival, 0, proportion))) %>% 
    # compute the relative proportion
    group_by(departure) %>% 
    mutate(proportion = proportion / sum(proportion)) %>% 
    # calculate the survival rate of strayer between departure and arrival
    mutate(survival = exp(-parameter$Mdisp * distance),
           survivingProportion = proportion * survival) %>%
    # put 0 for very low survining probality
    mutate(survivingProportion = ifelse(survivingProportion < 1e-10, 0, survivingProportion)) %>% 
    # pivot wider
    pivot_wider(id_cols = arrival, 
                names_from = departure, 
                values_from = survivingProportion) %>%
    # arrange rows and columns
    arrange(arrival) %>% 
    column_to_rownames('arrival') %>% 
    select(all_of( basins)) %>% 
    as.matrix() %>% 
    # transform into a sparse matrix to speed up the calculation
    as("sparseMatrix")
  
  #Rq: transpose of Besty's matrix (not sure now)
  
  if (verbose) toc()
  
  # for testing: resultsModel <- results[['model']][[1]]
  # compute effective for 1 model ----
  computeEffectiveForModel_PML = function(model, currentYear, results, generationtime, nbCohorts){
    #cat(model, "\t", currentYear, "\n")
    currentYear_str = as.character(currentYear)
    
    # extract the results model
    resultsModel <- results[['model']][[model]]
    
    # cohorts contributing to this reproduction
    activeCohorts <- years %>% 
      filter(between(year, currentYear - generationtime, currentYear - generationtime + nbCohorts - 1)) %>% 
      mutate(year = as.character(year)) %>%       
      pull(year)
    
    # spawner from each departure catchment (weighted mean in a matrix mode)
    spawnersFrom <-  resultsModel$Nit[,activeCohorts] %*%  cohortWeight
    
    emigrants <- spawnersFrom * parameter$gamma
    resultsModel$emigrants[, currentYear_str] <- emigrants
    
    homers <- spawnersFrom - emigrants
    
    # immigrants in each arrival catchment (weighted mean in a matrix mode)
    immigrants <- results[['other']][['survivingProportion']] %*% emigrants %>% 
      as.matrix()
    #immigrants <- mat.mult(results[['other']][['survivingProportion']], emigrants)

    resultsModel$immigrants[, currentYear_str] <- immigrants
    
    # sum of homers and strayers surviving after anthropogenic mortality
    spawnersTo <- results[['other']] [['eh2']][, currentYear_str] * (homers + immigrants) 
    resultsModel$spawnersTo[, currentYear_str] <- spawnersTo
    
    # survival offspring
    if (parameter$withAllee) {
      # calculate the proportion of active spawners
      survivalOffsprings <- parameter$r * (spawnersTo^2 / (spawnersTo_50^2 + spawnersTo^2)) * spawnersTo
    } else
      survivalOffsprings <- parameter$r * spawnersTo 
    
    # max abundance
    maxN <-  resultsModel$Nmax_eh1[, currentYear_str]
    
    # update result with min of survival offsprings and max abundance (min by row)
    #resultsModel$Nit[, currentYear_str] <- apply(cbind(survivalOffsprings, maxN), 1, min)
    resultsModel$Nit[, currentYear_str] <- do.call(pmin, list(survivalOffsprings, maxN))
    return(resultsModel)
  }

  # compute effective for all models ----
  computeEffective_PML = function(currentYear, results, generationtime, nbCohorts) {
    
    # loop over models
    provResults <-  lapply(names(results[['model']]),
                           computeEffectiveForModel_PML, currentYear, results,  generationtime, nbCohorts)
    names(provResults) <- models
    
    # store  the provisional results in results
    results[['model']] <-  provResults
    
    return(results)
  }
  
  # years to run simulation 
  yearsToRun <- years %>% filter(phase != 'initial') %>% 
    #filter(as.integer(year) <= 1954) %>% suppressWarnings() %>% 
    pull(year)
  
  # Initialize a progress bar for keeping track of progress
  if (verbose) progbar <- txtProgressBar(min = min(yearsToRun),
                                         max = max(yearsToRun),
                                         style = 3)
  
  
  # run simulation over years 
  if (verbose) tic()
  for (currentYear  in yearsToRun) {
    # currentYear <- yearsToRun[1]
    ## print a progress bar to the console
    if (verbose) setTxtProgressBar(progbar, currentYear)
    results <- computeEffective_PML(currentYear, results, generationtime, nbCohorts)
  } 
  
  # dput(results, file = "tests/testthat/results_pml_dput")
  cat('\n')
  if (verbose) toc()
  
  return(results)
}

# =======================================================================================================
# run simulation ----
tic()
results <- runSimulation_pml(selected_latin_name, hydiad_parameter, anthropogenic_mortality,
                         catchment_surface, data_hsi_nmax, data_ni0, outlet_distance, verbose = FALSE)
toc()

dput(results, file = "tests/testthat/results_pml_dput")
utils::zip("tests/testthat/results_pml_dput", zipfile = "tests/testthat/results_pml_dput.zip")
file.remove("tests/testthat/results_pml_dput")
# ================================================================== #
# graphics ----
Nit_list <- results[['model']] %>% 
  lapply(function(x) x$Nit)  

# HyDiaDResults_AAlosa <- read_rds("/home/patrick/Documents/AA/CC_et_migrateur/hybrid_model/HyDiaD_R/data_output/HyDiaDResults_AAlosa_rcp85.RDS")
# 
# Nit_ref <- HyDiaDResults_AAlosa[[1]][[1]][1:3] %>%
#   lapply(function(x) x$Nit)



nit_feature_pml = function(data_list){
  return( data_list %>% reduce(pmin) %>%  
            as_tibble(rownames = 'basin_name') %>% 
            pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'min') %>% 
            mutate(year = as.integer(year)) %>% 
            inner_join(
              data_list %>% reduce(pmax) %>%  
                as_tibble(rownames = 'basin_name') %>% 
                pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'max') %>% 
                mutate(year = as.integer(year)),
              by = c('basin_name', 'year')) %>% 
            inner_join(
              data_list %>% simplify2array() %>% apply(c(1,2), mean) %>%  
                as_tibble(rownames = 'basin_name') %>% 
                pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'mean') %>% 
                mutate(year = as.integer(year)),
              by = c('basin_name', 'year')) %>% 
            group_by(basin_name) %>% 
            mutate(rolling_mean = frollmean(mean, n = 10, align = 'center')))
}

# basin = 'Mondego'
basin = 'Adour'

# nit_feature_pml(Nit_ref) %>% 
#   filter(basin_name == basin) %>% 
#   print(n = Inf) %>% 
#   ggplot(aes(x=year)) +
#   geom_ribbon(aes(ymin = min, ymax = max), alpha = .5)  +
#   geom_line(aes(y=rolling_mean), col ='red')
# 
# reference_results %>% 
#   filter(latin_name == selected_latin_name) %>% 
#   group_by(basin_name, year) %>% 
#   summarise(min = min(nit),
#             max = max(nit),
#             mean = mean(nit), .groups = 'drop') %>% 
#   group_by(basin_name) %>% 
#   mutate(rolling_mean = frollmean(mean, n = 10, align = 'center')) %>% 
#   mutate(source = 'reference') %>% 
#   filter(basin_name == basin) %>% 
#   print(n = Inf) %>% 
#   ggplot(aes(x=year)) +
#   geom_ribbon(aes(ymin = min, ymax = max), alpha = .5)  +
#   geom_line(aes(y=rolling_mean), col ='red')
# 
# 
# nit_feature_pml(Nit_list) %>% 
#   filter(basin_name == basin) %>% 
#   print(n = Inf) %>% 
#   ggplot(aes(x=year)) +
#   geom_ribbon(aes(ymin = min, ymax = max), alpha = .5)  +
#   geom_line(aes(y=rolling_mean), col ='red')

model_nit_outputs <- nit_feature_pml(Nit_list)
dput(model_nit_outputs, file = "tests/testthat/model_nit_outputs_dput")

model_res_filtered_pml <- model_nit_outputs %>% 
  mutate(source = 'simul') %>% 
  bind_rows(reference_results %>% 
              collect() %>% 
              filter(latin_name == selected_latin_name) %>% 
              group_by(basin_name, year) %>% 
              summarise(min = min(nit),
                        max = max(nit),
                        mean = mean(nit), .groups = 'drop') %>% 
              group_by(basin_name) %>% 
              mutate(rolling_mean = frollmean(mean, n = 10, align = 'center')) %>% 
              mutate(source = 'reference')) %>% 
  suppressWarnings() %>% 
  filter(basin_name == basin,
         year >= 1951) 

dput(model_res_filtered_pml, file = "tests/testthat/model_res_filtered_dput")

model_res_filtered_pml %>% 
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = min, ymax = max, fill = source), alpha = .5) + 
  geom_line(aes(y = rolling_mean, colour = source, linetype = source),
            alpha = 0.9) + 
  ylab('Nit') 

#=================================================================================
# verification ----
HyDiaDResults_AAlosa <- read_rds("/home/patrick/Documents/AA/CC_et_migrateur/hybrid_model/HyDiaD_R/data_output/HyDiaDResults_AAlosa_rcp85.RDS")

## verif param: ok ----
results[['param']][['hydiad_parameter']] 
HyDiaDResults_AAlosa[[1]][[1]][['ParmSet']][1:15] %>% as_tibble()

## verif HSI: ok ----
results[['model']][['cnrmcm5']]$HSI['Aa',1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["HSI"]]['Aa',1:20]

## verif Nit: ok ----
results[['model']][['cnrmcm5']]$Nit[1:5,1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["Nit"]][1:5,1:20]

## verif Nmax: ok ----
results[['model']][['cnrmcm5']]$Nmax[1:5,1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["Min1"]][1:5,1:20]

## verif r_eh2: ok ----
results[["other"]] [['r_eh2']]['Aa',1:20]

## verif Nit: ok ----
results[['model']][['cnrmcm5']]$Nit[1:5,1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["Nit"]][1:5,1:20]

## verif emigrants: Ok ----
results[['model']][['cnrmcm5']]$emigrants[1:5, 1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["Njy"]][1:5,1:20]

## verif survival matrix: ok ----
results[["other"]] [['survivingProportion']][1:10, 1:10]
t(HyDiaDResults_AAlosa[[1]][[1]][["ParmSet"]][['DispMatrix']][1:10, 1:10])

## verif immigrants: OK ----
results[['model']][['cnrmcm5']]$immigrants[1:5, 1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["DNjy"]][1:5,1:20]

## verif spawnersTo: PB ----
results[['model']][['cnrmcm5']]$spawnersTo[1:5,1:20]
HyDiaDResults_AAlosa[[1]][[1]][["Ann_Enviro_cn"]][["Bit"]][1:5,1:20]

# from values from the database: discrenpency ----
refRes <- reference_results %>%  
  filter(climatic_model_code == 'cnrmcm5',
         latin_name == selected_latin_name) %>%
  collect() %>% 
  arrange(basin_name, year) %>% 
  pivot_wider(id_cols = basin_name, names_from = year, values_from = nit) %>% 
  column_to_rownames('basin_name') %>% 
  as.matrix()
refRes[1:5,1:5]

