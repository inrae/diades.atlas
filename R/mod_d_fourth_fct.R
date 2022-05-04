#' Transform a sentence into a proper slug
#' @param x Character
#' @noRd
slugify <- function(x) {
  gsub("\\s+|_", "-", tolower(x))
}

#' Create a list of sliders UI inputs
#' 
#' @param ns session$ns function
#' @param countries Character vector
#' @param prefix prefix to add to the InputId of the sliderInput
#' @noRd
multi_sliders <- function(ns, countries, prefix = "period1") {
  countries_slug <- slugify(countries)
  tagList(
    purrr::map2(
      countries, countries_slug, ~{sliderInput(
        ns(paste(prefix, .y, sep = "-")),
        with_i18(.x, .y),
        min = 0.1,
        max = 2,
        value = 0.2
      )}
    )
  )
}

#' Run simulation
#'
#' @param selected_latin_name 
#' @param hydiad_parameter 
#' @param anthropogenic_mortality 
#' @param catchment_surface 
#' @param data_hsi_nmax 
#' @param data_ni0 
#' @param outlet_distance 
#' @param verbose 
#' 
#' @importFrom tidyr pivot_wider expand_grid
#' @importFrom tibble column_to_rownames
#' @import Matrix
runSimulation <- function(selected_latin_name, 
                          hydiad_parameter, 
                          anthropogenic_mortality,
                          catchment_surface, 
                          data_hsi_nmax, 
                          data_ni0,  
                          outlet_distance, 
                          verbose = FALSE) {
  # if (verbose) tic()
  
  # --------------------------------------------------------------------------------------- #
  results = list()
  
  # ---------------------------------------------------------------------- #
  # Local variables ----
  ## ordered list of basin s----
  basins <- data_hsi_nmax %>% 
    distinct(basin_name) %>%
    pull() %>% 
    sort()
  # arrange(basin_name) %>% 
  # pull(basin_name)
  
  ## list of models ----
  models <- data_hsi_nmax %>% 
    distinct(climatic_model_code) %>%
    # arrange(climatic_model_code) %>% 
    pull(climatic_model_code) %>% 
    sort()
  
  ## HyDiaD parameters for the selected species ----
  parameter <- hydiad_parameter %>% 
    filter(latin_name_s == !!selected_latin_name ) %>% 
    collect()
  results[['param']][['hydiad_parameter']] <- parameter
  
  ##  cohorts in  spawner run (number and weights) ----
  nbCohorts <-  parameter$nbCohorts
  cohortWeight <- matrix(rep(1/nbCohorts, nbCohorts), ncol = 1)
  
  ## generation time : number or years contributing to a spawner run ----
  generationtime <- floor(parameter$AgeFirstMat + (nbCohorts / 2))
  
  ## nb of years in the 'burn-in' period (let the model stabilizing after model population) ----
  burnin <- 10
  
  ## first year of simulation ----
  firstYear <- data_hsi_nmax %>% 
    summarise(min = min(year, na.rm = TRUE)) %>% 
    pull()
  # firstYear <- min(data_hsi_nmax$year, na.rm = TRUE)
  
  # ---------------------------------------------------------------------- #
  # Local matrices ----
  ## the spawnerTo that are half active in reproduction (Allee effect) ----
  # spawnersTo_50 <-  parameter$lambda * parameter$Dmax * catchment_surface$surface_area
  # spawnersTo_50 <-  parameter$lambda * parameter$Dmax * catchment_surface %>% pull(surface_area)
  
  catchment_spawnersTo_50 <- catchment_surface %>% 
    mutate(spawnersTo_50 = !!parameter$lambda * !!parameter$Dmax * surface_area)
  
  # spawnersTo_50 <- catchment_spawnersTo_50 %>% collect() %>% arrange(basin_name) %>% pull(spawnersTo_50)
  
  ## update Nmax according to anthropogenic mortality eh1 = exp(-h1) ----
  Nit <- data_hsi_nmax %>% 
    filter(latin_name == !!selected_latin_name) %>% 
    mutate(phase = 'simul') %>% 
    collect() %>% 
    # update maximal abundance (Nmax) with anthropogenic mortality (h1)
    inner_join(
      anthropogenic_mortality %>% 
        select(country, year, h1),
      by = c('country', 'year')) %>% 
    mutate(Nmax_eh1 = Nmax * exp(-h1), 
           Nit = 0)
  
  # anticipate simulation to initialise model and to burn-in run 
  anticipation <-  
    tidyr::expand_grid( 
      data_ni0 %>% 
        filter(latin_name == !!selected_latin_name) %>%  
        select(-c(year)) %>% 
        collect(),
      tibble(
        year = seq(firstYear - burnin - generationtime, firstYear - 1), 
        phase = c(rep('initial', generationtime), rep('burnin', burnin))
      )
    ) %>% 
    # initial value (HSI predict with 10-year average of environmental factors)
    mutate(Nmax_eh1 = Nmax ,
           Nit =  ifelse(phase == 'initial', Nmax, 0))
  
  extendedNit <- Nit %>% 
    bind_rows(anticipation) %>% 
    arrange(basin_id, climatic_model_code, year)
  
  
  ## list years in simulation ----
  years <- extendedNit %>%  
    distinct(year, phase) %>%  
    arrange(year)
  results[['param']][['years']] <- years 
  
  # if (verbose) toc()
  
  # if (verbose) tic()
  # ------------------------------------------------------------------------------- #
  
  results[["model"]] <- lapply(models, compute_nmax_eh1, extendedNit = extendedNit)
  names(results[["model"]]) <-  models
  
  # --------------------------------------------------------------------------------------- #
  ## r * exp(-h2) matrix ----
  
  # compute exp(-h2) 
  eh2 <-  extendedNit %>% 
    distinct(year, basin_name, country) %>% 
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
  eh2[, as.character(min(years[["year"]]):(firstYear - 1))] <-  eh2[, as.character(firstYear)]
  # store r_eh2 in results
  results[["other"]][['eh2']] <- eh2
  
  # ----------------------------------------------------------------------- #
  ## matrix of survival proportion between catchments among emigrants  ----
  # row: arrival basin
  # colum: departure basin
  results[["other"]][['survivingProportion']]  <- outlet_distance %>% 
    # filter basins
    filter(departure %in% !!basins,
           arrival %in% !!basins) %>% 
    # Calculate the relative fraction of fish that would return to each according to the kernel function
    mutate(proportion =  exp(-(!!parameter$alpha * distance ^ !!parameter$beta))) %>%  
    # no fish 'accidentally stray' into their natal catchment when NatalStray is FALSE
    mutate(withNatalStray = !!parameter$withNatalStray, 
           proportion = ifelse(withNatalStray, proportion, 
                               ifelse(departure == arrival, 0, proportion))) %>% 
    # compute the relative proportion
    group_by(departure) %>% 
    mutate(proportion = proportion / sum(proportion, na.rm = TRUE)) %>% 
    # calculate the survival rate of strayer between departure and arrival
    mutate(survival = exp(-!!parameter$Mdisp * distance),
           survivingProportion = proportion * survival) %>%
    # put 0 for very low survining probality
    mutate(survivingProportion = ifelse(survivingProportion < 1e-10, 0,
                                        survivingProportion)) %>% 
    collect() %>% 
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
  
  # if (verbose) toc()
  
  # for testing: resultsModel <- results[['model']][[1]]
  # years to run simulation 
  yearsToRun <- years %>% filter(phase != 'initial') %>% 
    #filter(as.integer(year) <= 1954) %>% suppressWarnings() %>% 
    pull(year)
  
  # Initialize a progress bar for keeping track of progress
  if (verbose) {
    print(paste(min(yearsToRun), max(yearsToRun), sep = "-"))
    
    n <- max(yearsToRun) - min(yearsToRun) + 1
    if (is.null(getDefaultReactiveDomain())) {
      progbar <- txtProgressBar(min = min(yearsToRun),
                                max = max(yearsToRun),
                                style = 3)
    } else {
      incProgress(1/n, detail = paste("Doing part", 0))
    }
  }
  
  # run simulation over years 
  # if (verbose) tic()
  for (currentYear in yearsToRun) {
    # currentYear <- yearsToRun[1]
    ## print a progress bar to the console
    if (verbose) {
      if (is.null(getDefaultReactiveDomain())) {
        setTxtProgressBar(progbar, currentYear)
      } else {
        prog <- (currentYear - min(yearsToRun))/(max(yearsToRun) - min(yearsToRun) + 1)
        incProgress(prog, detail = paste("Doing part", currentYear))
      }
    }
    results <- computeEffective(currentYear, 
                                results = results, 
                                generationtime = generationtime, 
                                nbCohorts = nbCohorts,
                                years = years,
                                parameter = parameter,
                                cohortWeight = cohortWeight,
                                models = models,
                                catchment_spawnersTo_50 = catchment_spawnersTo_50)
  } 
  cat('\n')
  # if (verbose) toc()
  
  return(results)
}

#' Compute Nmax_eh1 matrix and prepare Nit matrix
#'
#' @param model model 
#' @param extendedNit extendedNit
#' 
#' @noRd
compute_nmax_eh1 <- function(model, extendedNit) {
  out <- list()
  
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
  
  out[['emigrants']] <- matrix(
    0, 
    nrow = nrow(out[['Nit']]),
    ncol = ncol(out[['Nit']]), 
    dimnames = list(rownames(out[['Nit']]), colnames(out[['Nit']])))
  
  out[['immigrants']] <- matrix(
    0, 
    nrow = nrow(out[['Nit']]),
    ncol = ncol(out[['Nit']]), 
    dimnames = list(rownames(out[['Nit']]), colnames(out[['Nit']])))
  
  out[['spawnersTo']] <- matrix(
    0, 
    nrow = nrow(out[['Nit']]),
    ncol = ncol(out[['Nit']]), 
    dimnames = list(rownames(out[['Nit']]), colnames(out[['Nit']])))
  
  
  return(out)
}

#' Compute effective for 1 model
#'
#' @param model 
#' @param currentYear 
#' @param results 
#' @param generationtime 
#' @param nbCohorts 
#'
#' @noRd
computeEffectiveForModel <- function(model,
                                     currentYear,
                                     results,
                                     generationtime,
                                     nbCohorts,
                                     years,
                                     parameter,
                                     cohortWeight,
                                     catchment_spawnersTo_50) {
  #cat(model, "\t", currentYear, "\n")
  currentYear_str <- as.character(currentYear)
  
  # extract the results model
  resultsModel <- results[['model']][[model]]
  
  # cohorts contributing to this reproduction
  activeCohorts <- years %>% 
    filter(between(year, currentYear - generationtime, currentYear - generationtime + nbCohorts - 1)) %>% 
    mutate(year = as.character(year)) %>%       
    pull(year)
  
  # spawner from each departure catchment (weighted mean in a matrix mode)
  spawnersFrom <- resultsModel$Nit[, activeCohorts] %*% cohortWeight
  
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
    # Safe Join Spawners to be sure that basins are in the same order
    catchment_spawnersTo_50_collect <- catchment_spawnersTo_50 %>% collect()
    # Keep in order of `spawnersTo`
    survivalOffsprings <- as.data.frame(spawnersTo) %>% 
      # get back rownames as column for the join
      rownames_to_column(var = "basin_name") %>% # count() # 134
      rename(spawnersTo = V1) %>% 
      # Join
      left_join(catchment_spawnersTo_50_collect, by = "basin_name") %>% # %>% count()
      # Calculate survivalOffsprings with correct basin
      mutate(
        survivalOffsprings = parameter$r * 
          (spawnersTo^2 / (spawnersTo_50^2 + spawnersTo^2)) * spawnersTo) %>% 
      # Back to matrix for the calculations
      column_to_rownames(var = "basin_name") %>% 
      select(survivalOffsprings) %>% 
      as.matrix() #%>% 
    # head() %>% is()
    
    # calculate the proportion of active spawners
    # survivalOffsprings <- parameter$r * (spawnersTo^2 / (spawnersTo_50^2 + spawnersTo^2)) * spawnersTo
  } else {
    survivalOffsprings <- parameter$r * spawnersTo 
  }
  # max abundance
  maxN <-  resultsModel$Nmax_eh1[, currentYear_str]
  
  # update result with min of survival offsprings and max abundance (min by row)
  #resultsModel$Nit[, currentYear_str] <- apply(cbind(survivalOffsprings, maxN), 1, min)
  resultsModel$Nit[, currentYear_str] <- do.call(pmin, list(survivalOffsprings, maxN))
  return(resultsModel)
}

#' Compute effective for all models
#'
#' @param currentYear 
#' @param results 
#' @param generationtime 
#' @param nbCohorts 
#'
#' @noRd
computeEffective <- function(currentYear,
                             results,
                             generationtime,
                             nbCohorts,
                             years,
                             parameter,
                             cohortWeight,
                             catchment_spawnersTo_50,
                             models) {
  
  # loop over models
  provResults <-  lapply(
    names(results[['model']]),
    computeEffectiveForModel, 
    currentYear = currentYear, 
    results = results,  
    generationtime = generationtime, 
    nbCohorts = nbCohorts, 
    years = years,
    parameter = parameter,
    cohortWeight = cohortWeight,
    catchment_spawnersTo_50 = catchment_spawnersTo_50)
  names(provResults) <- models
  
  # store  the provisional results in results
  results[['model']] <-  provResults
  
  return(results)
}
