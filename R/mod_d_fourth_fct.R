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
#' @param selected_latin_name Species latin name
#' @param hydiad_parameter Hydiad model parameters
#' @param anthropogenic_mortality table of anthropogenic mortalities
#' @param catchment_surface Surface of basins
#' @param data_hsi_nmax HSI Nmax values
#' @param data_ni0 ni0 values
#' @param outlet_distance distance from outlet
#' @param scenario Climatic scenario. e.g. "rcp85"
#' @param verbose Logical.
#' 
#' @importFrom tidyr pivot_wider expand_grid
#' @importFrom tibble column_to_rownames
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom methods as
#' @import Matrix
#' 
#' @return List of models outputs
#' @export
runSimulation <- function(selected_latin_name, 
                          hydiad_parameter, 
                          anthropogenic_mortality,
                          catchment_surface, 
                          data_hsi_nmax, 
                          data_ni0,  
                          outlet_distance, 
                          scenario = "rcp85",
                          verbose = FALSE) {
  # if (verbose) tic()
  if (verbose) {
    if (is.null(getDefaultReactiveDomain())) {
      print("Prepare simulation")
    } else {
      incProgress(0, detail = paste("Init simulation"))
    }
  }
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
    filter(latin_name == !!selected_latin_name ) %>% 
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
  # browser()
  
  # spawnersTo_50 <-  parameter$lambda * parameter$Dmax * catchment_surface %>% pull(surface_area)
  
  catchment_spawnersTo_50 <- catchment_surface %>%
    mutate(spawnersTo_50 = !!parameter$lambda * !!parameter$Dmax * surface_area)
  
  # Be sure to be in correct order
  spawnersTo_50 <- catchment_spawnersTo_50 %>% 
    collect() %>% 
    arrange(basin_name) %>% 
    pull(spawnersTo_50)
  
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
  
  # expect_equal(extendedNit, extendedNit_PML)
  
  ## list years in simulation ----
  years <- extendedNit %>%  
    distinct(year, phase) %>%  
    arrange(year)
  results[['param']][['years']] <- years 
  
  # if (verbose) toc()
  
  # if (verbose) tic()
  # ------------------------------------------------------------------------------- #

  # Create dput for tests
  # dput(head(extendedNit), file = "tests/testthat/extendedNit_dput")
  # 
  results[["model"]] <- lapply(models, compute_nmax_eh1, extendedNit = extendedNit, scenario = scenario)
  names(results[["model"]]) <-  models
  
  # expect_equal(results[["model"]], resultsPM) # OK
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
  
  # expect_equal(eh2, eh2_PML) # OK
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
  
  # expect_equal(results[["other"]][['survivingProportion']], survivingProportion_PML) # OK
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
    
    nyears <- max(yearsToRun) - min(yearsToRun) + 1
    if (is.null(getDefaultReactiveDomain())) {
      progbar <- txtProgressBar(min = min(yearsToRun),
                                max = max(yearsToRun),
                                style = 3)
    } else {
      incProgress(1/nyears, detail = paste("Doing year init"))
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
        # prog <- (currentYear - min(yearsToRun))/(max(yearsToRun) - min(yearsToRun) + 1)
        incProgress(1/nyears, detail = paste("Doing year", currentYear))
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
                                # catchment_spawnersTo_50 = catchment_spawnersTo_50,
                                spawnersTo_50 = spawnersTo_50)
  } 
  cat('\n')
  # if (verbose) toc()
  
  return(results)
}

#' Compute Nmax_eh1 matrix and prepare Nit matrix
#'
#' @param model model, e.g. "cnrmcm5"
#' @param scenario Global warming scenario e.g "rcp85"
#' @param extendedNit extendedNit data input
#' 
#' @noRd
compute_nmax_eh1 <- function(model, scenario, extendedNit) {
  out <- list()
  
  out[['HSI']] <- 
    extendedNit %>% 
    filter(climatic_model_code == model,
           climatic_scenario == scenario) %>% 
    pivot_wider(id_cols = basin_name,
                names_from = year,
                values_from = hsi) %>% 
    arrange(basin_name) %>% 
    column_to_rownames('basin_name') %>% 
    as.matrix()
  
  out[['Nmax_eh1']] <- 
    extendedNit %>% 
    filter(climatic_model_code == model,
           climatic_scenario == scenario) %>% 
    pivot_wider(id_cols = basin_name,
                names_from = year,
                values_from = Nmax_eh1) %>% 
    arrange(basin_name) %>% 
    column_to_rownames('basin_name') %>% 
    as.matrix()
  
  out[['Nit']] <- 
    extendedNit %>% 
    filter(climatic_model_code == model,
           climatic_scenario == scenario) %>% 
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
#' @param spawnersTo_50
#' 
#' @importFrom tibble rownames_to_column
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
                                     # catchment_spawnersTo_50,
                                     spawnersTo_50) {
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
    # # Safe Join Spawners to be sure that basins are in the same order
    # catchment_spawnersTo_50_collect <- catchment_spawnersTo_50 %>% collect()
    # # Keep in order of `spawnersTo`
    # survivalOffsprings <- as.data.frame(spawnersTo) %>% 
    #   # get back rownames as column for the join
    #   rownames_to_column(var = "basin_name") %>% # count() # 134
    #   rename(spawnersTo = V1) %>% 
    #   # Join
    #   left_join(catchment_spawnersTo_50_collect, by = "basin_name") %>% # %>% count()
    #   # Calculate survivalOffsprings with correct basin
    #   mutate(
    #     survivalOffsprings = parameter$r * 
    #       (spawnersTo^2 / (spawnersTo_50^2 + spawnersTo^2)) * spawnersTo) %>% 
    #   # Back to matrix for the calculations
    #   column_to_rownames(var = "basin_name") %>% 
    #   select(survivalOffsprings) %>% 
    #   as.matrix() #%>% 
    # head() %>% is()
    
    # calculate the proportion of active spawners
    survivalOffsprings <- parameter$r * (spawnersTo^2 / (spawnersTo_50^2 + spawnersTo^2)) * spawnersTo
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
#' @param spawnersTo_50
#'
#' @noRd
computeEffective <- function(currentYear,
                             results,
                             generationtime,
                             nbCohorts,
                             years,
                             parameter,
                             cohortWeight,
                             spawnersTo_50,
                             # catchment_spawnersTo_50,
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
    spawnersTo_50 = spawnersTo_50
    # catchment_spawnersTo_50 = catchment_spawnersTo_50
    )
  names(provResults) <- models
  
  # store  the provisional results in results
  results[['model']] <-  provResults
  
  return(results)
}

#' Expand anthropogenic mortality
#'
#' @param data_hsi_nmax data_hsi_nmax
#' @param mortalities Table of mortalities
#' 
#' @importFrom dplyr distinct mutate full_join select collect left_join
#' @importFrom dplyr case_when between
#' 
#' @return Tibble with anthropogenic mortality for each year
#' @noRd
#'
expand_anthropogenic_mortality <- function(data_hsi_nmax, mortalities) {
  full_join(data_hsi_nmax %>% 
              distinct(year) %>% 
              mutate(by = 1),
            data_hsi_nmax %>% 
              distinct(country) %>% 
              mutate(by = 1), by = "by") %>% 
    select(-by) %>% 
    mutate(h2 = 0) %>% 
    collect() %>% 
    # Join with table of mortalities
    left_join(mortalities, by = "country") %>% 
    mutate(
      h1 = case_when(
        # between(year, 2001, 2050) & country == 'France' ~ -log(.5),
        between(year, 2001, 2050) ~ yearsimubegin, #-log(.5),
        # between(year, 2051, 2100) & country == 'France' ~ -log(.75),
        between(year, 2051, 2100) ~ yearsimuend, #-log(.75),
        TRUE ~ 0 # before 2001
      ))
}

#' Get Nit results from model results
#' @param results List of results
#' @noRd
get_model_nit <- function(results) {
  Nit_list <- results[['model']] %>% 
    lapply(function(x) x[["Nit"]])  
  return(Nit_list)
}

#' Calculate NIT feature
#'
#' @param data_list List of Nit outputs only
#' 
#' @importFrom dplyr as_tibble mutate inner_join group_by
#' @importFrom tidyr pivot_longer
#' @importFrom purrr reduce
#' @importFrom data.table frollmean
#' 
#' @return A tibble
#' @export
#'
nit_feature <- function(data_list) {
  res <- data_list %>% 
    reduce(pmin) %>%  
    as_tibble(rownames = 'basin_name') %>% 
    pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'min') %>% 
    mutate(year = as.integer(year)) %>% 
    inner_join(
      data_list %>% 
        reduce(pmax) %>%  
        as_tibble(rownames = 'basin_name') %>% 
        pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'max') %>% 
        mutate(year = as.integer(year)),
      by = c('basin_name', 'year')) %>% 
    inner_join(
      data_list %>% 
        simplify2array() %>% 
        apply(c(1,2), mean) %>%  
        as_tibble(rownames = 'basin_name') %>% 
        pivot_longer(cols = -basin_name, names_to = 'year', values_to = 'mean') %>% 
        mutate(year = as.integer(year)),
      by = c('basin_name', 'year')) %>% 
    group_by(basin_name) %>% 
    mutate(rolling_mean = frollmean(mean, n = 10, align = 'center')) %>% 
    ungroup()
  return(res)
}

#' Get nit reference and predictions for one species and all basins
#' 
#' @param Nit_list Nit_list as issued from [get_model_nit()]
#' @param reference_results reference_results as issued from [prepare_datasets()]
#' @param selected_latin_name Latin species name
#' 
#' @importFrom dplyr mutate bind_rows filter group_by summarise
#' @importFrom dplyr ungroup
#' @importFrom data.table frollmean
#' @return data.frame of Nit results for one species and one basin
#' @export
nit_feature_species <- function(Nit_list,
                                      reference_results,
                                      selected_latin_name) {
  nit_feature(Nit_list) %>% 
    mutate(source = 'simul') %>% 
    bind_rows(
      # reference for this species
      reference_results %>% 
        filter(latin_name == !!selected_latin_name) %>% 
        collect() %>% 
        group_by(basin_name, year) %>% 
        summarise(min = min(nit),
                  max = max(nit),
                  mean = mean(nit),
                  .groups = 'drop') %>% 
        # collect() %>% 
        group_by(basin_name) %>% 
        mutate(rolling_mean = frollmean(mean, n = 10, align = 'center')) %>% 
        mutate(source = 'reference') %>% 
        ungroup()
      ) %>%
    suppressWarnings() %>% 
    filter(year >= 1951) %>% 
    rename(
      nit_min = min,
      nit_max = max,
      nit_mean = mean,
      nit_movingavg = rolling_mean
    )
}
