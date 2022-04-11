#' Title
#'
#' Description
#' 
#' @param species_id,scenario internal param for SQL filter
#' @param session Shiny session  object
#' @param lg Character. Lang to be used.
#' @param widthWindow Numeric. Moving window for moving average
#' @importFrom dplyr tbl filter collect mutate
#'
#' @export
#'
get_hybrid_model <- function(species_id,
                             scenario,
                             lg,
                             session = shiny::getDefaultReactiveDomain(),
                             widthWindow = 10) {
  if (!"v_hybrid_model_mavg" %in% DBI::dbListTables(get_con(session))) {
    # Create table v_hybrid_model_mavg 
    # load model outputs
    query = paste0('SELECT species_id, s.latin_name, basin_id, basin_name, "year", climatic_model_id, climatic_scenario, climatic_model_code, nit, hsi FROM diadesatlas.hybrid_model_result hmr
  INNER JOIN diadesatlas.species s USING (species_id)
  INNER JOIN diadesatlas.basin b USING (basin_id)
  INNER JOIN diadesatlas.climatic_model cm USING (climatic_model_id)')
    # WHERE hmr.year > 0 AND s.latin_name = \'', species, '\' AND basin_name  = \'', basin_name, '\' AND climatic_scenario =  \'', scenario, '\'')
    
    # data <- dbGetQuery(conn_eurodiad, query) %>%
    # tibble() %>%
    v_hybrid_model_mavg <- tbl(conn_eurodiad, sql(query)) %>%
      # filter and translate before ---
      dplyr::filter(
        # On choisis un seul climatic_model_id (à terme, ce sera le 999)
        # Voir https://diades.gitlab.irstea.page/diades.atlas.minute/point-davancement.html#page-3
        # climatic_model_id == 2,
        species_id %in% !!species_id,
        climatic_scenario %in% !!scenario, # unique scenario
        # year %in% !!seq(from = date[1], to = date[2], by = 1)
        year > 0 # 0
      ) %>% 
      # translation
      mutate(
        basin_name = diadesatlas.translate(basin_name, !!lg)#,
        # species_name = diadesatlas.translate(english_name, !!lg)
      ) %>% 
      # Create min, mean, max, rolling_mean ----
    pivot_longer(cols = c(nit, hsi), names_to = 'output') %>% 
      group_by(species_id, latin_name, basin_id, basin_name, year, output) %>%
      summarise(min = min(value),
                mean = mean(value),
                max = max(value),
                .groups = 'drop') %>%
      collect() %>% 
      group_by(species_id, latin_name, basin_id, basin_name, output) %>%
      mutate(rolling_mean = data.table::frollmean(mean, n = widthWindow, align = 'center')) %>%
      ungroup()
  } else {
    # v_hybrid_model_mavg is in the database
    v_hybrid_model_mavg <- tbl(
      get_con(session),
      "v_hybrid_model_mavg"
      # "v_hybrid_model"
    ) %>%
      filter(
        # On choisis un seul climatic_model_id (à terme, ce sera le 999)
        # Voir https://diades.gitlab.irstea.page/diades.atlas.minute/point-davancement.html#page-3
        # climatic_model_id == 2,
        species_id %in% !!species_id,
        climatic_scenario %in% !!scenario ,
        # year %in% !!seq(from = date[1], to = date[2], by = 1)
        year != 0 # 0
      ) %>%
      # translation
      mutate(
        basin_name = diadesatlas.translate(basin_name, !!lg)#,
        # species_name = diadesatlas.translate(english_name, !!lg)
      ) %>% 
      collect()
  }
}

#' @importFrom dplyr tbl filter mutate select collect
#' @noRd
get_bv_geoms <- function(bv_ids,
                         lg,
                         session = shiny::getDefaultReactiveDomain()) {
  # TODO Vérifier la projection
  res <- sf::st_read(
    get_con(session),
    query = sprintf(
      "select basin_id, verysimplified_geom from basin_outlet where basin_id IN %s",
      dbplyr::translate_sql(!!bv_ids)
    )
  )
  basin <- tbl(get_con(session), "basin") %>%
    filter(basin_id %in% !!res$basin_id) %>%
    mutate(basin_name = diadesatlas.translate(basin_name, !!lg)) %>%
    select(basin_id, basin_name) %>%
    collect()
  
  res %>%
    dplyr::left_join(basin, by = "basin_id")
}

#' @import leaflet
#' @importFrom utils getFromNamespace
draw_bv_leaflet <- function(bv_df,
                            model_res,
                            year) {
  bv_df <- dplyr::left_join(
    bv_df,
    filter(model_res, year == !!year),
    by = "basin_id"
  )
  if (all(is.na(bv_df$nit))) {
    return(NULL)
  }
  factpal <- colorNumeric(
    palette = "YlOrRd",
    domain = bv_df$nit,
    reverse = FALSE
  )
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = bv_df,
      layerId = ~basin_id,
      fillColor = ~ factpal(nit),
      color = "#525252",
      weight = 1,
      label = ~basin_name,
      opacity = 0.8,
      fillOpacity = 0.6
    ) %>% 
    addLegend(data = bv_df,
              pal = factpal, values = ~nit,
              title = "NIT",
              opacity = 0.6)
}

#' @import patchwork
#' @importFrom dplyr filter mutate inner_join between group_by summarise across
#' @import ggplot2
plot_hsi_nit <- function(model_res,
                         selected_year,
                         selected_bv,
                         lg,
                         withNitStandardisation = FALSE) {
  model_res_filtered <- model_res %>%
    filter(basin_id == selected_bv) #%>% 
  # mutate(label = factor(paste(latin_name, basin_name, sep = ' in ')))
  
  hsi <- model_res_filtered  %>%
    ggplot(aes(x = year)) +
    geom_ribbon(aes(ymin = hsi_min, 
                    ymax = hsi_max,
                    # fill =  label
    ),
    alpha = 0.3) +
    geom_line(aes(y = hsi_movingavg#,
                  # color = label
    )) +
    geom_vline(xintercept = selected_year, color = "red") +
    # ylab('Catchment suitability index') +
    ylab(get_translation_entry('hsi_ggplot', lg)) +
    theme(legend.title = element_blank()) +
    ylim(0,1)
  
  if (withNitStandardisation) {
    model_res_filtered <- model_res_filtered %>% 
      inner_join(model_res_filtered %>% 
                   filter(between(year, 1950,1980)) %>% 
                   group_by(species_id, basin_id) %>% 
                   summarise(nit_mean_mean = mean(nit_mean),
                             .groups = 'drop'),
                 by = c("species_id", "basin_id")) %>% 
      mutate(across(c("nit_min", "nit_mean", "nit_max", "nit_movingavg"), ~.x/nit_mean_mean))
  }
  
  nit <- model_res_filtered %>%
    ggplot(aes(x = year)) +
    geom_ribbon(aes(ymin = nit_min, 
                    ymax = nit_max,
                    # fill = label
    ),
    alpha = 0.3) +
    geom_line(aes(y = nit_movingavg,
                  # color = label
    )) +
    geom_vline(xintercept = selected_year, color = "red") +
    theme(legend.title = element_blank()) +
    # ylab('Abundance (nb of fish)')
    ylab(get_translation_entry('nsi_ggplot', lg))
  
  getFromNamespace("/.ggplot", "patchwork")(
    hsi, nit
  )
}
