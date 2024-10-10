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
    mutate(source = 'reference') %>% 
    collect()
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
      dbplyr::translate_sql(
        con = get_con(session), 
        !!bv_ids)
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
    by = c("basin_id", "basin_name")
  )
  if (all(is.na(bv_df[["nit_mean"]]))) {
    return(NULL)
  }
  factpal <- colorNumeric(
    palette = "YlOrRd",
    domain = bv_df[["nit_mean"]],
    reverse = FALSE
  )
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = bv_df,
      layerId = ~basin_id,
      fillColor = ~ factpal(nit_mean),
      color = "#525252",
      weight = 1,
      label = ~basin_name,
      opacity = 0.8,
      fillOpacity = 0.6
    ) %>% 
    addLegend(data = bv_df,
              pal = factpal, values = ~nit_mean,
              title = "NIT",
              opacity = 0.6)
}

#' Plot hsi and nit on the same graph
#'
#' @param model_res model outputs
#' @param selected_year Numeric. selected_year
#' @param selected_bv Character. Selected BV
#' @param lg lang
#' @param withNitStandardisation Logical. Whether to standardise NIT.
#' @param with_colour_source Name of a variable to change colour lines
#'
#' @rdname plot_hsi_nit
#' 
#' @import patchwork
#' @importFrom dplyr filter
#' @import ggplot2
#' @export
plot_hsi_nit <- function(model_res,
                         selected_year,
                         selected_bv,
                         lg,
                         withNitStandardisation = FALSE,
                         with_colour_source = NULL) {
  
  model_res_filtered <- model_res %>%
    filter(basin_id == selected_bv) #%>% 
  # mutate(label = factor(paste(latin_name, basin_name, sep = ' in ')))
  
  hsi <- plot_hsi(model_res_filtered = model_res_filtered,
                  selected_year = selected_year,
                  lg = lg)
  
  nit <- plot_nit(model_res_filtered = model_res_filtered,
                  selected_year = selected_year,
                  lg = lg,
                  withNitStandardisation = FALSE,
                  with_colour_source = "source")
  
  getFromNamespace("/.ggplot", "patchwork")(
    hsi, nit
  )
}

#' Plot NIT only
#' @rdname plot_hsi_nit
#' @param model_res_filtered model_res for a specific basin
#' @param with_colour_source Name of a variable to change colour lines
#' @importFrom dplyr filter mutate inner_join between group_by summarise across
#' @import ggplot2
#' @export
plot_nit <- function(model_res_filtered,
                     selected_year,
                     lg,
                     withNitStandardisation = FALSE,
                     with_colour_source = NULL) {
  
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
  
  nit <- ggplot(model_res_filtered) +
    aes(x = year) 
  
  if (is.null(with_colour_source)) {
    nit <- nit +
      geom_ribbon(
        aes(ymin = nit_min, 
            ymax = nit_max,
            # fill = label
        ), alpha = 0.3) +
      geom_line(
        aes(y = nit_movingavg,
            # color = label
        )
      )
  } else {
    nit <- nit +
      geom_ribbon(
        aes(ymin = nit_min, 
            ymax = nit_max,
            fill = .data[[with_colour_source]]
        ), alpha = 0.3) +
      geom_line(
        aes(y = nit_movingavg,
            colour = .data[[with_colour_source]],
            linetype = .data[[with_colour_source]]
        )
      ) +
      scale_colour_brewer(type = "qual", palette = "Set2") +
      scale_fill_brewer(type = "qual", palette = "Set2")
  }
  nit <- nit + 
    geom_vline(xintercept = 2001, colour = "gray", linetype = "dashed") +
    geom_vline(xintercept = 2051, colour = "gray", linetype = "dashed") +
    geom_vline(xintercept = selected_year, color = "red") +
    theme_bw() +
    theme(legend.title = element_blank()) +
    # ylab('Abundance (nb of fish)')
    ylab(get_translation_entry('nit_ggplot', lg))
  
  return(nit)
}

#' Plot NIT only
#' @param model_res_filtered model_res for a specific basin
#' @rdname plot_hsi_nit
#' @import ggplot2
#' @export
plot_hsi <- function(model_res_filtered,
                     selected_year,
                     lg) {
  model_res_filtered  %>%
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
}

#' @noRd
create_ui_summary_html <- function(
    species, 
    date, 
    basin_name, 
    country
) {
  HTML(
    paste0("<span data-i18n='", low_and_sub(species), "'>", species, "</span>"),
    "/",
    date,
    "/",
    basin_name,
    "-",
    country
  )
}
