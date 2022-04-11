#' Title
#'
#' Description
#' 
#' @param species_id,scenario internal param for SQL filter
#' @param session Shiny session  object
#'
#' @export
#'
get_hybrid_model <- function(species_id,
                             scenario,
                             session = shiny::getDefaultReactiveDomain()) {
    tbl(
        get_con(session),
        "hybrid_model_result"
    ) %>%
        filter(
            # On choisis un seul climatic_model_id (à terme, ce sera le 999)
            # Voir https://diades.gitlab.irstea.page/diades.atlas.minute/point-davancement.html#page-3
            climatic_model_id == 2,
            species_id %in% !!species_id,
            climatic_scenario %in% !!scenario ,
            # year %in% !!seq(from = date[1], to = date[2], by = 1)
            year != 0 # 0
        ) %>%
        collect()
}

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
plot_hsi_nit <- function(model_res,
                         selected_year,
                         selected_bv) {
    model_res_filtered <- model_res %>%
        filter(
            basin_id == selected_bv
        )
    hsi <- ggplot(
        model_res_filtered,
        aes(year, hsi)
    ) +
        geom_line() +
        geom_vline(xintercept = selected_year, color = "red") +
        theme_classic() +
        theme(
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20)
        )
    nit <- ggplot(
        model_res_filtered,
        aes(year, nit)
    ) +
        geom_line() +
        geom_vline(xintercept = selected_year, color = "red") +
        theme_classic() +
        theme(
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20)
        )
    getFromNamespace("/.ggplot", "patchwork")(
        hsi, nit
    )
}
