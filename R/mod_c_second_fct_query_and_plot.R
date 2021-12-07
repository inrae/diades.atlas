#' Title
#'
#' Description
#' 
#' @param species_id,scenario internal param for SQL filter
#' @param session Shiny session  object
#'
#' @export
#'
#' @example
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
            climatic_scenario %in% !!scenario # ,
            # year %in% !!seq(from = date[1], to = date[2], by = 1)
        ) %>%
        collect()
}

get_bv_geoms <- function(bv_ids,
                         session = shiny::getDefaultReactiveDomain()) {
    # TODO Vérifier la projection
    sf::st_read(
        get_con(session),
        query = sprintf(
            "select basin_id, verysimplified_geom from basin_outlet where basin_id IN %s",
            dbplyr::translate_sql(!!bv_ids)
        )
    )
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
    # TODO add the color
    # factpal <- colorNumeric(
    #     palette = "Blues",
    #     domain = bv_df$saturation_rate
    # )
    leaflet() %>%
        addTiles() %>%
        addPolygons(
            data = bv_df,
            layerId = ~basin_id # ,
            # color = ~ factpal(saturation_rate)
        )
}

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
        geom_vline(xintercept = selected_year, color = "red")
    nit <- ggplot(
        model_res_filtered,
        aes(year, nit)
    ) +
        geom_line() +
        geom_vline(xintercept = selected_year, color = "red")
    getFromNamespace("/.ggplot", "patchwork")(
        hsi, nit
    )
}