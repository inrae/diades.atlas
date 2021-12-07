#' Title
#'
#' @param species_latin_name The latin name of the species
#' @param spatial_type Geom to use in the map
#' @param con The Connection object
#' @param yearStart,yearEnd date used
#' @param dataCatchment,catchment_geom,dataALL,ices_geom  internal datasets
#'
#' @export
#'
ecosystem_table <- function(species,
                            case_study,
                            ecosystem,
                            r,
                            session = shiny::getDefaultReactiveDomain()) {
    res <- dplyr::tbl(
        get_con(session),
        "v_ecosystemic_services"
    )
    res <- res %>%
        filter(
            latin_name %in% !!species
        ) %>%
        filter(
            casestudy_id %in% !!case_study
        ) %>%
        filter(
            category_id %in% !!as.numeric(gsub("(.*)_.*", "\\1", ecosystem)),
            subcategory_id %in% !!as.numeric(gsub(".*_(.*)", "\\1", ecosystem))
        )


    res <- res %>%
        select(
            category_id,
            fish_name,
            casestudy_name,
            category_name,
            subcategory_name,
            esvalue_code,
            latin_name
        ) %>%
        mutate(
            fish_name = diadesatlas.translate(fish_name, !!r$lg),
            casestudy_name_tr = diadesatlas.translate(casestudy_name, !!r$lg),
            category_name_tr = diadesatlas.translate(category_name, !!r$lg),
            subcategory_name_tr = diadesatlas.translate(subcategory_name, !!r$lg),
        ) %>%
        collect() %>%
        left_join(
            golem::get_golem_options("species_list"),
            by = c("latin_name" = "latin_name")
        ) %>%
        mutate(
            fish_name = paste0("<span data-i18n='", low_and_sub(latin_name), "'>", fish_name, "</span>"),
            casestudy_name = paste0("<span data-i18n='", low_and_sub(casestudy_name), "'>", casestudy_name_tr, "</span>"),
            category = paste0(
                "[",
                paste0("<span data-i18n='", low_and_sub(category_name), "'>", category_name_tr, "</span>"),
                "] ",
                paste0("<span data-i18n='", low_and_sub(subcategory_name), "'>", subcategory_name_tr, "</span>")
            )
        )

    # HERE : do a join to get the local_name and build the translation

    data.frame(
        category_id = res$category_id,
        subcategory_name = res$subcategory_name,
        fish_name = res$fish_name,
        casestudy_name = res$casestudy_name,
        category = res$category,
        esvalue_code = res$esvalue_code
    ) %>%
        arrange(
            category_id,
            subcategory_name
        ) %>%
        select(-category_id, -subcategory_name)
}

low_and_sub <- function(x) {
    gsub(
        " ",
        "-",
        tolower(x)
    )
}