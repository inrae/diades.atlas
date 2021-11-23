make_new_help <- function(name) {
    dir.create(
        sprintf("inst/translation_help/%s", name)
    )
    file.create(
        sprintf("inst/translation_help/%s/fr.md", name)
    )
    write(
        sprintf("# %s\n\n%s FR", name, name),
        sprintf("inst/translation_help/%s/fr.md", name),
        append = TRUE
    )
    file.create(
        sprintf("inst/translation_help/%s/en.md", name)
    )
    write(
        sprintf("# %s\n\n%s EN", name, name),
        sprintf("inst/translation_help/%s/en.md", name),
        append = TRUE
    )
    cat(
        sprintf(
            'w3_help_button(
    "Explanation Species",
    "%s"
)
', name
        )
    )
}

make_new_help("species_modal_help")
make_new_help("map_geometrie_help")
make_new_help("catch_bycatch_help")
make_new_help("conservation_status_help")
make_new_help("select_ecosystem_help")
make_new_help("select_casestudy_help")
make_new_help("show_results_help")
make_new_help("situation_table_help")
make_new_help("choose_a_scenario_help")
make_new_help("choose_a_daterange_help")
make_new_help("map_abundance_help")
make_new_help("plot_evolution_help")
make_new_help("define_anthropo_help")
make_new_help("run_simulation_help")
make_new_help("prediction_map_abundance_help")
make_new_help("prediction_plot_abundance_help")

unlink("inst/translation_help.csv")

purrr::map_df(
    list.files(
        "inst/translation_help",
        full.names = TRUE
    ), ~ {
        data.frame(
            entry = basename(.x),
            en = htmltools::includeMarkdown(
                file.path(.x, "en.md")
            ),
            fr = htmltools::includeMarkdown(
                file.path(.x, "fr.md")
            )
        )
    }
) %>%
    readr::write_csv("inst/translation_help.csv")
