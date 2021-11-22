pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = FALSE)

se <- new.env()
connect(se)

species_list <- DBI::dbGetQuery(
    get_con(se),
    "SELECT *, diadesatlas.translate(english_name, 'fr') AS french_name from diadesatlas.species WHERE active=TRUE"
)

species_list %>%
    dplyr::select(entry = local_name, en = english_name, fr = french_name) %>%
    readr::write_csv("inst/translation_species.csv")

# IUCN

# TODO : get the translation inside the DB
stats <- strsplit(
    # https://uicn.fr/liste-rouge-mondiale/
    "Éteinte (EX), Éteinte à l’état sauvage (EW), En danger critique (CR), En danger (EN), Vulnérable (VU), Quasi menacée (NT), Préoccupation mineure (LC), Données insuffisantes (DD), Non évaluée (NE)",
    split = ", "
)[[1]]

french_iucn <- data.frame(
    fr_vals = gsub(
        "([^\\(]+) \\(([^\\(]+)\\)",
        "\\1,\\2",
        stats
    )
) %>%
    tidyr::separate(
        fr_vals,
        into = c("french_name", "iucn_level_code"), sep = ","
    )

session <- new.env()
connect(session)

en_iucn <- DBI::dbGetQuery(get_con(session), "select distinct iucn_level_code,iucn_level_name AS english_name from v_iucn")

en_iucn %>%
    dplyr::full_join(french_iucn) %>%
    dplyr::select(entry = iucn_level_code, en = english_name, fr = french_name) %>%
    readr::write_csv("inst/translation_iucn.csv")

# TODO écrire depuis la base

DBI::dbGetQuery(
    con,
    "select * from abundance_level"
) %>%
    select(
        entry = abundance_level_name,
        en = abundance_level_interpretation_short
    ) %>%
    mutate(
        fr = c(
            "Non enregistré sur la période",
            "Présence occasionnelle",
            "Populations fonctionnelles",
            "Populations fonctionnelles abondante"
        )
    ) %>%
    readr::write_csv("inst/translation_abundance_level.csv")