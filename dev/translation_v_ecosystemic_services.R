pkgload::load_all(helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = FALSE)

se <- new.env()
connect(se)


#  [1] "category_id"      "casestudy_id"     "species_id"       "subcategory_id"
#  [5] "esvalue_id"       "subcategory_name" "category_name"    "latin_name"
#  [9] "simplified_name"  "fish_name"        "casestudy_name"   "esvalue_code"
# [13] "esvalue_name"     "sortorder"
dplyr::bind_rows(
    DBI::dbGetQuery(
        get_con(se),
        "SELECT
        REPLACE(LOWER(casestudy_name), ' ', '-') as entry,
        casestudy_name as en,
        diadesatlas.translate(casestudy_name, 'fr') as fr
        from v_ecosystemic_services"
    ),
    DBI::dbGetQuery(
        get_con(se),
        "SELECT
        REPLACE(LOWER(category_name), ' ', '-') as entry,
        category_name as en,
        diadesatlas.translate(category_name, 'fr') as fr
        from v_ecosystemic_services"
    ),
    DBI::dbGetQuery(
        get_con(se),
        "SELECT
        REPLACE(LOWER(subcategory_name), ' ', '-') as entry,
        subcategory_name as en,
        diadesatlas.translate(subcategory_name, 'fr') as fr
        from v_ecosystemic_services"
    ),
    DBI::dbGetQuery(
        get_con(se),
        "SELECT
        REPLACE(LOWER(subcategory_name), ' ', '-') as entry,
        subcategory_name as en,
        diadesatlas.translate(subcategory_name, 'fr') as fr
        from v_ecosystemic_services"
    )
) %>%
    unique() %>%
    readr::write_csv("inst/translation_v_ecosystemic_services.csv")