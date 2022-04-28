test_that("translation works", {
  translation <- translation()
  expect_df(translation)
  expected_output <- read.csv(
    app_sys("translation.csv")
  )
  expected_output$DESCRIPTION <- NULL
  expect_equal(
    translation,
    expected_output
  )
})

test_that("get_translation_entry", {
  translation <- translation()
  res <- get_translation_entry("title-first", "fr")
  expect_true(inherits(res, "character"))
  
  expected_output <- read.csv(
    app_sys("translation.csv")
  )
  expected_output$DESCRIPTION <- NULL
  expect_equal(
    get_translation_entry("title-first", "fr"),
    expected_output[
      expected_output$entry == "title-first",
      "fr"
    ]
  )
})

test_that("translation_help", {
  translation <- translation_help()
  expect_df(translation)
  expect_identical(
    sort(names(translation)),
    sort(c("entry", "en", "fr", "pt", "es"))
  )
})

test_that("get_help_bubble_entries", {
  help_bubble_entries <- get_help_bubble_entries()
  expect_type(
    help_bubble_entries,
    "character"
  )
  expect_true(
    all(
      # All entries end with "_help"
      grepl("_help$", help_bubble_entries)
    )
  )
})

test_that("translation_iucn", {
  translation <- translation_iucn()
  expect_df(translation)
  expected_output <- read.csv(
    app_sys("translation_iucn.csv")
  )
  expect_equal(
    translation,
    expected_output
  )
})

test_that("translation_species", {
  skip_if_not_connectable(session_globale)
  translation <- translation_species(session_globale)
  expect_df(translation)
  expect_equal(
    names(translation)[1],
    "entry"
  )
  expect_true(
    "entry" %in% names(translation)
  )
  expect_true(
    "en" %in% names(translation)
  )
  expect_true(
    "fr" %in% names(translation)
  )
})

test_that("translation_abundance_level", {
  skip_if_not_connectable(session_globale)
  translation <- translation_abundance_level(session_globale)
  expect_df(translation)
  expect_equal(
    names(translation)[1],
    "entry"
  )
  expect_true(
    "entry" %in% names(translation)
  )
  expect_true(
    "en" %in% names(translation)
  )
  expect_true(
    "fr" %in% names(translation)
  )
})

test_that("translation_v_ecosystemic_services", {
  skip_if_not_connectable(session_globale)
  translation <- translation_v_ecosystemic_services(session_globale)
  expect_df(translation)
  expect_equal(
    names(translation)[1],
    "entry"
  )
  expect_true(
    "entry" %in% names(translation)
  )
  expect_true(
    "en" %in% names(translation)
  )
  expect_true(
    "fr" %in% names(translation)
  )
})

test_that("build_language_json", {
  skip_if_not_connectable(session_globale)
  translation <- build_language_json(session_globale)
  expect_true(
    inherits(translation, "json")
  )
  expect_true(
    grepl("fr", translation)
  )
  expect_true(
    grepl("en", translation)
  )
})