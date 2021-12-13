# trad_latin <- system.file("reprex", "trad_latin1.csv", package = "diades.atlas")

# new_output_test <- tempfile(fileext = ".csv")

# test_that("check_translation_csv works with encoding Latin1", {
#   output <- check_translation_csv(
#     path = trad_latin,
#     source_encoding = "latin1", new_path = new_output_test)
#   expect_equal(names(output), c("entry", "en", "fr"))
#   expect_equal(as.character(output[1,]), c("toto", "t\u00f9t\u00f9", "t\u00e0t\u00e0"))
#   expect_equal(
#     readLines(new_output_test)[1],
#     c("\"entry\",\"en\",\"fr\"")
#   )

#   # Read re-encoded one: new_output_test
#   output <- check_translation_csv(
#     path = new_output_test,
#     source_encoding = "UTF-8", new_path = new_output_test)
#   expect_equal(names(output), c("entry", "en", "fr"))
#   expect_equal(as.character(output[1,]), c("toto", "t\u00f9t\u00f9", "t\u00e0t\u00e0"))
#   expect_equal(
#     readLines(new_output_test)[1],
#     c("\"entry\",\"en\",\"fr\"") # Copied with write.csv
#   )
# })

# # with uf8 ----
# trad_utf8 <- system.file("reprex", "trad_utf8.csv", package = "diades.atlas")
# new_output_test <- tempfile(fileext = ".csv")

# test_that("check_translation_csv works with encoding UTF8 directly", {
#   output <- check_translation_csv(
#     path = trad_utf8,
#     source_encoding = "UTF-8", new_path = new_output_test)
#   expect_equal(names(output), c("entry", "en", "fr"))
#   expect_equal(as.character(output[1,]), c("toto", "t\u00f9t\u00f9", "t\u00e0t\u00e0"))
#   expect_equal(
#     readLines(new_output_test)[1],
#     c("entry,en,fr") # Copied as is
#   )
# })