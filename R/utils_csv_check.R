#' csv_check
#'
#' @description Check the CSV written by hand
#'
#' @return The return value, if any, from executing the utility.
#'
#' @export
check_translation_csv <- function(path) {
    if (!file.exists(path)) {
        stop("File doesn't  exists")
    }
    if (tools::file_ext(path) != "csv") {
        stop("File should be a csv file")
    }
    fls <- read.csv(path)
    if (
        names(fls)[1] != "entry"
    ) {
        stop("The first column should be named 'entry'")
    }
    if (
        !"en" %in% names(fls)
    ) {
        stop("Their is no 'en' column")
    }
    if (
        !"fr" %in% names(fls)
    ) {
        stop("Their is no 'fr' column")
    }
    cli::cli_alert_success("CSV seems correct")
}