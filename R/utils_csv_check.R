#' csv_check
#' 
#' @description Check the CSV written by hand
#'
#' @param path path to file to check
#' @param source_encoding Encoding of the source file 
#' @param new_path Where to save the modified file
#' @return The return value, if any, from executing the utility.
#' @importFrom dplyr mutate_all
#' @export
check_translation_csv <- function(path, source_encoding = "latin1", new_path = path) {
  if (!file.exists(path)) {
    stop("File doesn't  exists")
  }
  if (tools::file_ext(path) != "csv") {
    stop("path should be a csv file")
  }
  if (tools::file_ext(new_path) != "csv") {
    stop("new_path should be a csv file")
  }
  fls <- read.csv(path, encoding = source_encoding)
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
  if (source_encoding != "UTF-8") {
    fls <- fls %>% mutate_all(enc2utf8)
    write.csv(fls, file = new_path, 
              fileEncoding = "UTF-8",
              # col.names = TRUE, 
              row.names = FALSE)
    cli::cli_alert_success("CSV was re-encoded to UTF-8")
  } else {
    file.copy(path, new_path)
  }
  cli::cli_alert_success("CSV seems correct")
  
  return(fls)
}