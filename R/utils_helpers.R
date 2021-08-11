#' @importFrom utils read.csv
build_language_json <- function(){
  lg <- read.csv(
    app_sys("translation.csv")
  )
  
  build_entry <- function(subset){
    x <- list(
      translation = as.list(lg[[subset]])
    )
    names(x$translation) <- lg$entry
    x
  }
  
  list(
    en = build_entry("en"),
    fr = build_entry("fr")
  ) %>% jsonlite::toJSON(auto_unbox =  TRUE)
  
}

with_multilg <- function(fun, i18n, default){
  purrr::partial(
    fun, 
    label = with_i18(
      tags$span(
        default
      ), 
      i18n
    )
  )
}

get_dt_lg <- function(lg){
  list(
    url = switch(
      lg, 
      en = "//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json", 
      fr = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json"
    )
  )
}