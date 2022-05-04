#' Transform a sentence into a proper slug
#' @param x Character
#' @noRd
slugify <- function(x) {
  gsub("\\s+|_", "-", tolower(x))
}

#' Create a list of sliders UI inputs
#' 
#' @param ns session$ns function
#' @param countries Character vector
#' @param prefix prefix to add to the InputId of the sliderInput
#' @noRd
multi_sliders <- function(ns, countries, prefix = "period1") {
  countries_slug <- slugify(countries)
  tagList(
    purrr::map2(
      countries, countries_slug, ~{sliderInput(
      ns(paste(prefix, .y, sep = "-")),
      with_i18(.x, .y),
      min = 0.1,
      max = 2,
      value = 0.2
    )}
    )
  )
}

