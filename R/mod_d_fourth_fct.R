multi_sliders <- function(ns, countries, prefix = "period1") {
  tagList(
    purrr::map(
      countries, ~{sliderInput(
      ns(paste(prefix, .x, sep = "-")),
      with_i18(.x, .x),
      min = 0.1,
      max = 2,
      value = 0.2
    )}
    )
  )
}

