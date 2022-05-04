# slugify ----
test_that("slugify works", {
  expect_equal(slugify("United  Kingdom"), "united-kingdom")
})

# multi_sliders ----
test_that("multi_sliders works", {
  skip_if_not_connectable(session_globale)
  ns <- session_globale$ns
  output <- multi_sliders(ns = ns, countries = c("France", "United Kingdom"))

  # long output ----
  expected_output <-
'<div class="form-group shiny-input-container">
  <label class="control-label" id="mock-session-period1-france-label" for="mock-session-period1-france">
    <span data-i18n="france">France</span>
  </label>
  <input class="js-range-slider" id="mock-session-period1-france" data-skin="shiny" data-min="0.1" data-max="2" data-from="0.2" data-step="0.02" data-grid="true" data-grid-num="9.5" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
</div>
<div class="form-group shiny-input-container">
  <label class="control-label" id="mock-session-period1-united-kingdom-label" for="mock-session-period1-united-kingdom">
    <span data-i18n="united-kingdom">United Kingdom</span>
  </label>
  <input class="js-range-slider" id="mock-session-period1-united-kingdom" data-skin="shiny" data-min="0.1" data-max="2" data-from="0.2" data-step="0.02" data-grid="true" data-grid-num="9.5" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
</div>'
  # end of long output ----
  expect_equal(as.character(output), expected_output)
})
