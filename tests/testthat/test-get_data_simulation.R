test_that("get_data_simulation works", {
  skip_if_not_connectable(session_globale)
  
  data_simulation <- get_data_simulation(get_con(session_globale))
  get_nrow <- function(x) count(x) %>% collect() %>% pull(n)
  
  # No empty tables
  for (table_n in length(data_simulation)) {
    expect_true(
      data_simulation[[table_n]] %>% get_nrow > 0
    )
  }
})
