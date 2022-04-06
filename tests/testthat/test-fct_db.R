test_that("db connection works", {
  #readLines("http://localhost:5432")
  skip_on_ci()
  session <- new.env()
  x <- try({
    connect(session)
  }, silent = TRUE)
  skip_if(inherits(x, "try-error"))
  
  con_object <- get_con(session)
  expect_true(
    inherits(con_object, "PqConnection")
  )
  info_ <- DBI::dbGetInfo(con_object)
  # Check the connection is correct
  expect_equal(
    info_$dbname,
    get_golem_config("POSTGRES_DBNAME")
  )
  expect_equal(
    info_$host,
    get_golem_config("POSTGRES_HOST")
  )
  expect_equal(
    info_$port,
    get_golem_config("POSTGRES_PORT")
  )
  expect_equal(
    info_$username,
    Sys.getenv("POSTGRES_USER")
  )
})
