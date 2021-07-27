shiny::testServer(app_server, {
  skip_on_ci()
  connect()
  con_object <- get_con()
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
    as.numeric(info_$port),
    get_golem_config("POSTGRES_PORT")
  )
  expect_equal(
    info_$username,
    Sys.getenv("POSTGRES_USER")
  )
})
