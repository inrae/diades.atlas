session <- shiny::MockShinySession$new()
is_connectable <- tryCatch({
    connect(session)
})

if (attempt::is_try_error(is_connectable)) {
    session$userData$is_connectable <- FALSE
} else {
    session$userData$is_connectable <- TRUE
}

skip_if_not_connectable <- function(session) {
    skip_if_not(
        session$userData$is_connectable
    )
}

expect_df <- function(df) {
    expect_true(
        inherits(df, "data.frame")
    )
}
