test_that(
  "establish_connection_postgres works with good credentials",
  {
    result <-
      establish_connection_postgres(
        data.frame(
          host = "localhost",
          user = "postgres",
          password = "blackcar",
          port = 5432,
          dbname = "marcus"
        )
      )
    expect_s4_class(
      result[[1]],
      "PqConnection"
    )
    expect_type(
      result[[2]],
      "character"
    )

    DBI::dbDisconnect(result[[1]])
  }
)

test_that(
  "establish_connection_postgres catches bad credentials",
  {
    result <-
      establish_connection_postgres(
        data.frame(
          host = "localhost",
          user = "postgres",
          password = "whitecar",
          port = 5432,
          dbname = "marcus"
        )
      )
    expect_identical(
      result[[1]],
      NA
    )

    expect_type(
      result[[2]],
      "character"
    )
  }
)


test_that(
  "get_schemas returns a character vector of schemas",
  {
    result <-
      establish_connection_postgres(
        data.frame(
          host = "localhost",
          user = "postgres",
          password = "blackcar",
          port = 5432,
          dbname = "marcus"
        )
      )
    expect_type(
      get_schemas_postgres(
        result[[1]]
      ),
      "character"
    )

    DBI::dbDisconnect(result[[1]])
  }
)

test_that(
  "get_tables_postgres returns a character vector of tables with a correct schema",
  {
    result <-
      establish_connection_postgres(
        data.frame(
          host = "localhost",
          user = "postgres",
          password = "blackcar",
          port = 5432,
          dbname = "marcus"
        )
      )

    expect_type(
      get_tables_postgres(
        result[[1]],
        "public"
      ),
      "character"
    )

    DBI::dbDisconnect(result[[1]])
  }
)
