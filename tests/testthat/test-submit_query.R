con <- DBI::dbConnect(
  RSQLite::SQLite()
)

test_that(
  "queries with results too large are not returned",
  {

    example_data <-
      data.frame(
        x = seq_len(50001)
      )

    write_table_sqlite(
      con,
      schema = "main",
      table_name = "example_data",
      data = example_data
    )

    n_rows <- get_n_rows_sqlite(
      con = con,
      schema = "",
      table = "",
      query = "SELECT * FROM example_data"
    )

    expect_error(
      submit_query(
        query = query,
        con = con,
        n_rows = n_rows
      )
    )

  }
)

test_that(
  "blank queries cause an error",
  {

    query <- ""

    n_rows <- get_n_rows_sqlite(
      con = con,
      schema = "",
      table = "",
      query = query
    )

    expect_error(
      submit_query(
        query = query,
        con = con,
        n_rows = n_rows
      )
    )

  }
)

test_that(
  "select queries return a result",
  {

    example_data <-
      data.frame(
        x = c(1, 2, 3),
        y = c(4, 5, 6)
      )

    write_table_sqlite(
      con,
      schema = "main",
      table_name = "example_data",
      data = example_data
    )

    query <- "SELECT * FROM example_data"

    n_rows <- get_n_rows_sqlite(
      con = con,
      schema = "",
      table = "",
      query = query
    )

    expect_equal(
      example_data,
      submit_query(
        query = query,
        con = con,
        n_rows = n_rows
      )
    )

  }
)

test_that(
  "create queries return only a success message",
  {

    query <- "CREATE TABLE example_2 AS SELECT * FROM example_data"

    n_rows <- get_n_rows_sqlite(
      con = con,
      schema = "",
      table = "",
      query = query
    )

    expect_equal(
      submit_query(
        query = query,
        con = con,
        n_rows = n_rows
      ),
      data.frame(result = "Success")
    )
  }
)

test_that(
  "queries with syntax errors return errors",
  {

    query <- "SELECT * FRM example_data"

    n_rows <- get_n_rows_sqlite(
      con = con,
      schema = "",
      table = "",
      query = query
    )

    expect_error(
      submit_query(
        query = query,
        con = con,
        n_rows = n_rows
      )
    )
  }
)

DBI::dbDisconnect(con)
