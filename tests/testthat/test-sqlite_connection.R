### RSQLite Package is Required to Run These Tests

con <-
  DBI::dbConnect(
    RSQLite::SQLite(),
    ":memory:"
  )

#-------------------------------------------------------------------------------

test_that(
  "get_schemas retrieves schemas correctly",
  {

    expect_equal(
      get_schemas_sqlite(con),
      c("main")
    )

  }
)

test_that(
  "get_tables retrieve tables correctly",
  {

    DBI::dbWriteTable(
      con,
      name = "mtcars",
      value = mtcars,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      con,
      name = "mtcars_temp",
      value = mtcars,
      overwrite = TRUE,
      temporary = TRUE
    )

    expect_true(
      "mtcars" %in% get_tables_sqlite(
        con,
        schema = "main"
      )
    )

    expect_true(
      "mtcars_temp" %in% get_tables_sqlite(
        con,
        schema = "temp"
      )
    )

  }
)

test_that(
  "get_n_rows retrieves the correct number of rows of a table",
  {
    expect_equal(
      get_n_rows_sqlite(
        con,
        schema = "main",
        table = "mtcars"
      ) |> as.numeric(),
      nrow(mtcars) |> as.numeric()
    )
  }
)

test_that(
  "get_n_rows retrieves the correct number of rows of a query",
  {
    expect_equal(
      get_n_rows_sqlite(
        con,
        schema = "main",
        table = "mtcars",
        query = "SELECT * FROM mtcars LIMIT 10"
      ) |> as.numeric(),
      10
    )
  }
)

test_that(
  "get_preview returns a view of the dataframe",
  {
    mtcars_wo_rownames <-
      mtcars

    rownames(mtcars_wo_rownames) <-
      NULL

    expect_equal(
      get_preview_sqlite(
        con,
        schema = "main",
        table = "mtcars"
      ),
      mtcars_wo_rownames
    )
  }
)

test_that(
  "a create table query works correcty",
  {
    n_rows = get_n_rows_sqlite(
      con = con,
      schema = "",
      table = "",
      "CREATE TABLE mtcars_2 AS SELECT * FROM mtcars"
    )

    submit_query(
      "CREATE TABLE mtcars_2 AS SELECT * FROM mtcars",
      con = con,
      n_rows = n_rows
    )

    expect_true(
      "mtcars_2" %in% DBI::dbListTables(con)
    )
  }
)

test_that(
  "delete_table correctly drops the table",
  {
    expect_true(
      "mtcars" %in% DBI::dbListTables(con)
    )

    expect_equal(
      "Success",
      delete_table_sqlite(
        con,
        schema = "main",
        table = "mtcars"
      )
    )

    expect_false(
      "mtcars" %in% DBI::dbListTables(con)
    )

  }
)

test_that(
  "write_table correctly upload table",
  {

    write_table_sqlite(
      con,
      schema = "main",
      table_name = "mtcars",
      data = mtcars
    )

    expect_true(
      "mtcars" %in% get_tables_sqlite(con, schema = "main")
    )

  }
)

#-------------------------------------------------------------------------------

DBI::dbDisconnect(con)
