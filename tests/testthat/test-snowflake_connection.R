## A Snowflake database is required for these tests.
## You can get a free trial at https://signup.snowflake.com/

if (keyring::key_get("SnowflakeTrialPassword") != ""){

  con <-
    DBI::dbConnect(
      odbc::odbc(),
      dsn = "Snowflake_Trial",
      pwd = keyring::key_get("SnowflakeTrialPassword")
    )

  test_that(
    "get_schemas retrieves schemas correctly",
    {

      expect_equal(
        get_schemas_snowflake(con),
        c("INFORMATION_SCHEMA", "PUBLIC")
      )

    }
  )

  test_that(
    "get_tables retrieves tables correctly",
    {

      mtcars_2 <- mtcars

      rownames(mtcars_2) <-
        seq_len(nrow(mtcars))

      DBI::dbWriteTable(
        con,
        name = DBI::Id(
          schema = "PUBLIC",
          table = "MTCARS"
        ),
        value = mtcars_2,
        overwrite = TRUE,
        row.names = TRUE
      )

      expect_true(
        "MTCARS" %in% get_tables_snowflake(
          con,
          schema = "PUBLIC"
        )
      )

    }
  )

  test_that(
    "get_n_rows retrieves the correct number of rows of a table",
    {
      expect_equal(
        get_n_rows_snowflake(
          con,
          schema = "PUBLIC",
          table = "MTCARS"
        ) |> as.numeric(),
        nrow(mtcars) |> as.numeric()
      )
    }
  )

  test_that(
    "get_n_rows retrieves the correct number of rows of a query",
    {
      expect_equal(
        get_n_rows_snowflake(
          con,
          schema = "PUBLIC",
          table = "MTCARS",
          query = "SELECT * FROM MTCARS LIMIT 10"
        ) |> as.numeric(),
        10
      )
    }
  )

  test_that(
    "get_preview returns a view of the dataframe",
    {

      # Note: Snowflake writes tables in a different order

      mtcars_2 <- mtcars

      rownames(mtcars_2) <-
        seq_len(nrow(mtcars))

      expect_equal(
        get_preview_snowflake(
          con,
          schema = "PUBLIC",
          table = "MTCARS"
        ) |>
          dplyr::mutate(
            row_names = readr::parse_number(
              row_names
            )
          ) |>
          dplyr::arrange(row_names) |>
          dplyr::select(-c("row_names")),
        mtcars_2
      )
    }
  )

  test_that(
    "delete_table correctly drops the table",
    {
      expect_true(
        "MTCARS" %in% DBI::dbListTables(con)
      )

      expect_equal(
        "Success",
        delete_table_snowflake(
          con,
          schema = "PUBLIC",
          table = "MTCARS"
        )
      )

      expect_false(
        "MTCARS" %in% DBI::dbListTables(con)
      )

    }
  )

  test_that(
    "write_table correctly uploads table",
    {

      DBI::dbSendQuery(con, "CREATE SCHEMA EXAMPLE") |>
        DBI::dbClearResult()

      DBI::dbSendQuery(con, "USE SCHEMA EXAMPLE") |>
        DBI::dbClearResult()

      write_table_snowflake(
        con,
        schema = "EXAMPLE",
        table_name = "MTCARS",
        data = mtcars
      )

      expect_true(
        "MTCARS" %in% get_tables_snowflake(con, schema = "EXAMPLE")
      )

      DBI::dbSendQuery(con, "DROP SCHEMA EXAMPLE") |>
        DBI::dbClearResult()

    }
  )

  DBI::dbDisconnect(con)
}

