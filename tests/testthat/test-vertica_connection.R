### Docker is required to run these tests.
### Read more about docker at https://hub.docker.com/_/postgres

docker_working <-
  tryCatch({
    container_sha <-
      system(
        "docker run -d -p 5433:5433 -e APP_DB_USER=newdbadmin -e APP_DB_PASSWORD=vertica vertica/vertica-ce",
        intern = TRUE
      )

    Sys.sleep(180)

    con <- DBI::dbConnect(
      odbc::odbc(),
      dsn = "Vertica_Test",
      pwd = "vertica"
    )
    TRUE
  }, error = \(x){
    FALSE
  })


if(docker_working){

  #-------------------------------------------------------------------------------

  test_that(
    "get_schemas retrieves schemas correctly",
    {

      expect_equal(
        get_schemas_vertica(con),
        c("online_sales", "public", "store", "v_catalog", "v_func", "v_internal", "v_monitor", "v_txtindex")
      )

    }
  )

  test_that(
    "get_tables retrieve tables correctly",
    {

      mtcars_2 <- mtcars

      mtcars_2["order"] <- seq_len(
        nrow(mtcars)
      )

      DBI::dbWriteTable(
        con,
        name = DBI::Id(
          schema = "public",
          table = "mtcars"
        ),
        value = mtcars_2,
        overwrite = TRUE,
        row.names = FALSE
      )

      expect_true(
        "mtcars" %in% get_tables_vertica(
          con,
          schema = "public"
        )
      )

    }
  )

  test_that(
    "get_n_rows retrieves the correct number of rows of a table",
    {
      expect_equal(
        get_n_rows_vertica(
          con,
          schema = "public",
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
        get_n_rows_vertica(
          con,
          schema = "public",
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
        get_preview_vertica(
          con,
          schema = "public",
          table = "mtcars"
        ) |>
          dplyr::arrange(order) |>
          dplyr::select(-c(order)),
        mtcars_wo_rownames
      )
    }
  )

  test_that(
    "a create table query works correcty",
    {
      n_rows = get_n_rows_vertica(
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
        delete_table_vertica(
          con,
          schema = "public",
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

      res <- DBI::dbSendQuery(con, "CREATE SCHEMA example")
      DBI::dbClearResult(res)

      write_table_vertica(
        con,
        schema = "example",
        table_name = "mtcars",
        data = mtcars
      )

      expect_true(
        "mtcars" %in% get_tables_vertica(con, schema = "example")
      )

    }
  )

  test_that(
    "a join query returns the correct number of rows",
    {
      table_1 <-
        data.frame(
          x = c(1, 2, 3),
          y = c("A", "B", "C")
        )

      table_2 <-
        data.frame(
          z = c(4, 5, 6),
          y = c("A", "B", "C")
        )

      DBI::dbWriteTable(con, "table_1", table_1)
      DBI::dbWriteTable(con, "table_2", table_2)

      expect_equal(
        get_n_rows_vertica(
          con = con,
          schema = "",
          table = "",
          query = "SELECT * FROM table_1 INNER JOIN table_2 USING (y)"
        ) |> as.numeric(),
        3
      )
    }
  )

  #-------------------------------------------------------------------------------

  DBI::dbDisconnect(con)

  system(
    glue::glue(
      "docker stop {container_sha}"
    ),
    intern = TRUE
  )

  Sys.sleep(3)

  system(
    glue::glue(
      "docker rm {container_sha}"
    ),
    intern = TRUE
  )

}

