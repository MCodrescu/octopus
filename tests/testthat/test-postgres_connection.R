### Docker is required to run these tests.
### Read more about docker at https://hub.docker.com/_/postgres

docker_working <-
  tryCatch({
    container_sha <-
      system(
        "docker run -e POSTGRES_PASSWORD=password -e POSTGRES_DB=example -p 5455:5432 -d postgres",
        intern = TRUE
      )
    TRUE
  }, error = \(x){
    FALSE
  })

if(docker_working){

  Sys.sleep(3)

  con <-
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = "localhost",
      user = "postgres",
      password = "password",
      dbname = "example",
      port = 5455
    )

  #-------------------------------------------------------------------------------

  test_that(
    "get_schemas retrieves schemas correctly",
    {

      expect_equal(
        get_schemas_postgres(con),
        c("information_schema", "pg_catalog", "pg_toast", "public")
      )

    }
  )

  test_that(
    "get_tables retrieve tables correctly",
    {

      DBI::dbWriteTable(
        con,
        name = DBI::Id(
          schema = "public",
          table = "mtcars"
        ),
        value = mtcars,
        overwrite = TRUE
      )

      expect_true(
        "mtcars" %in% get_tables_postgres(
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
        get_n_rows_postgres(
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
        get_n_rows_postgres(
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
        get_preview_postgres(
          con,
          schema = "public",
          table = "mtcars"
        ),
        mtcars_wo_rownames
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
        delete_table_postgres(
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

      write_table_postgres(
        con,
        schema = "example",
        table_name = "mtcars",
        data = mtcars
      )

      expect_true(
        "mtcars" %in% get_tables_postgres(con, schema = "example")
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

