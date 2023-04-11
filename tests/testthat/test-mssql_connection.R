### Read more about docker at https://hub.docker.com/_/microsoft-mssql-server

docker_working <-
  tryCatch({
    container_sha <-
      system(
        "docker run -e ACCEPT_EULA=Y -e \"SA_PASSWORD=msyq74982!\" -p 1433:1433 --name sql1 -h sql1 -d mcr.microsoft.com/mssql/server:2019-latest",
        intern = TRUE
      )

    Sys.sleep(60)

    # MS SQL Server ODBC Driver https://learn.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver16
    # Set up using ODBC Driver with Server = 127.0.0.1, 1433
    con <- DBI::dbConnect(
      odbc::odbc(),
      uid = "sa",
      pwd = "msyq74982!",
      dsn = "mssql_test"
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
        get_schemas_mssql(con),
        c("dbo", "guest", "INFORMATION_SCHEMA", "sys", "db_owner", "db_accessadmin",
          "db_securityadmin", "db_ddladmin", "db_backupoperator", "db_datareader",
          "db_datawriter", "db_denydatareader", "db_denydatawriter")
      )

    }
  )

  test_that(
    "get_tables retrieve tables correctly",
    {

      res <- DBI::dbSendQuery(con, "CREATE DATABASE example")
      DBI::dbClearResult(res)

      result <- write_table_mssql(
        con,
        schema = "example",
        table_name = "mtcars",
        data = mtcars
      )

      expect_true(
        "mtcars" %in% get_tables_mssql(
          con,
          schema = "example"
        )
      )

    }
  )

  test_that(
    "get_n_rows retrieves the correct number of rows of a table",
    {
      expect_equal(
        get_n_rows_mssql(
          con,
          schema = "example",
          table = "mtcars"
        ) |> as.numeric(),
        nrow(mtcars) |> as.numeric()
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
        get_preview_mssql(
          con,
          schema = "example",
          table = "mtcars"
        ),
        mtcars_wo_rownames
      )
    }
  )


  test_that(
    "a create table query works correcty",
    {
      n_rows = get_n_rows_mssql(
        con = con,
        schema = "",
        table = "",
        "SELECT * INTO mtcars_2 FROM mtcars"
      )

      submit_query(
        "SELECT * INTO mtcars_2 FROM mtcars",
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

      expect_equal(
        "Success",
        delete_table_mssql(
          con,
          schema = "example",
          table = "mtcars"
        )
      )

      expect_false(
        "mtcars" %in% get_tables_mssql(con, schema = "example")
      )

    }
  )

  test_that(
    "write_table correctly upload table",
    {

      res <- DBI::dbSendQuery(con, "CREATE DATABASE example_2")
      DBI::dbClearResult(res)

      write_table_mssql(
        con,
        schema = "example_2",
        table_name = "mtcars",
        data = mtcars
      )

      expect_true(
        "mtcars" %in% get_tables_mssql(con, schema = "example_2")
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

      write_table_mssql(
        con,
        schema = "example",
        table_name = "table_1",
        data = table_1
      )

      write_table_mssql(
        con,
        schema = "example",
        table_name = "table_2",
        data = table_2
      )

      expect_equal(
        get_n_rows_mssql(
          con = con,
          schema = "example",
          table = "",
          query = "SELECT x, table_1.y, z FROM table_1 JOIN table_2 ON table_1.y = table_2.y"
        ),
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

