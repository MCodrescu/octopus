### Docker is required to run these tests.
### Read more about docker at https://hub.docker.com/_/mysql

docker_working <-
  tryCatch({
    container_sha <-
      system(
        "docker run -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=admin -e MYSQL_PASSWORD=password -e MYSQL_DATABASE=example -p 3306:3306 -d mysql",
        intern = TRUE
      )

    Sys.sleep(20)

    con <-
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = "127.0.0.1",
        username = "root",
        password = "password",
        dbname = "example",
        port = 3306
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
        get_schemas_mysql(con),
        c("example", "information_schema", "mysql", "performance_schema", "sys")
      )

    }
  )

  test_that(
    "get_tables retrieve tables correctly",
    {

      DBI::dbSendQuery(
        con,
        "SET @@GLOBAL.local_infile = 1;"
      )

      result <- write_table_mysql(
        con,
        schema = "example",
        table_name = "mtcars",
        data = mtcars
      )

      expect_true(
        "mtcars" %in% get_tables_mysql(
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
        get_n_rows_mysql(
          con,
          schema = "example",
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
        get_n_rows_mysql(
          con,
          schema = "example",
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
        get_preview_mysql(
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
      n_rows = get_n_rows_mysql(
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

      expect_equal(
        "Success",
        delete_table_mysql(
          con,
          schema = "example",
          table = "mtcars"
        )
      )

      expect_false(
        "mtcars" %in% get_tables_mysql(con, schema = "example")
      )

    }
  )

  test_that(
    "write_table correctly upload table",
    {

      res <- DBI::dbSendQuery(con, "CREATE SCHEMA example_2")
      DBI::dbClearResult(res)

      write_table_mysql(
        con,
        schema = "example_2",
        table_name = "mtcars",
        data = mtcars
      )

      expect_true(
        "mtcars" %in% get_tables_mysql(con, schema = "example_2")
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

      write_table_mysql(
        con,
        schema = "example",
        table_name = "table_1",
        data = table_1
      )

      write_table_mysql(
        con,
        schema = "example",
        table_name = "table_2",
        data = table_2
      )

      expect_equal(
        get_n_rows_mysql(
          con = con,
          schema = "",
          table = "",
          query = "SELECT * FROM table_1 INNER JOIN table_2 USING (y)"
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

