#' A Database Specific Function To Retrieve Database Schemas
#'
#' @param con A database connection object.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr pull
#'
#' @return A character vector of all schemas in the database.
get_schemas_postgres <- function(con) {
  schemas <-
    DBI::dbGetQuery(
      con,
      "
      SELECT schema_name
      FROM information_schema.schemata
      ORDER BY schema_name;
      "
    ) |>
    dplyr::pull(
      schema_name
    )
}

#' A Database Specific Function to Retrieve All Database Tables
#'
#' @param con A database connection object.
#' @param schema A
#'
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom dplyr pull
#'
#' @return A character vector of all tables in a given schema.
get_tables_postgres <- function(con, schema) {
  result <- tryCatch({
    DBI::dbGetQuery(
      con,
      glue::glue(
        "
      SELECT *
      FROM information_schema.tables
      WHERE table_schema = '{schema}'
      ORDER BY table_name;
      "
      )
    ) |>
      dplyr::pull(
        table_name
      )
  }, error = function(error){
    data.frame(
      error = error$message
    )
  })

}

#' A Database Specific Function to Retrieve the Number of Rows of a Table
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom dplyr pull
#'
#' @return An integer for the number of rows in the table.
get_n_rows_postgres <- function(con, schema, table, query = "") {

  if (query != ""){
    query_string <-
      glue::glue(
        "
        WITH cte1 AS (
          {query}
        )
        SELECT
          COUNT(*)
        FROM cte1
        "
      )
  } else {
    query_string <-
      glue::glue(
        "
        WITH cte1 AS (
          SELECT *
          FROM \"{schema}\".\"{table}\"
        )
        SELECT
          COUNT(*)
        FROM cte1
        "
      )
  }

  result <-
    tryCatch({
      result <-
        DBI::dbGetQuery(
          con,
          query_string
        ) |>
        dplyr::pull(
          count
        )
    }, error = function(error){
      result <- 0
    })


}

#' A Database Specific Function for Retrieving Table Previews
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#'
#' @return A data frame of 100 rows from the database table.
get_preview_postgres <- function(con, schema, table) {
  dbGetQuery(
    con,
    glue(
      "
      SELECT *
      FROM \"{schema}\".\"{table}\"
      LIMIT 100;
      "
    )
  )
}


#' A Database Specific Function for Dropping Tables
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#'
#' @return A result string. Either "Success" or an error message.
delete_table_postgres <- function(con, schema, table){

  result <-
    tryCatch({
      DBI::dbSendQuery(
        con,
        glue::glue(
          "DROP TABLE \"{schema}\".\"{table}\""
        )
      )
      result <- "Success"
    }, error = function(error){
      result <- error$message
    })

}


#' A Database Specific Function for Uploading Data Frames
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table_name A string for the new table name.
#' @param data A data frame to be uploaded.
#' @param temporary A logical value. Should the table be temporary?
#'
#' @return A result string. Either "Success" or an error message.
write_table_postgres <-
  function(
    con,
    schema,
    table_name,
    data,
    temporary = FALSE
  ){

    result <-
      tryCatch({
        DBI::dbWriteTable(
          con,
          name = DBI::Id(
            table = table_name,
            schema = schema
          ),
          value = data.frame(data),
          overwrite = TRUE,
          temporary = temporary
        )

        result <- "Success"
      }, error = function(error){
        result <- error$message
      })

}
