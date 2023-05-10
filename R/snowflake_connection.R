#' A Database Specific Function To Retrieve Database Schemas
#' @noRd
#'
#' @param con A database connection object.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr pull
#'
#' @return A character vector of all schemas in the database.
get_schemas_snowflake <- function(con) {
  tryCatch({
    schemas <-
      DBI::dbGetQuery(
        con,
        "
        SELECT schema_name
        FROM information_schema.schemata
        ORDER BY schema_name;
        "
      ) |>
      dplyr::pull(1)
  }, error = function(error){
    error$message
  })

}

#' A Database Specific Function to Retrieve All Database Tables
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A
#'
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom dplyr pull
#'
#' @return A character vector of all tables in a given schema.
get_tables_snowflake <- function(con, schema) {
  result <- tryCatch({
    DBI::dbGetQuery(
      con,
      glue::glue(
        "
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = '{schema}'
        ORDER BY table_name;
        "
      )
    ) |>
      dplyr::pull(1)

  }, error = function(error){
    data.frame(
      error = error$message
    )
  })

}

#' A Database Specific Function to Retrieve the Number of Rows of a Table
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#' @param query A string containing the query to send.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom dplyr pull
#'
#' @return An integer for the number of rows in the table.
get_n_rows_snowflake <- function(con, schema, table, query = "") {

  if (query != ""){
    if (!grepl("^SELECT|^WITH", trimws(query), ignore.case = TRUE)){
      return(0)
    }
    query_string <-
      glue::glue(
        "
        SELECT
          COUNT(*)
        FROM (
          {query}
        ) AS subquery;
        "
      )
  } else {
    query_string <-
      glue::glue(
        "
        SELECT
          COUNT(*)
        FROM (
          SELECT *
          FROM \"{schema}\".\"{table}\"
        ) AS subquery;
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
        dplyr::pull(1)

    }, error = function(error){
      print(error$message)
      result <- 0
    })


}

#' A Database Specific Function for Retrieving Table Previews
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#'
#' @return A data frame of 100 rows from the database table.
get_preview_snowflake <- function(con, schema, table) {
  dbGetQuery(
    con,
    glue(
      "
      SELECT *
      FROM \"{schema}\".\"{table}\"
      LIMIT 100
      "
    )
  )
}


#' A Database Specific Function for Dropping Tables
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#'
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbClearResult
#' @importFrom glue glue
#'
#' @return A result string. Either "Success" or an error message.
delete_table_snowflake <- function(con, schema, table){

  result <-
    tryCatch({
      res <- DBI::dbSendQuery(
        con,
        glue::glue(
          "DROP TABLE \"{schema}\".\"{table}\""
        )
      )
      DBI::dbClearResult(res)
      result <- "Success"
    }, error = function(error){
      result <- error$message
    })

}


#' A Database Specific Function for Uploading Data Frames
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table_name A string for the new table name.
#' @param data A data frame to be uploaded.
#' @param temporary A logical value. Should the table be temporary?
#'
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbClearResult
#'
#' @return A result string. Either "Success" or an error message.
write_table_snowflake <-
  function(
    con,
    schema,
    table_name,
    data,
    temporary = FALSE
  ){

    DBI::dbSendQuery(
      con,
      glue::glue(
        "USE SCHEMA {schema}"
      )
    ) |>
      DBI::dbClearResult()

    result <-
      tryCatch({
        DBI::dbWriteTable(
          con,
          name = table_name,
          value = data.frame(data),
          overwrite = TRUE,
          temporary = temporary
        )

        result <- "Success"
      }, error = function(error){
        result <- error$message
      })

  }
