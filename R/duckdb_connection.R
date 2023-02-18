#' A Database Specific Function To Retrieve Database Schemas
#' @noRd
#'
#' @param con A database connection object.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr pull
#'
#' @return A character vector of all schemas in the database.
get_schemas_duckdb <- function(con) {

  DBI::dbGetQuery(
    con,
    "
    SELECT DISTINCT schema_name
    FROM information_schema.schemata
    "
  ) |>
    dplyr::pull(1)
}

#' A Database Specific Function to Retrieve All Database Tables
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A
#'
#' @importFrom DBI dbListTables
#'
#' @return A character vector of all tables in a given schema.
get_tables_duckdb <- function(con, schema) {

  DBI::dbGetQuery(
    con,
    glue::glue(
      "
      SELECT table_name
      FROM information_schema.tables
      WHERE table_schema = '{schema}'
      "
    )
  ) |>
    dplyr::pull(1)

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
get_n_rows_duckdb <- function(con, schema, table, query = "") {

  if (query != ""){
    query_string <-
      glue::glue(
        "
        SELECT COUNT(*) AS count
        FROM (
          {query}
        ) AS subquery
        "
      )
  } else {
    query_string <-
      glue::glue(
        "
        SELECT COUNT(*) AS count
        FROM (
            SELECT *
            FROM \"{schema}\".\"{table}\"
        ) AS subquery;
        "
      )
  }

  DBI::dbGetQuery(
    con,
    query_string
  ) |>
    dplyr::pull(1)


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
get_preview_duckdb <- function(con, schema, table) {
  DBI::dbGetQuery(
    con,
    glue::glue(
      "
      SELECT *
      FROM \"{schema}\".\"{table}\"
      LIMIT 100;
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
delete_table_duckdb <- function(con, schema, table){

  DBI::dbSendQuery(
    con,
    glue::glue(
      "DROP TABLE \"{schema}\".\"{table}\""
    )
  ) |>
    DBI::dbClearResult()

  "Success"

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
write_table_duckdb <-
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
        "SET SEARCH_PATH TO '{schema}'"
      )
    ) |>
      DBI::dbClearResult()

    DBI::dbWriteTable(
      con,
      name = table_name,
      value = data.frame(data),
      overwrite = TRUE,
      temporary = temporary
    )

    "Success"

  }
