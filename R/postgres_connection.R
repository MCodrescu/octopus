#' Create A Postgres Database Connection
#'
#' @param database_credentials A data frame with connection specific credentials.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom shiny ShowNotification
#'
#' @return A database connection object.
#' @export
#'
#' @examples
establish_connection_postgres <- function(database_credentials) {
  con <- NA
  message <- ""

  if (require("RPostgres")) {
    tryCatch({
      con <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = database_credentials$host,
        user = database_credentials$user,
        password = database_credentials$password,
        port = database_credentials$port,
        dbname = database_credentials$dbname
      )
      message <- "Success"
    }, error = function(error){
      message <- error$message
    })

  } else {
    message <- "Please install RPostgres package."
  }

  list(con, message)
}

#' A Database Specific Function To Retrieve Database Schemas
#'
#' @param con A database connection object.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr pull
#'
#' @return A character vector of all schemas in the database.
#' @export
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
#' @export
#'
#' @examples
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
#' @export
#'
#' @examples
get_n_rows_postgres <- function(con, schema, table) {
  DBI::dbGetQuery(
    con,
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
  ) |>
    dplyr::pull(
      count
    )
}

#' A Database Specific Function for Retrieving Table Previews
#'
#' @param con A database connection object.
#' @param schema A string containing the schema name.
#' @param table A string containing the table name.
#'
#' @return A data frame of 100 rows from the database table.
#' @export
#'
#' @examples
get_preview_postgres <- function(con, shema, table) {
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
