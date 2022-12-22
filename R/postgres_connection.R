#' Create A Postgres Database Connection
#'
#' @param database_credentials A data frame with connection specific credentials.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom shiny showNotification
#'
#' @return A database connection object.
#'
establish_connection_postgres <- function(database_credentials) {
  con <- NA
  message <- ""

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


#' New Connection Form - Postgres
#'
#' @return A shiny tag list for inputing new connection details.
new_connection_form_postgres <-
  function(){
    shiny::tagList(
      shiny::div(
        id = "postgresConnectionForm",
        style = "display: none;",
        shiny::textInput("newPostgresConnectionName", "Connection Name", width = "100%"),
        shiny::textInput("newPostgresHost", "Host", width = "100%"),
        shiny::textInput("newPostgresUser", "User", width = "100%"),
        shiny::textInput("newPostgresPassword", "Password", width = "100%"),
        shiny::textInput("newPostgresPort", "Port", value = 5432, width = "100%"),
        shiny::textInput("newPostgresDbname", "Database Name", width = "100%"),
      )
    )

  }

#' Write New Connection - Postgres
#'
#' @param new_name A string connection name.
#' @param new_host A string connection host. Ex: localhost
#' @param new_user A string connection username.
#' @param new_password A string connection password.
#' @param new_port An integer connection port.
#' @param new_dbname A string connection database name.
#'
#' @return A connections dataframe is returned.
#' The connection details are also written to a connection file.
write_new_connection_postgres <-
  function(
    new_name,
    new_host,
    new_user,
    new_password,
    new_port,
    new_dbname){

    dbc_path <- getOption("dbc_path")

    connections_df <-
      readr::read_csv(
        glue::glue("{dbc_path}\\database_connections.csv"),
        show_col_types = FALSE
      )

    connections_df <-
      connections_df |>
      dplyr::bind_rows(
        data.frame(
          connection_id = as.character(nrow(connections_df) + 1),
          connection_name = new_name
        )
      )

    readr::write_csv(
      data.frame(
        driver = "postgres",
        host = new_host,
        user = new_user,
        password = new_password,
        port = new_port,
        dbname = new_dbname
      ),
      glue::glue(
        "{dbc_path}\\connection_{nrow(connections_df)}.csv"
      )
    )

    return (connections_df)

  }
