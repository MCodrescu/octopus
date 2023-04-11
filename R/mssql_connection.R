#' A Database Specific Function To Retrieve Database Schemas
#' @noRd
#'
#' @param con A database connection object.
#'
#' @importFrom DBI dbGetQuery
#'
#' @return A character vector of all schemas in the database.
get_schemas_mssql <- function(con) {
  schemas <-
    DBI::dbGetQuery(
      con,
      "
      SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA
      "
    )[[1]]
}

#' A Database Specific Function to Retrieve All Database Tables
#' @noRd
#'
#' @param con A database connection object.
#' @param schema A
#'
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#'
#' @return A character vector of all tables in a given schema.
get_tables_mssql <- function(con, schema) {
  result <- tryCatch({
    DBI::dbGetQuery(
      con,
      glue::glue(
        "
      SELECT TABLE_NAME
      FROM [{schema}].INFORMATION_SCHEMA.TABLES
      WHERE TABLE_TYPE = 'BASE TABLE'
      "
      )
    )[[1]]

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
#'
#' @return An integer for the number of rows in the table.
get_n_rows_mssql <- function(con, schema, table, query = "") {

  if (query != ""){
    if (!grepl("^SELECT", trimws(query), ignore.case = TRUE)){
      return(0)
    }
    if (grepl("^SELECT .+ INTO", trimws(query), ignore.case = TRUE)){
      return (0)
    }
    query_string <-
      glue::glue(
        "
        SELECT COUNT(*) AS count
        FROM (
          {query}
        ) AS subquery;
        "
      )
  } else {
    query_string <-
      glue::glue(
        "
        SELECT COUNT(*) AS count
        FROM (
            SELECT *
            FROM [{table}]
        ) AS subquery;
        "
      )
  }

  result <-
    tryCatch({
      res <- DBI::dbSendQuery(
        con,
        glue::glue(
          "USE {schema}"
        )
      )

      DBI::dbClearResult(res)

      result <-
        DBI::dbGetQuery(
          con,
          query_string
        )[[1]]

    }, error = function(error){
      error$message
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
get_preview_mssql <- function(con, schema, table) {
  res <- DBI::dbSendQuery(
    con,
    glue::glue(
      "USE {schema}"
    )
  )

  DBI::dbClearResult(res)

  DBI::dbGetQuery(
    con,
    glue::glue(
      "
      SELECT TOP 100 *
      FROM [{table}];
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
#' @importFrom glue glue
#'
#' @return A result string. Either "Success" or an error message.
delete_table_mssql <- function(con, schema, table){

  res <- DBI::dbSendQuery(
    con,
    glue::glue(
      "USE {schema}"
    )
  )

  DBI::dbClearResult(res)

  result <-
    tryCatch({
      res <- DBI::dbSendQuery(
        con,
        glue::glue(
          "DROP TABLE [{table}]"
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
#' @importFrom DBI Id
#'
#' @return A result string. Either "Success" or an error message.
write_table_mssql <-
  function(
    con,
    schema,
    table_name,
    data,
    temporary = FALSE
  ){

    res <- DBI::dbSendQuery(
      con,
      glue::glue(
        "USE {schema}"
      )
    )

    DBI::dbClearResult(res)

    result <-
      tryCatch({
        DBI::dbWriteTable(
          con,
          name = table_name,
          value = data.frame(data),
          overwrite = TRUE,
          temporary = temporary,
          row.names = FALSE
        )

        result <- "Success"
      }, error = function(error){
        result <- error$message
      })

  }
