#' List Compatible Database Drivers
#'
#' @return A character vector of compatible database drivers.
#' @export
list_drivers <-
  function(){
    c(
      "PqConnection",
      "Snowflake",
      "Vertica Database",
      "duckdb_connection",
      "MySQLConnection",
      "SQLiteConnection",
      "Microsoft SQL Server"
    )
  }
