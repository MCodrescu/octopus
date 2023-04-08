#' Get Database Driver Specific Functions
#' @noRd
#'
#' @param driver A string database connection driver name.
#'
#' @return A list of database specific functions.
get_database_functions <-
  function(
    driver
  ){
    if (driver == "PqConnection"){
      return (
        list(
          get_schemas_postgres,
          get_tables_postgres,
          get_n_rows_postgres,
          get_preview_postgres,
          delete_table_postgres,
          write_table_postgres
        )
      )

    } else if (driver == "Snowflake"){
      return (
        list(
          get_schemas_snowflake,
          get_tables_snowflake,
          get_n_rows_snowflake,
          get_preview_snowflake,
          delete_table_snowflake,
          write_table_snowflake
        )
      )

    } else if (driver == "Vertica Database"){
      return (
        list(
          get_schemas_vertica,
          get_tables_vertica,
          get_n_rows_vertica,
          get_preview_vertica,
          delete_table_vertica,
          write_table_vertica
        )
      )

    } else if (driver == "duckdb_connection"){
      return (
        list(
          get_schemas_duckdb,
          get_tables_duckdb,
          get_n_rows_duckdb,
          get_preview_duckdb,
          delete_table_duckdb,
          write_table_duckdb
        )
      )

    } else if (driver == "MySQLConnection"){
      return (
        list(
          get_schemas_mysql,
          get_tables_mysql,
          get_n_rows_mysql,
          get_preview_mysql,
          delete_table_mysql,
          write_table_mysql
        )
      )

    } else if (driver == "SQLiteConnection"){
      return (
        list(
          get_schemas_sqlite,
          get_tables_sqlite,
          get_n_rows_sqlite,
          get_preview_sqlite,
          delete_table_sqlite,
          write_table_sqlite
        )
      )

    } else if (driver == "Microsoft SQL Server"){
      return (
        list(
          get_schemas_mssql,
          get_tables_mssql,
          get_n_rows_mssql,
          get_preview_mssql,
          delete_table_mssql,
          write_table_mssql
        )
      )

    } else if (driver == "FaultyConnectionExample"){
      return (
        list(
          \(con) stop("This is a get_schemas error"),
          \(con, schema) stop("This is a get_tables error"),
          \(con, schema, table, query) stop("This is a get_n_rows error"),
          \(con, schema, table) stop("This is a get_preview error"),
          \(con, schema, table) stop("This is a delete_table error"),
          \(con, schema, table_name, data, temporary) stop("This is a write_table error")
        )
      )

    } else {
      stop(
        "Database driver not found.
        Please use octopus::list_drivers() to see a list of all available drivers.
        "
      )
    }
  }
