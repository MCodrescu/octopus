#' Retrieve DB credentials
#' @description This function saves DB credentials on the local computer.
#'
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#'
#' @return A character vector of connection names.
#'
retrieve_db_credentials <-
  function(){
    # Create a place to store DB credentials
    dbc_path <-
      glue::glue(
        "{Sys.getenv(\"USERPROFILE\")}\\AppData\\Local\\Programs\\Octopus\\"
      )

    options("dbc_path" = dbc_path)

    if (file.exists(glue::glue("{dbc_path}\\database_connections.csv"))) {
      connections_df <-
        readr::read_csv(
          glue::glue("{dbc_path}\\database_connections.csv"),
          show_col_types = FALSE
        )
      connections <- connections_df$connection_id
      names(connections) <- connections_df$connection_name

      return (connections)

    } else {
      dir.create(dbc_path)
      file.create(
        glue::glue(
          "{dbc_path}\\database_connections.csv"
        )
      )
      connections_df <- data.frame(
        connection_id = integer(),
        connection_name = character()
      )
      readr::write_csv(
        connections_df,
        glue::glue(
          "{dbc_path}\\database_connections.csv"
        )
      )
      connections <- connections_df$connection_id
      names(connections) <- connections_df$connection_name

      return (connections)
    }
  }
