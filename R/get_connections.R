#' Get existing saved database connections.
#'
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#'
#' @return A named character vector of database connection names and ids.
#'
#' @export
#'
get_connections <- function() {
  dbc_path <-
    glue::glue(
      "{Sys.getenv(\"USERPROFILE\")}\\AppData\\Local\\Programs\\Database_App\\"
    )


  if (file.exists(glue::glue("{dbc_path}\\database_connections.csv"))) {
    connections_df <-
      readr::read_csv(
        glue::glue("{dbc_path}\\database_connections.csv"),
        show_col_types = FALSE
      )

    connections <- connections_df$connection_id
    names(connections) <- connections_df$connection_name

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

  }

  return(connections)
}
