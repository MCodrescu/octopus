#' Submit Query
#' @noRd
#'
#' @param query A character string of the SQL query.
#' @param con A database connection object
#' @param n_rows The number of rows of the query.
#'
#' @return A data frame of the query result or an error.
#'
submit_query <-
  function(
    query,
    con,
    n_rows
  ){

    if (query == ""){
      stop(
        "Please input a query"
      )
    } else if(n_rows > 50000){
      stop(
        "Your query returned a result too large."
      )
    } else {

      if (
        all(
          grepl("SELECT", query, ignore.case = TRUE),
          !grepl("CREATE", query, ignore.case = TRUE)
        )
      ) {

        result <- DBI::dbGetQuery(con, query)

      } else {

        DBI::dbSendQuery(con, query) |>
          DBI::dbClearResult()

        result <- data.frame(result = "Success")

      }
    }

    return (result)
  }
