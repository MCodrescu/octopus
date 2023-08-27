#' Table Modal with Download UI
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom shiny tagList
#' @importFrom shiny modalDialog
#' @importFrom shiny h3
#' @importFrom shiny p
#' @importFrom shiny div
#' @importFrom shiny downloadButton
#' @importFrom shiny modalButton
#' @importFrom glue glue
#' @importFrom DT renderDataTable
#'
#' @param id The namespace Id
#' @param title The title to be displayed in the modal.
#' @param download_title The title on the download button.
#' @param n_rows The number of rows of the result.
#' @param result The data frame to display in the modal.
#'
#' @return A shiny tagList
table_modal_w_download_UI <- function(id, title, download_title, n_rows, result) {
  ns <- shiny::NS(id)
  options(scipen = 99999)
  shiny::tagList(
    shiny::showModal(
      shiny::modalDialog(
        easyClose = TRUE,
        size = "xl",
        shiny::h3(glue("{title}")),
        shiny::p(
          glue::glue("{format(n_rows, big.mark = \",\")} rows")
        ),
        shiny::div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          DT::renderDataTable(
            options = list(dom = "t", paging = FALSE, ordering = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              if(nrow(result) > 1000){
                result[1:1000, ]
              } else {
                result
              }
            }
          )
        ),
        footer = shiny::tagList(
          shiny::downloadButton(
            ns("downloadQuery"),
            glue::glue("{download_title}")
          ),
          shiny::modalButton("Dismiss")
        )
      )
    )
  )
}

#' Table Modal with Download Server (Preview)
#' @noRd
#'
#' @importFrom shiny moduleServer
#' @importFrom shiny downloadHandler
#' @importFrom glue glue
#' @importFrom utils write.csv
#'
#' @param id A namespace id.
#' @param result A data frame to open for downloading.
#'
#' @return A model server.
table_modal_w_download_Server <- function(id, result) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$downloadQuery <-
        shiny::downloadHandler(
          filename = function(){
            glue::glue(
              "query_{format(Sys.time(), \"%Y%m%d%H%M%S\")}.csv"
            )
          },
          content = function(file) {
            utils::write.csv(result, file, row.names = FALSE)
          }
        )
    }
  )
}


#' Table Modal with Download Server (Full Table)
#' @noRd
#'
#' @importFrom shiny moduleServer
#' @importFrom shiny downloadHandler
#' @importFrom shiny removeModal
#' @importFrom shiny withProgress
#' @importFrom shiny setProgress
#' @importFrom shiny showNotification
#' @importFrom DBI dbQuoteIdentifier
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbFetch
#' @importFrom DBI dbHasCompleted
#' @importFrom DBI Id
#' @importFrom glue glue
#' @importFrom data.table fwrite
#' @importFrom data.table rbindlist
#'
#' @param id A namespace id.
#' @param con A database connection object.
#' @param schema The schema name.
#' @param table The table name.
#' @param n_rows The number of rows of the table.
#' @param step_size The number of rows to retrieve at each step.
#'
#' @return A model server.
table_modal_w_download_full_Server <- function(id, con, schema, table, n_rows, step_size = floor(n_rows / 100)) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      tryCatch({

        output$downloadQuery <-
          shiny::downloadHandler(
            filename = function(){

              glue::glue(
                "{schema}_{table}.csv"
              )
            },

            content = function(file) {

              shiny::withProgress(
                min = 0,
                max = n_rows,
                value = 0,
                message = "Initializing",
                expr = {
                  query <- paste(
                    "SELECT * FROM",
                    DBI::dbQuoteIdentifier(
                      con,
                      DBI::Id(schema = schema, table = table)
                    )
                  )

                  res <- DBI::dbSendQuery(con, query)
                  table_pieces <- vector("list", length = 101)
                  i <- 1

                  while (!DBI::dbHasCompleted(res)){
                    table_pieces[[i]] <- DBI::dbFetch(res, n = step_size)

                    shiny::setProgress(
                      value = step_size * i,
                      message = paste("Rows Retrieved:", step_size * i),
                      session = session
                    )

                    i <- i + 1
                  }

                  full_table <- data.frame(
                    data.table::rbindlist(table_pieces)
                  )

                }
              )

              data.table::fwrite(full_table, file)
            }
          )

      }, error = function(error){
        shiny::showNotification(error$message)
      })

    }
  )
}
