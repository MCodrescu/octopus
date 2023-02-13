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
  shiny::tagList(
    shiny::showModal(
      shiny::modalDialog(
        easyClose = TRUE,
        size = "xl",
        shiny::h3(glue("{title}")),
        shiny::p(
          glue::glue("{n_rows} rows")
        ),
        shiny::div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          DT::renderDataTable(
            options = list(dom = "t", paging = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              result
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

#' Table Modal with Download Server
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
