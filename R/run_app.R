#' Run Octopus App
#' @description This function runs the app.
#'
#' @param con A database connection object. The result of DBI::dbConnect().
#' @param options A named list of options to be passed along to shinyApp().
#'
#' @importFrom shiny shinyApp
#' @importFrom shiny showNotification
#' @importFrom shiny updateSelectInput
#' @importFrom shinyjs onclick
#' @importFrom shinyjs hideElement
#' @importFrom shinyjs showElement
#'
#'
#' @return An RShiny instance.
#' @export
#'
run_app <-
  function(con, options = list()){
    ui <- shiny::bootstrapPage(
      theme = bslib::bs_theme(version = 5),

      # Initiate shinyjs
      shinyjs::useShinyjs(),

      #-------------------------------------------------------------------------

      # Navbar
      shiny::tags$div(
        class = "container-fluid pt-2",
        shiny::tags$nav(
          class = "navbar navbar-expand-sm navbar-light bg-light border rounded shadow-sm",
          shiny::div(
            class = "container-fluid",
            shiny::tags$a(
              class = "navbar-brand",
              "Database App"
            ),
            shiny::tags$button(
              class = "navbar-toggler",
              type = "button",
              "data-bs-toggle" = "collapse",
              "data-bs-target" = "#navbarSupportedContent",
              shiny::tags$span(class = "navbar-toggler-icon")
            ),
            shiny::div(
              class = "collapse navbar-collapse",
              id = "navbarSupportedContent",
              shiny::tags$ul(
                class = "navbar-nav me-auto mb-2 mb-sm-0",
                shiny::tags$li(
                  class = "nav-item",
                  shiny::tags$a(
                    class = "nav-link",
                    id = "viewNav",
                    role = "button",
                    "data-bs-toggle" = "dropdown",
                    "View"
                  ),
                ),
                shiny::tags$li(
                  class = "nav-item",
                  shiny::tags$a(
                    class = "nav-link",
                    id = "queryNav",
                    role = "button",
                    "data-bs-toggle" = "dropdown",
                    "Query"
                  )
                ),


                # Run Query Button
                shiny::tags$li(
                  class = "nav-item",
                  id = "submitQuery",
                  style = "display: none;",
                  title = "Run Query",
                  shiny::tags$a(
                    class = "nav-link",
                    role = "button",
                    shiny::icon(
                      name = "play"
                    )
                  )
                ),
                # Format Query Button
                shiny::tags$li(
                  class = "nav-item",
                  id = "formatQuery",
                  style = "display: none;",
                  title = "Format Query",
                  shiny::tags$a(
                    class = "nav-link",
                    role = "button",
                    shiny::icon(
                      name = "indent"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      #-------------------------------------------------------------------------

      # Main container
      shiny::div(
        class = "container py-3",
        shiny::div(
          class = "row justify-content-center",
          shiny::div(
            class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 border rounded shadow",
            id = "viewDiv",

            # Header
            shiny::tags$h3(class = "text-center", "View Tables"),

            # Select schema
            shiny::selectInput("schema", "Schema", choices = c("Loading..."), width = "100%", ),

            # Select table
            shiny::selectInput("tables", "Table", choices = c("Loading..."), width = "100%"),
            shiny::div(
              class = "row justify-content-between py-3",

              # View Table
              shiny::div(
                class = "col-6",
                shiny::tags$button(
                  id = "viewTable",
                  class = "btn btn-outline-primary w-100",
                  "View"
                )
              ),

              # Delete Table
              shiny::div(
                class = "col-6",
                shiny::tags$button(
                  id = "deleteTable",
                  class = "btn btn-outline-danger w-100",
                  "Delete"
                )
              )
            ),

            # File upload to database
            shiny::fileInput("newTableUpload", "Upload CSV", accept = ".csv", width = "100%"),
          ),
          shiny::div(
            class = "col-12 col-lg-10 bg-light pb-2 pt-1 border rounded shadow",
            id = "queryDiv",
            style = "display: none; height: 80vh;",

            shiny::div(
              class = "row h-100 pt-1",
              shiny::div(
                class = "col",
                # Send query to database
                shinyAce::aceEditor(
                  "query",
                  mode = "pgsql",
                  height = "100%",
                  value = "",
                  showPrintMargin = FALSE,
                  fontSize = 16,
                  highlightActiveLine = FALSE
                ),
              )
            )
          ),
        )
      )
    )

    server <- function(input, output, session) {

      # Navigation
      shinyjs::onclick("viewNav", {
        shinyjs::hideElement("queryDiv")
        shinyjs::hideElement("submitQuery")
        shinyjs::hideElement("formatQuery")
        shinyjs::showElement("viewDiv")
      })

      shinyjs::onclick("queryNav", {
        shinyjs::hideElement("viewDiv")
        shinyjs::showElement("queryDiv")
        shinyjs::showElement("submitQuery")
        shinyjs::showElement("formatQuery")
      })


      #-------------------------------------------------------------------------

      # Set database specific functions
      driver <- class(con)

      if (driver == "PqConnection"){
        schemas <- get_schemas_postgres(con)
        get_tables <- get_tables_postgres
        get_n_rows <- get_n_rows_postgres
        get_preview <- get_preview_postgres

      } else if (driver == "Snowflake"){
        # TODO

      } else if (driver == "Vertica Database"){
        # TODO

      }

        # Update schema list
        shiny::updateSelectizeInput(
          session,
          "schema",
          choices = schemas,
          selected = schemas[1],
          server = TRUE
        )

        # Set initial tables
        current_tables <- get_tables(con, schemas[1])
        shiny::updateSelectizeInput(
          session,
          "tables",
          choices = current_tables,
          selected = current_tables[1],
          server = TRUE
        )

        # Update table select on schema change
        shinyjs::onevent("change", "schema", {
          current_tables <- get_tables(con, input$schema)
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected = current_tables[1],
            server = TRUE
          )
        })


      #-------------------------------------------------------------------------

      # View tables on click view button
      shinyjs::onclick("viewTable", {


        # Get the number of rows
        n_rows <- tryCatch({
          get_n_rows(
            con,
            input$schema,
            input$tables
          )
        }, error = function(error){
          shiny::showNotification(error$message)
        })


        # Show the modal
        shiny::showModal(
          shiny::modalDialog(
            easyClose = TRUE,
            size = "xl",
            shiny::h3(
              glue::glue("Preview {input$tables} in {input$schema}")
            ),
            shiny::p(
              glue::glue("{n_rows} rows")
            ),
            shiny::div(
              class = "table-responsive",
              style = "max-height: 70vh;",
              DT::renderDataTable(
                options = list(dom = "t", paging = FALSE, ordering = FALSE),
                server = TRUE,
                rownames = FALSE,
                {
                  result <- tryCatch({
                    get_preview(con, input$schema, input$tables)
                  }, error = function(error){
                    data.frame(
                      error = error$message
                    )
                  })
                  result
                }
              )
            ),
            footer = shiny::tagList(
              shiny::tags$button(
                class = "btn btn-outline-secondary",
                id = "downloadPreview",
                style = "display: none;",
                "Download"
              ),
              shiny::modalButton("Dismiss")
            )
          )
        )

        # Don't allow downloading if query result too big
        if (n_rows < 50000 & n_rows != 0) {
          shinyjs::showElement("downloadPreview")
        } else {
          shinyjs::hideElement("downloadPreview")
        }

        # Download the query result
        shinyjs::onclick("downloadPreview", {
          result <- tryCatch(
            {

              # TODO Edit this function
              # Get query and write to csv
              readr::write_csv(
                DBI::dbGetQuery(
                  con,
                  glue::glue(
                    "SELECT * FROM {table_sql}"
                  )
                ),
                glue::glue(
                  "{Sys.getenv(\"USERPROFILE\")}\\Downloads\\query_result_{format(Sys.time(), \"%Y-%m-%d-%H%M%S\")}.csv"
                )
              )
              result <-
                glue::glue(
                  "Downloaded Successfully to {Sys.getenv(\"USERPROFILE\")}\\Downloads"
                )
            },
            error = function(error) {
              result <- error$message
            }
          )
          shiny::showNotification(result)
        })
      })

      # Disconnect from DB
      session$onSessionEnded(function() {
        try(DBI::dbDisconnect(con))
        shiny::stopApp()
      })

    }

    shiny::shinyApp(ui, server, options = options)
  }
