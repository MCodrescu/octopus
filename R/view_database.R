#' View Database Connection with Octopus
#' @description This function opens a shiny instance where the database can
#'   be viewed.
#'
#' @param con A database connection object. The result of DBI::dbConnect().
#' @param options A named list of options to be passed along to shinyApp().
#'
#' @importFrom shiny shinyApp
#' @importFrom shiny showNotification
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSelectizeInput
#' @importFrom shiny tags
#' @importFrom shiny div
#' @importFrom shiny p
#' @importFrom shiny h3
#' @importFrom shiny selectInput
#' @importFrom shiny fileInput
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny tagList
#' @importFrom shiny modalButton
#' @importFrom shiny removeModal
#' @importFrom shiny observeEvent
#' @importFrom shiny stopApp
#' @importFrom shiny bootstrapPage
#' @importFrom shiny req
#' @importFrom shinyAce aceEditor
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs onclick
#' @importFrom shinyjs hideElement
#' @importFrom shinyjs showElement
#' @importFrom shinyjs onevent
#' @importFrom shinyjs html
#' @importFrom glue glue
#' @importFrom DT renderDataTable
#' @importFrom rio import
#' @importFrom janitor clean_names
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbGetQuery
#' @importFrom httr GET
#' @importFrom httr use_proxy
#' @importFrom httr content
#' @importFrom bslib bs_theme
#'
#'
#' @return An R Shiny instance.
#' @export
#'
view_database <-
  function(con, options = list()){
    ui <- shiny::bootstrapPage(
      theme = bslib::bs_theme(
        version = 5,
        base_font = bslib::font_google("Prompt")
      ),

      # Initiate shinyjs
      shinyjs::useShinyjs(),


      # User Interface----------------------------------------------------------

      shiny::div(
        class = "container py-5",

        shiny::div(
          class = "row mx-0 px-0 justify-content-center",
          shiny::div(
            class = "col-10 col-md-9 col-lg-3 me-3",
            id = "viewDiv",

            ## Manage UI -------------------------------------------------------

            shiny::div(
              class = "row bg-light py-3 mb-3 px-3 border rounded shadow",

              # Header
              shiny::tags$h3(
                class = "text-center pb-3",
                "Manage"
              ),

              shiny::hr(),

              # Select schema
              shiny::selectInput(
                "schema",
                label = shiny::strong("Schema"),
                choices = c("Loading..."),
                width = "100%"
              ),

              # Select table
              shiny::selectInput(
                "tables",
                label = shiny::strong("Table"),
                choices = c("Loading..."),
                width = "100%"
              ),

              shiny::div(
                class = "btn-group mt-2 mb-5 w-100",

                # View Tables
                shiny::actionButton(
                  inputId = "viewTable",
                  icon = shiny::icon("expand"),
                  "View"
                ),

                # Delete Tables
                shiny::actionButton(
                  inputId = "deleteTable",
                  icon = shiny::icon("trash"),
                  "Delete"
                )
              ),

              # File upload to database
              shiny::fileInput(
                "newTableUpload",
                label = shiny::strong("Upload File"),
                accept = c(".csv", ".xlsx"),
                width = "100%"
              ),

              shiny::div(class = "pt-4"),

            ),

            ## Query UI --------------------------------------------------------

            shiny::div(
              class = "row bg-light py-3 mt-3 px-3 border rounded shadow",

              # Header
              shiny::tags$h3(
                class = "text-center pb-3",
                "Query"
              ),

              shiny::hr(),

              shiny::div(
                class = "btn-group mt-3 mb-4 w-100",

                # Run Query
                shiny::actionButton(
                  inputId = "submitQuery",
                  icon = shiny::icon("play"),
                  "Run"
                ),

                # Format Query
                shiny::actionButton(
                  inputId = "formatQuery",
                  icon = shiny::icon("indent"),
                  "Format"
                )
              )
            ),

          ),
          shiny::div(
            class = "col-12 col-lg-7 bg-light pb-2 pt-1 mt-3 mt-lg-0 border rounded shadow",
            id = "queryDiv",
            style = "height: 80vh;",

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

      # Database Functions -----------------------------------------------------
      driver <- class(con)

      if (driver == "PqConnection"){
        schemas <- get_schemas_postgres(con)
        get_tables <- get_tables_postgres
        get_n_rows <- get_n_rows_postgres
        get_preview <- get_preview_postgres
        delete_table <- delete_table_postgres
        write_table <- write_table_postgres

      } else if (driver == "Snowflake"){
        schemas <- get_schemas_snowflake(con)
        get_tables <- get_tables_snowflake
        get_n_rows <- get_n_rows_snowflake
        get_preview <- get_preview_snowflake
        delete_table <- delete_table_snowflake
        write_table <- write_table_snowflake


      } else if (driver == "Vertica Database"){
        # TODO

      } else if (driver == "Teradata"){
        # TODO

      } else if (driver == "MySQLConnection"){
        schemas <- get_schemas_mysql(con)
        get_tables <- get_tables_mysql
        get_n_rows <- get_n_rows_mysql
        get_preview <- get_preview_mysql
        delete_table <- delete_table_mysql
        write_table <- write_table_mysql

      } else if (driver == "SQLiteConnection"){
        schemas <- get_schemas_sqlite(con)
        get_tables <- get_tables_sqlite
        get_n_rows <- get_n_rows_sqlite
        get_preview <- get_preview_sqlite
        delete_table <- delete_table_sqlite
        write_table <- write_table_sqlite

      }

      # Initialize Inputs -----------------------------------------------

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


      # View Table ------------------------------------------------------------

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

      })

      # Delete Table -----------------------------------------------------------

      # Allow deleting a table
      shinyjs::onclick("deleteTable", {
        table <- input$tables
        shiny::showModal(
          shiny::modalDialog(
            easyClose = TRUE,
            shiny::h3("Confirm Deletion"),
            shiny::p(glue("Are you sure you want to delete the table: {table}?")),
            footer = shiny::tags$button(
              id = "confirmDelete",
              class = "btn btn-outline-danger",
              "Confirm"
            )
          )
        )

        # Confirm delete
        shinyjs::onclick("confirmDelete", asis = TRUE, {
          shiny::removeModal()

          result <- delete_table(
            con,
            input$schema,
            input$tables
          )

          # Notify success
          shiny::showNotification(result)

          # Update select input
          current_tables <- get_tables(con, input$schema)
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected = current_tables[1],
            server = TRUE
          )

        })
      })


      # Upload Table -----------------------------------------------------------

      # Increase file upload limit
      options(shiny.maxRequestSize = 2000 * 1024^2)

      # Upload file to DB
      shiny::observeEvent(input$newTableUpload, {

        # Read file
        file <- input$newTableUpload
        shiny::req(file)
        new_table <- rio::import(
          file$datapath
        )

        shiny::showModal(
          shiny::modalDialog(
            easyClose = TRUE,
            size = "s",
            shiny::tagList(
              shiny::textInput(
                "newTableName",
                "Confirm Table Name",
                value = gsub(".csv", "", file$name)
              ),
              shiny::selectInput(
                "cleanColumnNames",
                "Clean column names?",
                choices = c("Yes", "No"),
                selected = "Yes"
              ),
              shiny::selectInput(
                "tempTable",
                "Temporary table?",
                choices = c("Yes", "No"),
                selected = "Yes"
              )
            ),
            footer = shiny::tagList(
              shiny::tags$button(
                id = "confirmNewTableName",
                class = "btn btn-outline-primary",
                "data-bs-dismiss" = "modal",
                "Confirm"
              )
            )
          )
        )

        shinyjs::onclick("confirmNewTableName", {
          if (input$cleanColumnNames == "Yes") {
            new_table <-
              janitor::clean_names(new_table)
          }

          # Write data frame to DB
          result <- write_table(
            con,
            schema = input$schema,
            table_name = input$newTableName,
            data = new_table,
            temporary = ifelse(
              input$tempTable == "Yes",
              TRUE,
              FALSE
            )
          )

          # Show result
          shiny::showNotification(result, duration = 3)

          # Update select input
          current_tables <- get_tables(con, input$schema)
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected = current_tables[1],
            server = TRUE
          )
        })
      })

      # Query ----------------------------------------------------------------

      # Allow submitting queries
      shinyjs::onclick("submitQuery", {

          tryCatch({

            n_rows <- get_n_rows(
              con = con,
              schema = input$schema,
              table = input$tables,
              query = input$query
            )

            result <- submit_query(
              query = input$query,
              con = con,
              n_rows = n_rows
            )

            # Show query result
            shiny::showModal(
              shiny::modalDialog(
                easyClose = TRUE,
                size = "xl",
                shiny::h3("Query Preview"),
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
                  shiny::tags$button(
                    class = "btn btn-outline-secondary",
                    id = "downloadQuery",
                    style = "display: none;",
                    "Download"
                  ),
                  shiny::modalButton("Dismiss")
                )
              )
            )

            # Don't allow downloading if query result too big
            if (n_rows < 50000 & n_rows != 0) {
              shinyjs::showElement("downloadQuery")
            } else {
              shinyjs::hideElement("downloadQuery")
            }

            # TODO: Change download to use R Shiny mechanism instead
            # Download the query result
            shinyjs::onclick("downloadQuery", {
              result <- tryCatch(
                {
                  # Get query and write to csv
                  readr::write_csv(
                    DBI::dbGetQuery(con, input$query),
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

            # Update select input
            current_tables <- get_tables(con, input$schema)
            shiny::updateSelectizeInput(
              session,
              "tables",
              choices = current_tables,
              selected = current_tables[1],
              server = TRUE
            )

          }, error = function(error){
            shiny::showNotification(
              error$message
            )
          })

      })

      # Format -----------------------------------------------------------------

      # Reformat SQL code
      shinyjs::onclick("formatQuery", {
        original_query <- input$query
        tryCatch(
          {
            response <-
              httr::GET(
                glue::glue(
                  "https://sqlformat.org/api/v1/format",
                  "?reindent=1",
                  "&keyword_case=upper",
                  "&sql={URLencode(original_query)}"
                ),
                httr::use_proxy(
                  Sys.getenv("https_proxy")
                )
              )
            shinyAce::updateAceEditor(
              session = session,
              editorId = "query",
              value = httr::content(response, as = "parsed")$result
            )
          },
          error = function(error) {
            shiny::showNotification(error$message)
          }
        )
      })

    }

    shiny::shinyApp(ui, server, options = options)
  }
