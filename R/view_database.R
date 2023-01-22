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
              class = "navbar-brand mb-1",
              "Octopus"
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
                    "Manage"
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
                ),

                # Selected Schema
                shiny::tags$li(
                  class = "nav-item",
                  id = "selectedSchema",
                  style = "display: none;",
                  shiny::tags$a(
                    class = "nav-link",
                    id = "selectedSchemaContent",
                    role = "button"
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
            shiny::tags$h3(class = "text-center", "Manage Database"),

            # Select schema
            shiny::selectInput("schema", "Schemas", choices = c("Loading..."), width = "100%", ),

            # Select table
            shiny::selectInput("tables", "Tables", choices = c("Loading..."), width = "100%"),
            shiny::div(
              class = "row justify-content-between py-3",

              # View Table
              shiny::div(
                class = "col-6",
                shiny::tags$button(
                  id = "viewTable",
                  class = "btn btn-outline-primary w-100",
                  "View Table"
                )
              ),

              # Delete Table
              shiny::div(
                class = "col-6",
                shiny::tags$button(
                  id = "deleteTable",
                  class = "btn btn-outline-danger w-100",
                  "Drop Table"
                )
              )
            ),

            # File upload to database
            shiny::fileInput(
              "newTableUpload",
              "Upload File",
              accept = c(".csv", ".xlsx"),
              width = "100%"
            ),

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
        shinyjs::hideElement("selectedSchema")
        shinyjs::showElement("viewDiv")
      })

      shinyjs::onclick("queryNav", {
        shinyjs::hideElement("viewDiv")
        shinyjs::showElement("queryDiv")
        shinyjs::showElement("submitQuery")
        shinyjs::showElement("formatQuery")
        shinyjs::showElement("selectedSchema")
        shinyjs::html(
          "selectedSchemaContent",
          glue::glue(
            "Current Schema: {input$schema}"
          )
        )
      })


      #-------------------------------------------------------------------------

      # Set database specific functions
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

      })

      #-------------------------------------------------------------------------

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
          current_tables <- get_tables(con, schemas[1])
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected = current_tables[1],
            server = TRUE
          )

        })
      })


      #-------------------------------------------------------------------------

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
          current_tables <- get_tables(con, schemas[1])
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected = current_tables[1],
            server = TRUE
          )
        })
      })

      #-------------------------------------------------------------------------

      # Allow submitting queries
      shinyjs::onclick("submitQuery", {

        # Get the query
        query <- input$query

        # Get the number of rows
        n_rows <- get_n_rows(
          con,
          schema = input$schema,
          table = input$tables,
          query = query
        )

        # Catch query errors
        if (query == ""){
          shiny::showNotification(
            "Please input a query"
          )
        } else if(n_rows > 50000){
          shiny::showNotification(
            "Your query returned a result too large.
              Please narrow down the result."
          )
        } else {

          # Set search path
          if (driver %in% c("PqConnection", "Vertica Database")){
            DBI::dbSendQuery(
              con,
              glue::glue(
                "SET search_path TO public, {input$schema}"
              )
            )
          } else if (driver == "Snowflake"){
            DBI::dbSendQuery(
              con,
              glue::glue(
                "USE SCHEMA {input$schema}"
              )
            )
          }

          # Check if it is a select statement
          if (grepl("SELECT|Select|select", query) & !grepl("CREATE", query)) {
            result <- tryCatch(
              {

                # Get the result
                DBI::dbGetQuery(con, query)

              },
              error = function(error) {
                result <- data.frame(result = error$message)
              }
            )

          } else {
            result <- tryCatch(
              {

                # Send the query
                DBI::dbSendQuery(con, query)
                result <- data.frame(result = "Success")

              },
              error = function(error) {
                result <- data.frame(result = error$message)
              }
            )
          }

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

          # Download the query result
          shinyjs::onclick("downloadQuery", {
            result <- tryCatch(
              {

                # Set search path
                if (driver %in% c("PqConnection", "Vertica Database")){
                  DBI::dbSendQuery(
                    con,
                    glue::glue(
                      "SET search_path TO public, {input$schema}"
                    )
                  )
                } else if (driver == "Snowflake"){
                  DBI::dbSendQuery(
                    con,
                    glue::glue(
                      "USE SCHEMA {input$schema}"
                    )
                  )
                }

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

        }

      })

      #-------------------------------------------------------------------------

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


      # Disconnect from DB
      session$onSessionEnded(function() {
        shiny::stopApp()
      })

    }

    shiny::shinyApp(ui, server, options = options)
  }
