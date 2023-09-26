#' View Database Connection with Octopus
#' @description This function opens a shiny instance where the database can
#'   be viewed.
#'
#' @param con A database connection object. The result of DBI::dbConnect().
#' @param options A named list of options to be passed along to shinyApp().
#' @param max_file_upload_size An integer. The max number of bits allowed in file uploads.
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
#' @importFrom utils write.csv
#'
#'
#' @return An R Shiny instance.
#' @export
#'
view_database <-
  function(con, options = list(), max_file_upload_size = 2000 * 1024^2){
    ui <- shiny::bootstrapPage(
      theme = bslib::bs_theme(
        version = 5
      ),

      # Initiate shinyjs
      shinyjs::useShinyjs(),


      # User Interface----------------------------------------------------------

      shiny::div(
        class = "container pt-3",

        shiny::div(
          class = "row mx-0 px-0 justify-content-center",
          shiny::div(
            class = "col-12 col-md-5 col-lg-4 col-xl-3 mx-3",
            id = "viewDiv",

            ## Manage UI -------------------------------------------------------

            shiny::div(
              class = "row bg-light py-3 mb-3 px-3 border rounded shadow",

              # Header
              shiny::tags$h3(
                class = "text-center",
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
                class = "btn-group mt-2 mb-4 w-100",

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

            ),

            ## Query UI --------------------------------------------------------

            shiny::div(
              class = "d-none d-md-block row bg-light py-3 mt-3 px-3 border rounded shadow",

              # Header
              shiny::tags$h3(
                class = "text-center",
                "Query"
              ),

              shiny::hr(),

              shiny::div(
                class = "btn-group mt-3 mb-3 w-100",

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
            class = "d-none d-md-block col-12 col-md-6 col-lg-7 bg-light pb-2 pt-1 mt-3 mt-md-0 border rounded shadow",
            id = "queryDiv",

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
                  highlightActiveLine = FALSE,
                  autoComplete = "enabled",
                  autoCompleters = c("static"),
                  hotkeys = list(
                    run_key = list(
                      win = "Ctrl-Shift-Enter",
                      mac = "CMD-SHIFT-ENTER"
                    ),
                    format_key = list(
                      win = "Ctrl-Shift-F",
                      mac = "CMD-SHIFT-F"
                    )
                  ),
                  debounce = 1
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

      tryCatch({
        database_functions <- get_database_functions(driver)
        get_schemas <- database_functions[[1]]
        get_tables <- database_functions[[2]]
        get_n_rows <- database_functions[[3]]
        get_preview <- database_functions[[4]]
        delete_table <- database_functions[[5]]
        write_table <- database_functions[[6]]

      }, error = function(error){
        shiny::showNotification(error$message)
      })


      # Initialize Inputs -----------------------------------------------

      tryCatch({
        schemas <- get_schemas(con)

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
          shiny::showNotification(
            "Loading...",
            duration = NULL,
            id = "loading-notification"
          )

          current_tables <- get_tables(con, input$schema)
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected = current_tables[1],
            server = TRUE
          )

          shiny::removeNotification(
            "loading-notification"
          )
        })

        # Update column name suggestions
        shinyjs::onevent("change", "tables",{

          if (input$tables %in% current_tables){

            # Set Ace Editors Auto Complete Suggestions
            current_schema <- input$schema
            current_table <- input$tables
            current_table_preview <- get_preview(
              con,
              current_schema,
              current_table
            )
            auto_complete_suggestions <- list()
            auto_complete_suggestions[["Schema"]] <- schemas
            auto_complete_suggestions[[current_schema]] <- current_tables
            auto_complete_suggestions[[current_table]] <- colnames(current_table_preview)


            shinyAce::updateAceEditor(
              session = session,
              editorId = "query",
              autoCompleteList = auto_complete_suggestions
            )
          }

        })

      }, error = function(error){
        shiny::showNotification(error$message)
      })


      # View Table ------------------------------------------------------------

      # View tables on click view button
      shinyjs::onclick("viewTable", {

        shiny::showNotification(
          "Loading Table...",
          duration = NULL,
          id = "loading-notification"
        )

        tryCatch({

          # Get the number of rows
          n_rows <- get_n_rows(
            con,
            input$schema,
            input$tables
          )

          result <- get_preview(
            con,
            input$schema,
            input$tables
          )

          table_modal_w_download_full_Server(
            id = "preview",
            con = con,
            schema = input$schema,
            table = input$tables,
            n_rows = n_rows
          )

          table_modal_w_download_UI(
            id = "preview",
            title = glue::glue("Preview Table: {input$tables}"),
            download_title = "Download Table",
            n_rows = n_rows,
            result = result
          )

        }, error = function(error){
          shiny::showNotification(error$message)
        })

        shiny::removeNotification(
          "loading-notification"
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

          tryCatch({

            shiny::showNotification(
              "Loading...",
              duration = NULL,
              id = "loading-notification"
            )

            result <- delete_table(
              con,
              input$schema,
              input$tables
            )

            shiny::removeNotification(
              "loading-notification"
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

          }, error = function(error){
            shiny::showNotification(error$message)
          })

        })
      })


      # Upload Table -----------------------------------------------------------

      # Increase file upload limit
      options(shiny.maxRequestSize = max_file_upload_size)

      # Upload file to DB
      shiny::observeEvent(input$newTableUpload, {

        shiny::showNotification(
          "Reading File...",
          duration = NULL,
          id = "loading-notification"
        )

        # Read file
        file <- input$newTableUpload
        shiny::req(file)
        new_table <- rio::import(
          file$datapath
        )

        shiny::removeNotification(
          "loading-notification"
        )

        shiny::showModal(
          shiny::modalDialog(
            easyClose = TRUE,
            size = "s",
            shiny::tagList(
              shiny::textInput(
                "newTableName",
                "Confirm Table Name",
                value = gsub("\\..+", "", file$name)
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
            if (driver == "Snowflake"){
              colnames(new_table) <- toupper(colnames(new_table))
            }

          }

          tryCatch({
            shiny::showNotification(
              "Uploading...",
              duration = NULL,
              id = "loading-notification"
            )

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

            shiny::removeNotification(
              "loading-notification"
            )

            # Show result
            shiny::showNotification(result, duration = 3)

            # Update select input
            current_tables <- get_tables(con, input$schema)
            shiny::updateSelectizeInput(
              session,
              "tables",
              choices = current_tables,
              selected = input$newTableName,
              server = TRUE
            )

          }, error = function(error){
            shiny::showNotification(error$message)
          })

        })
      })

      # Query ----------------------------------------------------------------

      query_submit <- function(){
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

          table_modal_w_download_Server(
            id = "query",
            result = result
          )

          table_modal_w_download_UI(
            id = "query",
            title = "Query Preview",
            download_title = "Download",
            n_rows = n_rows,
            result = result
          )

          # Update select input
          previous_table <- input$tables
          current_tables <- get_tables(con, input$schema)
          shiny::updateSelectizeInput(
            session,
            "tables",
            choices = current_tables,
            selected =  ifelse(
              previous_table %in% current_tables,
              previous_table,
              current_tables[1]
            ),
            server = TRUE
          )

        }, error = function(error){
          shiny::showNotification(
            error$message
          )
        })
      }

      # Hotkey Query Submit
      observeEvent(input$query_run_key, {
        shiny::showNotification(
          "Loading...",
          duration = NULL,
          id = "loading-notification"
        )
        query_submit()
        shiny::removeNotification(
          "loading-notification"
        )
      })

      # Run Button Query Submit
      shinyjs::onclick("submitQuery", {
        shiny::showNotification(
          "Loading...",
          duration = NULL,
          id = "loading-notification"
        )
        query_submit()
        shiny::removeNotification(
          "loading-notification"
        )
      })

      # Format -----------------------------------------------------------------

      # Reformat SQL code
      format_code <- function(){
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
      }

      # Hotkey to Format
      observeEvent(input$query_format_key, format_code())

      # Format Button
      shinyjs::onclick("formatQuery", format_code())

    }

    shiny::shinyApp(ui, server, options = options)
  }
