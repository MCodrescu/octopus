#' Run Octopus App
#' @description This function runs the app.
#'
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
  function(options){
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
                    id = "connectionNav",
                    role = "button",
                    "data-bs-toggle" = "dropdown",
                    "Connection"
                  )
                ),
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
            id = "connectionDiv",



            shiny::div(
              class = "row",
              shiny::div(
                class = "col",
                # Header
                shiny::tags$h3(class = "text-start", "Connect"),
              ),

              shiny::div(
                class = "col",
                # Add new connection button
                shiny::tags$button(
                  class = "btn btn-light mt-3 float-right",
                  id = "addConnectionButton",
                  shiny::icon(
                    "square-plus",
                    class = "fa-thin fa-2x"
                  )
                ),
                # Edit connections button
                shiny::tags$button(
                  class = "btn btn-light mt-3 float-right",
                  id = "manageConnectionsButton",
                  shiny::icon(
                    "gear",
                    class = "fa-thin fa-2x"
                  )
                ),
              ),
            ),

            # Select a connection
            shiny::selectInput("connectionSelect", "", choices = NULL, width = "100%"),

            # Connect button
            shiny::tags$button(
              class = "btn btn-outline-success w-100 mt-2 mb-3",
              id = "connectButton",
              "Connect"
            ),

            # Connection status
            shiny::p(id = "connectionStatus")
          ),
          shiny::div(
            class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 border rounded shadow",
            id = "viewDiv",
            style = "display: none;",

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
      # Set initial connection status
      connectionStatus <- FALSE

      # Retrieve DB credentials
      connections <- retrieve_db_credentials()
      dbc_path <- getOption("dbc_path")

      #-------------------------------------------------------------------------

      # Navigation
      shinyjs::onclick("viewNav", {
        if (connectionStatus) {
          shinyjs::hideElement("connectionDiv")
          shinyjs::hideElement("queryDiv")
          shinyjs::hideElement("submitQuery")
          shinyjs::hideElement("formatQuery")
          shinyjs::showElement("viewDiv")
        } else {
          shiny::showNotification("Please connect to a database first")
        }
      })

      shinyjs::onclick("queryNav", {
        if (connectionStatus) {
          shinyjs::hideElement("connectionDiv")
          shinyjs::hideElement("viewDiv")
          shinyjs::showElement("queryDiv")
          shinyjs::showElement("submitQuery")
          shinyjs::showElement("formatQuery")
        } else {
          shiny::showNotification("Please connect to a database first")
        }
      })

      shinyjs::onclick("connectionNav", {
        shinyjs::hideElement("viewDiv")
        shinyjs::hideElement("queryDiv")
        shinyjs::hideElement("submitQuery")
        shinyjs::hideElement("formatQuery")
        shinyjs::showElement("connectionDiv")
      })

      #-------------------------------------------------------------------------

      # Update the connection select
      shiny::updateSelectInput(
        inputId = "connectionSelect",
        choices = connections
      )

      # Connect to DB
      shinyjs::onclick("connectButton", {
        database_credentials <-
          readr::read_csv(
            glue::glue("{dbc_path}\\connection_{input$connectionSelect}.csv"),
            show_col_types = FALSE
          )

        driver <-
          database_credentials$driver

        result <-
          tryCatch({
            if(driver == "postgres"){
              if(require(RPostgres)){
                connection_result <-
                  establish_connection_postgres(
                    database_credentials
                  )

                con <- connection_result[[1]]
                message <- connection_result[[2]]
                schemas <- get_schemas_postgres(con)
                get_tables <- get_tables_postgres
                get_n_rows <- get_n_rows_postgres
                get_preview <- get_preview_postgres
                reesult <- "Success"

              } else {
                shiny::showNotification("Please install the RPostgres package.")
              }

            } else if (driver == "teradata"){
              if(require(teradatasql)){
                # TODO

              } else {
                shiny::showNotification("Please install the teradatasql package.")
              }

            } else if (driver == "snowflake"){
              if (require(odbc)){
                if ("SnowflakeDSIIDriver" %in% dplyr::pull(odbc::odbcListDrivers(), name)){
                  # TODO

                } else {
                  showNotification("Please install the Snowflake 64 bit driver")
                }

              } else {
                showNotification("Please install odbc package.")
              }

            } else if (driver == "vertica"){

              if (require(odbc)){
                if ("Vertica" %in% dplyr::pull(odbc::odbcListDrivers(), name)){
                  # TODO

                } else {
                  showNotification("Please install the Vertica 64 bit driver")
                }

              } else {
                showNotification("Please install odbc package.")
              }

            }
          }, error = function(error){
              result <- error$message
          })

        #-----------------------------------------------------------------------


        # Notify of connection result
        shiny::showNotification(result)

        if (result == "Success") {
          connectionStatus <- TRUE
          shinyjs::html(
            "connectionStatus",
            glue::glue("Connected to: {input$connectionSelect}")
          )
          shinyjs::hideElement("connectionDiv")
          shinyjs::showElement("viewDiv")


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
        }

      })

      #-------------------------------------------------------------------------

      shinyjs::onclick(
        "addConnectionButton",
        {
          # Show the modal
          shiny::showModal(
            shiny::modalDialog(
              easyClose = TRUE,
              size = "m",
              title = "Add Connection",
              shiny::selectInput(
                "newConnectionType",
                "Select new connection type.",
                choices = c(
                  "postgres",
                  "teradata",
                  "vertica",
                  "snowflake"
                ),
                width = "100%"
              ),
              new_connection_form_postgres(),
              div(
                id = "teradataConnectionForm",
                style = "display: none;",
                textInput("newTeradataConnectionName", "Connection Name", width = "100%"),
                textInput("newTeradataServer", "Server", width = "100%"),
                textInput("newTeradataUsername", "Username", width = "100%"),
                textInput("newTeradataPassword", "Password", width = "100%"),
              ),
              div(
                id = "verticaConnectionForm",
                style = "display: none;",
                textInput("newVerticaConnectionName", "Connection Name", width = "100%"),
                textInput("newVerticaServer", "Server", width = "100%"),
                textInput("newVerticaUsername", "Username", width = "100%"),
                textInput("newVerticaPassword", "Password", width = "100%"),
                textInput("newVerticaPort", "Port", value = 5433, width = "100%"),
                textInput("newVerticaDatabase", "Database Name", width = "100%"),
              ),
              div(
                id = "snowflakeConnectionForm",
                style = "display: none;",
                textInput("newSnowflakeConnectionName", "Connection Name", width = "100%"),
                textInput("newSnowflakeServer", "Server", width = "100%"),
                textInput("newSnowflakeUser", "User", width = "100%"),
                textInput("newSnowflakePassword", "Password", width = "100%"),
                textInput("newSnowflakeDatabase", "Database", width = "100%"),
                textInput("newSnowflakeRole", "Role", width = "100%"),
                textInput("newSnowflakeWarehouse", "Warehouse", width = "100%"),
              ),
              footer = shiny::tagList(
                shiny::tags$button(
                  id = "createConnection",
                  class = "btn btn-outline-secondary",
                  "Create"
                ),
                shiny::tags$button(
                  class = "btn btn-outline-primary",
                  id = "confirmNewConnection",
                  style = "display: none;",
                  "Confirm"
                )
              )
            )
          )

          shinyjs::onclick(
            "createConnection",
            {
              shinyjs::hideElement("newConnectionType")
              shinyjs::showElement(
                id = glue::glue(
                  "{input$newConnectionType}ConnectionForm"
                ),
              )
              shinyjs::hideElement("createConnection")
              shinyjs::showElement("confirmNewConnection")
            }
          )

          shinyjs::onclick("confirmNewConnection", {
            if (input$newConnectionType == "postgres"){

              connections_df <-
                write_new_connection_postgres(
                  input$newPostgresConnectionName,
                  input$newPostgresHost,
                  input$newPostgresUser,
                  input$newPostgresPassword,
                  input$newPostgresPort,
                  input$newPostgresDbname
                )

            } else if(input$newConnectionType == "teradata"){
              new_name <- input$newTeradataConnectionName
              new_server <- input$newTeradataServer
              new_username <- input$newTeradataUsername
              new_password <- input$newTeradataPassword

              connections_df <-
                read_csv(
                  glue("{dbc_path}\\database_connections.csv"),
                  show_col_types = FALSE
                )

              connections_df <-
                connections_df |>
                bind_rows(
                  data.frame(
                    connection_id = nrow(connections_df) + 1,
                    connection_name = new_name
                  )
                )

              write_csv(
                data.frame(
                  driver = "teradata",
                  server = new_server,
                  username = new_username,
                  password = new_password
                ),
                glue(
                  "{dbc_path}\\connection_{nrow(connections_df)}.csv"
                )
              )
            } else if(input$newConnectionType == "vertica"){
              new_name <- input$newVerticaConnectionName
              new_server <- input$newVerticaServer
              new_username <- input$newVerticaUsername
              new_password <- input$newVerticaPassword
              new_port <- input$newVerticaPort
              new_database <- input$newVerticaDatabase

              connections_df <-
                read_csv(
                  glue("{dbc_path}\\database_connections.csv"),
                  show_col_types = FALSE
                )

              connections_df <-
                connections_df |>
                bind_rows(
                  data.frame(
                    connection_id = nrow(connections_df) + 1,
                    connection_name = new_name
                  )
                )

              write_csv(
                data.frame(
                  driver = "vertica",
                  server = new_server,
                  username = new_username,
                  password = new_password,
                  port = new_port,
                  database = new_database
                ),
                glue(
                  "{dbc_path}\\connection_{nrow(connections_df)}.csv"
                )
              )

            } else if(input$newConnectionType == "snowflake"){
              new_name <- input$newSnowflakeConnectionName
              new_server <- input$newSnowflakeServer
              new_user <- input$newSnowflakeUser
              new_password <- input$newSnowflakePassword
              new_database <- input$newSnowflakeDatabase
              new_role <- input$newSnowflakeRole
              new_warehouse <- input$newSnowflakeWarehouse

              connections_df <-
                read_csv(
                  glue("{dbc_path}\\database_connections.csv"),
                  show_col_types = FALSE
                )

              connections_df <-
                connections_df |>
                bind_rows(
                  data.frame(
                    connection_id = nrow(connections_df) + 1,
                    connection_name = new_name
                  )
                )

              write_csv(
                data.frame(
                  driver = "snowflake",
                  server = new_server,
                  user = new_user,
                  password = new_password,
                  database = new_database,
                  role = new_role,
                  warehouse = new_warehouse
                ),
                glue(
                  "{dbc_path}\\connection_{nrow(connections_df)}.csv"
                )
              )
            }


            # Replace the connections file
            file.remove(
              glue::glue("{dbc_path}\\database_connections.csv")
            )

            readr::write_csv(
              connections_df,
              glue::glue("{dbc_path}\\database_connections.csv")
            )

            connections <- connections_df$connection_id
            names(connections) <- connections_df$connection_name

            shiny::updateSelectInput(
              inputId = "connectionSelect",
              choices = connections,
              selected = connections[-1]
            )

            shiny::removeModal()
          }
          )
        }
      )

      #-------------------------------------------------------------------------

      # Manage connections modal
      shinyjs::onclick("manageConnectionsButton", {

        # Get most updated connections file
        database_credentials <-
          readr::read_csv(
            glue::glue("{dbc_path}\\connection_{input$connectionSelect}.csv"),
            show_col_types = FALSE
          )

        # Set table proxy
        proxy <- DT::dataTableProxy("dbConnectionsTable")

        # Show the modal
        shiny::showModal(
          shiny::modalDialog(
            easyClose = TRUE,
            size = "xl",
            title = "Manage Connection",
            shiny::div(
              class = "table-responsive",
              style = "max-height: 70vh;",
              DT::DTOutput("dbConnectionsTable")
            ),
            footer = shiny::tagList(
              shiny::tags$button(
                id = "deleteConnection",
                class = "btn btn-outline-danger",
                "Delete"
              ),
              shiny::tags$button(
                id = "manageConfirm",
                class = "btn btn-outline-secondary",
                style = "display: none;",
                "Confirm Changes",
                "data-bs-dismiss" = "modal"
              )
            )
          )
        )


        # Create an editable datatable
        output$dbConnectionsTable <- DT::renderDT({
          DT::datatable(
            database_credentials,
            options = list(dom = "t"),
            editable = TRUE
          )
        })

        # # Save changes to csv file
        shiny::observeEvent(input$dbConnectionsTable_cell_edit, {
          shinyjs::showElement("manageConfirm")
          database_credentials <<- DT::editData(
            database_credentials,
            input$dbConnectionsTable_cell_edit,
            proxy
          )
        })

        # Delete row button
        shinyjs::onclick("deleteConnection", {
          connections_df <-
            readr::read_csv(
              glue::glue("{dbc_path}\\database_connections.csv"),
              show_col_types = FALSE
            )

          connections_df <-
            connections_df |>
            dplyr::filter(
              connection_id != input$connectionSelect
            ) |>
            dplyr::mutate(
              old_id = connection_id,
              new_id = dplyr::row_number()
            )

          connections_df |>
            dplyr::select(
              old_id,
              new_id
            ) |>
            purrr::pmap(
              function(old_id, new_id){
                file.rename(
                  glue::glue(
                    "{dbc_path}\\connection_{old_id}.csv",
                  ),
                  glue::glue(
                    "{dbc_path}\\connection_{new_id}.csv",
                  )
                )
              }
            )

          connections_df |>
            dplyr::select(
              connection_id = new_id,
              connection_name
            ) |>
            readr::write_csv(
              glue::glue(
                "{dbc_path}\\database_connections.csv"
              )
            )

          connections <- connections_df$connection_id
          names(connections) <- connections_df$connection_name

          shiny::updateSelectInput(
            inputId = "connectionSelect",
            choices = connections
          )

          shiny::removeModal()
          shiny::showNotification("Done!")
        })

        # Refresh the page
        shinyjs::onclick("manageConfirm", {
          # Replace the connections file
          file.remove(
            glue::glue(
              "{dbc_path}\\connection_{input$connectionSelect}.csv"
            )
          )
          readr::write_csv(
            database_credentials,
            glue::glue("{dbc_path}\\connection_{input$connectionSelect}.csv")
          )

        })
      })

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
