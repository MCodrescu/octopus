octopus_ui <-
  function(){
    ui <- shiny::bootstrapPage(
      theme = bslib::bs_theme(version = 5),

      # Initiate shinyjs
      shinyjs::useShinyjs(),


      #######################################################

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
              )
            )
          )
        )
      ),

      #########################################################

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
                  shiny::tags$img(
                    src = "plus-square.svg",
                    style = "width: 25px; height: 25px;"
                  )
                ),
                # Edit connections button
                shiny::tags$button(
                  class = "btn btn-light mt-3 float-right",
                  id = "manageConnectionsButton",
                  shiny::tags$img(
                    src = "gear.svg",
                    style = "width: 25px; height: 25px;"
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
            class = "col-10 col-lg-8 bg-light pb-2 pt-1 border rounded shadow",
            id = "queryDiv",
            style = "display: none; height: 80vh;",

            shiny::div(
              class = "row mt-0 pt-0",
              shiny::div(
                class = "col",
                shiny::tags$button(
                  id = "formatQuery",
                  class = "btn btn-sm btn-outline-none float-right",
                  shiny::tags$img(
                    src = "info-square.svg",
                    style = "width: 20px; height: 20px;"
                  ),
                  " Format "
                ),
                shiny::tags$button(
                  id = "submitQuery",
                  class = "btn btn-sm btn-outline-none float-right",
                  shiny::tags$img(
                    src = "caret-right-square.svg",
                    style = "width: 20px; height: 20px;"
                  ),
                  " Run "
                ),
              )
            ),

            shiny::div(
              class = "row h-100 pt-1",
              shiny::div(
                class = "col",
                # Send query to database
                shinyAce::aceEditor(
                  "query",
                  mode = "pgsql",
                  height = "95%",
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

  }
