# Interactively Test the Modal
if (interactive()){
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv)

  n <- 100000
  example <- data.frame(
    x = runif(n),
    y = runif(n),
    z = runif(n)
  )

  DBI::dbWriteTable(
    con,
    "example",
    example,
    temporary = FALSE,
    overwrite = TRUE
  )

  ui <- shiny::basicPage()

  server <- function(input, output, session){

    table_modal_w_download_full_Server(
      id = "preview",
      con = con,
      schema = "main",
      table = "example",
      n_rows = n,
      step_size = 10
    )

    table_modal_w_download_UI(
      id = "preview",
      title = "Preview Table Example",
      download_title = "Download Table",
      n_rows = n,
      result = example[1:1000,]
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}

# Same as Above but Using Larger Database
if (interactive()){
  con <- DBI::dbConnect(
    odbc::odbc(),
    dsn = "Snowflake_BBR",
    pwd = keyring::key_get("Upstart")
  )

  res <- DBI::dbSendQuery(
    con,
    "CREATE TEMPORARY TABLE SDW_ECDW_ATT_VIEWS.EXAMPLE AS
    SELECT * FROM SDW_ECDW_ATT_VIEWS.PRSD_ADDR LIMIT 1000000"
  )

  DBI::dbClearResult(res)

  ui <- shiny::basicPage()

  server <- function(input, output, session){

    table_modal_w_download_full_Server(
      id = "preview",
      con = con,
      schema = "SDW_ECDW_ATT_VIEWS",
      table = "EXAMPLE",
      n_rows = 1000000
    )

    result <- get_preview_snowflake(
      con,
      schema = "SDW_ECDW_ATT_VIEWS",
      table = "EXAMPLE"
    )

    table_modal_w_download_UI(
      id = "preview",
      title = "Preview Table EXAMPLE",
      download_title = "Download Table",
      n_rows = 1000000,
      result = result
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
