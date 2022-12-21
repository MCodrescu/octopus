run_app <-
  function(){
    ui <- octopus_ui()

    server <- function(input, output, session) {
    }

    shiny::shinyApp(ui, server)
  }
