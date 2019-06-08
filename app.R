# sources -----------------------------------------------------------------

source("global.R")

# ui_function -------------------------------------------------------------

ui <- main_ui('app')

# server_function ---------------------------------------------------------

server <- function(input, output, session) {
    callModule(main_server, 'app')
}

# Run Call ----------------------------------------------------------------

shinyApp(ui, server,options = list(port = 3333))