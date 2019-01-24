# Use ID like This
#
#
#          userid   password       date_inserted    role
# 1: system_admin GreyValley 2018-05-28 21:08:52   Admin
# 2:      visitor  simple123 2018-05-29 12:24:04 Visitor
# 3:       viewer  simple123 2018-07-11 14:31:42 Visitor


# library -----------------------------------------------------------------
library(pacman)
p_load(data.table,
DBI,
tidyverse,
shinyjs,
dbplyr,
DT,
flexdashboard,
lubridate,
plotly,
pool,
RSQLite,
safer,
shiny,
shinyBS,
shinydashboard)

library(data.table)

# sources -----------------------------------------------------------------


source('www/Modules/Main_app.R')


# ui_function -------------------------------------------------------------


ui <- main_ui('app')


# server_function ---------------------------------------------------------


server <- function(input, output, session) {
 
    
    
    callModule(main_server,'app')

    
    
    }


# Run Call ----------------------------------------------------------------


shinyApp(ui, server)