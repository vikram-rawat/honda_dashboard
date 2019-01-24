source('www/Modules/mod_SQL_functions.R')

# library(shiny)
# library(shinydashboard)
# library(RSQLite)
# library(data.table)
# library(DBI)
# library(tidyverse)

# Wrap shinymaterial apps in material_page
mod_login_ui <-function(id){
    
    
# namespace ---------------------------------------------------------------

ns<-NS(id)


# ui_function -------------------------------------------------------------
tagList(
        fluidRow(),
        fluidRow(column(width = 3),
                 column(
                     width = 6,
                     inputPanel(
                         textInput(ns('userid'), "Enter UserName"),
                         br(),
                         passwordInput(ns('password'), 'Enter Password'),
                         br(),
                         actionButton(
                             inputId = ns('login'),
                             label = 'Login',
                             icon = icon('user')
                         )
                         
                     )
                 ))
    )
}




mod_login_serve <- function(input, output, session, conn_string) {
    
    # namespace ---------------------------------------------------------------
    
    ns<-session$ns
    

    # Set Roles ---------------------------------------------------------------

    
    user_role <- reactiveValues(
        value = 'Nothing'
        # ,userid=input$userid
    )
    
    
    
    # server_function ---------------------------------------------------------
    
    
    
    sql_df <- reactive({
        decrypt_sql(
            sql_con = 'www/main_data.sqlite'
            ,
            sql_table = 'emp_user'
            ,
            decode_file = 'www/login/user.bin'
        )
        
    })
    

    # count_login_attempt -----------------------------------------------------
    
    
    login_attempt<-reactiveValues(number=0) 
    
    

    
    
    observeEvent(input$login,
                 
                 
                 if (login_attempt$number<5) {
                     
                     (if (nrow(sql_df()[(userid == isolate(input$userid)) &
                                       (password == isolate(input$password)), ]) ==
                         1) {
                         
                         user_role$value <- sql_df()[(userid == isolate(input$userid)) &
                            (password ==isolate(input$password)), role]
                         
                         
                         # (conn_string %>% 
                         #         dbWriteTable('workinglogs',
                         #                      data.frame(username=input$userid,
                         #                                 action='Logged_in',
                         #                                 record_date=Sys.time()),
                         #                      append=TRUE)
                         #     )
                         
                         login_attempt$number<-0
                         
                     } else{
                         
                         login_attempt$number<- (login_attempt$number + 1)
                         
                         user_role$value<-'Nothing'
                         
                         showModal(
                             modalDialog(
                                 title = "Try Again",
                                 'You have used Wrong ID or Password. Please Try Again',
                                 easyClose = TRUE,
                                 footer = NULL
                             )
                         )
                         
                         
                     })
                     
                 } else if (login_attempt$number >= 5) {
                     
                     
                     login_attempt$number<- (login_attempt$number + 1)
                     
                     user_role$value<-'Nothing'
                     
                     
                     showModal(
                         modalDialog(
                             title = "Please restart the App",
                             'You have exceeded maximum number of attemps',
                             easyClose = FALSE,
                             footer = NULL
                         )
                     )
                     
                 })
    
    
    return(user_role)
    
    
}
