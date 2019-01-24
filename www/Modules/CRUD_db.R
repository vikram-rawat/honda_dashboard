# library -----------------------------------------------------------------

# library(shiny)
# library(shinyBS)
# library(shinydashboard)
# library(shinyjs)
# library(flexdashboard)
# library(DBI)
# library(pool)
# library(DT)


# ModuleUi ----------------------------------------------------------------

mod_crud_ui <- function(id) {
    
    ns <- NS(id)
    
    fluidPage(shinyjs::useShinyjs(),
              sidebarLayout(
                  sidebarPanel(
                      radioButtons(ns('method'),'Choose an Action',choices = 
                                       c('Insert','Update','Delete')),
                    uiOutput(ns('well_panel'))                   
                      
                ),
                  mainPanel(useShinyjs(),
                            dataTableOutput(ns('State'))
                            # ,tableOutput(ns('test'))
                            )
                  
              ))
}

# ModuleServer ------------------------------------------------------------


mod_crud_serve <- function(input, output, session, connstring) {
    
    # namespace ---------------------------------------------------------------
    
    ns <- session$ns
    
    # Time --------------------------------------------------------------------
    
    time_value<-reactive({
        
        invalidateLater(5000, session)
        
        Sys.time()
    })

    
    output$Time <- renderUI({
        
        disabled(textInput(ns('timeid'), 'Current Time', value = time_value()))
    })
    

    # all_usernames -----------------------------------------------------------

    all_usernames<-reactive({
        getdata()[,userid]
    })


    # selectusername ----------------------------------------------------------

    output$user_list<-renderUI({
        selectInput(ns('select_user'),'Select UserName',
                    choices = all_usernames())
    })
    
    # select roles ----------------------------------------------------------
    
    output$role_select<-renderUI({
        selectInput(ns('role_list'),'Select Rights',
                    choices = c('Admin','Visitor'))
    })
    

    # Create Encrypted Data Frame ---------------------------------------------
    
    encrypt_df<-reactive({
        
        req(input$password)
        
        somevalue<-data.frame(userid=input$username,
                      password=input$password,
                      date_inserted=input$timeid,
                      role=input$role_list)
        
        encrypt_sql(table = somevalue,decode_file = 'www/login/user.bin')
        
        })

    
    encrypt_df_up<-reactive({
        req(input$password_up)
        
        somevalue<-data.frame(userid=input$select_user,
                              password=input$password_up,
                              date_inserted=input$timeid,
                              role=input$role_list)
        
        encrypt_sql(table = somevalue,decode_file = 'www/login/user.bin')

    })
    
    
    # output$test<-renderTable(encrypt_df())
    
    
    # Insert Data -------------------------------------------------------------------
    observeEvent(input$InsertData,{
        
        if(input$username %in% all_usernames()){
        
            showModal(
                modalDialog(
                    title = "Alert",
                    "Someone has already chosen this UserName",
                    easyClose = FALSE
                ))
                
        }else{
            dbWriteTable(connstring,
                         'emp_user',
                         encrypt_df(),
                         append=TRUE)
            
            
            showModal(
                modalDialog(
                    title = "Data Inserted",
                    "Please Don't click Insert again untill you want to change it!",
                    easyClose = FALSE
                )
            )    
        }
        })
    
    # delete Data -------------------------------------------------------------

    observeEvent(input$delete_data,{
        
        dbSendStatement(connstring,
                        paste("delete from emp_user where userid ='"
                              ,encrypt_sql(data.frame(userid=input$select_user),
                                           'www/login/user.bin')[1]
                              ,"'",sep = '')
                        )
        
        showModal(
            modalDialog(
                title = "Data Deleted",
                "Please Don't click Delete again.",
                easyClose = FALSE
            )
        )
    })

    # Update Data -------------------------------------------------------------
    
    observeEvent(input$Update_data,{
        
        dbSendStatement(connstring,
                        paste("delete from emp_user where userid ='"
                              ,encrypt_sql(data.frame(userid=input$select_user),
                                           'www/login/user.bin')[1]
                              ,"'",sep = '')
        )
        
        
        dbWriteTable(connstring,
                     'emp_user',
                     encrypt_df_up(),
                     append=TRUE)
        
        showModal(
            modalDialog(
                title = "Data Corrected",
                "Please Don't click Update again untill you Want to change it!",
                easyClose = FALSE
            )
        )
        

    })
    
    

  
    # DataTable ---------------------------------------------------------------
    
    getdata <- reactivePoll(500,session,
                            checkFunc = function(){
                                
                                checkDF<-decrypt_sql(sql_con = 'www/main_data.sqlite'
                                                    ,sql_table = 'emp_user'
                                                    ,decode_file = 'www/login/user.bin')
                                
                                checkDF[,':='(date_inserted=ymd_hms(date_inserted))]
                                
                                return(checkDF[,mean(date_inserted)])
                                
                            },
                            valueFunc = function(){
                                
                                someDF<-decrypt_sql(sql_con = 'www/main_data.sqlite'
                                                    ,sql_table = 'emp_user'
                                                    ,decode_file = 'www/login/user.bin')
                                                    
                                someDF[,':='(date_inserted=ymd_hms(date_inserted))]
                                
                                return(someDF)
        })
    
    output$State <- renderDataTable(getdata())
    

    # Render Well Panel -------------------------------------------------------
observeEvent(input$method,
             {
                 if(input$method == 'Insert'){
                     output$well_panel<-renderUI(
                         wellPanel(
                             uiOutput(ns('Time')),
                             bsTooltip(ns('Time'), 'Time of Entry into the System'),
                             bsTooltip(ns('username'), 'Type a Unique UserName'),
                             textInput(ns('username'), 'UserName', value =
                                           ''),
                             passwordInput(ns('password'),'Enter Password'),
                             bsTooltip(ns('password'), 'Type a Password'),
                             uiOutput(ns('role_select')),
                             bsTooltip(ns('role_select'), 'Assign Roles'),
                             actionButton(ns("InsertData"),"Insert Data")    
                         )
                     )
                 }else if(input$method == 'Delete'){
                     output$well_panel<-renderUI(
                     wellPanel(
                         uiOutput(ns('user_list')),
                         actionButton(ns("delete_data"),"Delete Data")
                     ))
                     
                 }else if(input$method == 'Update'){
                     output$well_panel<-renderUI(
                         wellPanel(
                             uiOutput(ns('Time')),
                             bsTooltip(ns('Time'), 'Time of Entry into the System'),
                             uiOutput(ns('user_list')),
                             passwordInput(ns('password_up'),'Enter Password'),
                             bsTooltip(ns('password_up'), 'Type a Password'),
                             uiOutput(ns('role_select')),
                             bsTooltip(ns('role_select'), 'Assign Roles'),
                             actionButton(ns("Update_data"),"Update Data")
                         ))
                 }
                 
             })
    
    
    
    # End ---------------------------------------------------------------------
}