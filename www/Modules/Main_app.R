# front_end ---------------------------------------------------------------

main_ui <- function(id){

# namespace ---------------------------------------------------------------

ns<-NS(id)

# Ui_function -------------------------------------------------------------
    dashboardPage(dashboardHeader(title = 'HR Dashboard',
                                  tags$li(actionLink(ns("openModal"),
                                                     label = "", 
                                                     icon = icon("sign-out")),
                                          class = "dropdown")
                                  ),
                  dashboardSidebar(sidebarMenuOutput(ns('sidebar_ui'))),
                  dashboardBody(
                      includeCSS('www/style.css'),
                      tabItems(
                      tabItem(
                          tabName = 'login_tab',
                          mod_login_ui(ns('loginPage'))
                      ),
                      tabItem(
                          tabName = 'dashboard_tab',
                          mod_dashboard_ui(ns('dashboardpage'))
                      ),
                      tabItem(tabName = 'sql_data_tab',
                              
                              
                              mod_select_data_ui(ns('sql'))
                      ),
                      tabItem(tabName = 'employee_tab',
                              mod_emp_ui(ns('employee'))
                      ),
                      tabItem(tabName = 'user_tab',
                              mod_crud_ui(ns('user'))
                      )
                  )))
}
    

# server ------------------------------------------------------------------

main_server <- function(input, output, session) {
    

    # namespace ---------------------------------------------------------------

    ns<-session$ns
    
   
    # connect_to_DB -----------------------------------------------------------
    
    
    sqlite <- reactive({
        dbConnect(SQLite(),
                  'www/main_data.sqlite')
    })
    
    # Show_Login --------------------------------------------------------------
    
    role<-callModule(mod_login_serve,'loginPage',sqlite())
    
    # retreive_data -----------------------------------------------------------
    
    fulldata<-callModule(mod_select_data_serve,'sql',sqlite)
    
    # show_dashboard ----------------------------------------------------------
    
    callModule(mod_dashboard_serve,'dashboardpage',sqlite,fulldata)
    
    # show_employee -----------------------------------------------------------
    
    callModule(mod_emp_serve,'employee',sqlite,fulldata)
    
    # show_passwords ----------------------------------------------------------
    
    callModule(mod_crud_serve,'user',sqlite())
    

    # RengerLogin_logic -------------------------------------------------------

    observeEvent(role$value,{
                 if(role$value=='Nothing'){
                     
                     output$sidebar_ui<-renderMenu({
                         sidebarMenu(id = ns('conditional_sidebar'),
                             menuItem('login_menu', icon = icon('sign-in'),
                                      tabName = 'login_tab'),
                             selected='login_menu')
                     })
                 }else if( role$value=='Visitor'){
                     output$sidebar_ui<-renderMenu({
                         sidebarMenu(id = ns('conditional_sidebar'),
                             menuItem('MainData', icon = icon('database'),
                                      tabName = 'sql_data_tab'),
                             menuItem(
                                 'Dashboard',
                                 icon = icon('dashboard'),
                                 tabName = 'dashboard_tab'
                             ),
                             menuItem(
                                 'Employee',
                                 icon = icon('user'),
                                 tabName = 'employee_tab'
                             ))  
                     })
                 }else if( role$value=='Admin'){
                     output$sidebar_ui<-renderMenu({
                       
                         sidebarMenu(id = ns('conditional_sidebar'),
                             menuItem('MainData', icon = icon('database'),
                                      tabName = 'sql_data_tab'),
                             menuItem(
                                 'Dashboard',
                                 icon = icon('dashboard'),
                                 tabName = 'dashboard_tab'
                             ),
                             menuItem(
                                 'Employee',
                                 icon = icon('user'),
                                 tabName = 'employee_tab'
                             ),
                             menuItem(
                                 'Password',
                                 icon = icon('users'),
                                 tabName = 'user_tab'
                             )
                         )  
                     })
                 }
            })
    # signoutbutton -----------------------------------------------------------
    # observeEvent(role$value,{
    # if(role$value !='Nothing'){
    #     output$signout<-renderUI({
    #         
    #         req(role$value)
    #         
    #         tags$li(actionLink(ns("openModal"),
    #                            label = "", icon = icon("sign-out")),
    #                 class = "dropdown")
    #     })
    # }})

    # Signout -----------------------------------------------------------------
    
    observeEvent(input$openModal, {
         role$value<-'Nothing'
         # 
         # (sqlite() %>%
         #         dbWriteTable('workinglogs',
         #                      data.frame(username=role$userid,
         #                                action='Logged_out',
         #                                record_date=Sys.time()),
         #                      append=TRUE)
         # )
    })
    
    
    # # dbdisconnect ------------------------------------------------------------
    # 
    # onSessionEnded(function(){
    #     dbDisconnect(sqlite())
    # })    
    # 
    # 
}
# # runapp ------------------------------------------------------------------
# 
# shinyApp(main_ui, main_server)