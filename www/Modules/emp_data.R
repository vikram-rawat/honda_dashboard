# load_library ------------------------------------------------------------

# library(shiny)
# library(RSQLite)
# library(dbplyr)
# library(data.table)
# library(tidyverse)
# library(plotly)
# library(DBI)
# library(tibbletime)
# library(tidyquant)
# library(flexdashboard)
# library(crosstalk)
# library(shinydashboard)

# # ui_function -------------------------------------------------------------



mod_emp_ui <- function(id) {
    # namespace_ui ------------------------------------------------------------
    
    ns <- NS(id)
    # main_code ---------------------------------------------------------------
    
    
    sidebarLayout(
        sidebarPanel =
            sidebarPanel(
                fluidRow(
                    downloadButton(ns('download_button'),
                    'Download_Scale_Data'),    
                    uiOutput(ns(
                    'select_emp_code'
                ))),
                fluidRow(
                    plotlyOutput(ns('output_bar'),
                               height = '200px')),
                fluidRow(dataTableOutput(ns('detail_table'))),
                fluidRow(),
                fluidRow(dataTableOutput(ns('scale_table')))
            ),
        mainPanel =
            mainPanel(
                fluidRow(
                    infoBoxOutput(ns('fatigue_value1')),
                    infoBoxOutput(ns('fatigue_value2')),
                    infoBoxOutput(ns('fatigue_value3'))    
                ),
                fluidRow(
                    plotlyOutput(ns('output_MA'))
                )
  
            )
    )
    
    
}





# server_function ---------------------------------------------------------

mod_emp_serve <-
    function(input,
             output,
             session,
             con_string,
             main_data) {
        # namespace_server ---------------------------------------------------------
        
        ns <- session$ns
        

        # emp_codes ---------------------------------------------------------------
        
        emp_codes <- reactive({
            setorder(main_data(), emp_code)
            main_data()[, unique(emp_code)]
            
        })
        
        # selectPanel -------------------------------------------------------------
        
        
        output$select_emp_code <- renderUI({
            selectInput(
                ns('selectemp'),
                'Select Employee ID',
                multiple = TRUE,
                choices = emp_codes()
            )
        })
        
        # scaled_value ------------------------------------------------------------
        
        

        scaled_value <- reactive({
            req(input$selectemp)
            

            scale <- main_data()[,
                           .SD[which.max(date)],emp_code][
                               ,.(emp_code,name,position,shop,
                                  date,overtime,organization,cum,
                                  new_week,
                                  scales=round(moving_avg,2))]
            

            
            setkey(scale, emp_code)
            
            scale[, subs := cut(
                scales,
                c(0, 1, 2, 3.5, 5, 100),
                labels = c(
                    'Perfect',
                    'Healthy',
                    'little-Tired',
                    'exhausted',
                    'frustrated'
                ),include.lowest = TRUE
            )]
            
            scale[, colors := cut(
                scales,
                c(0, 1, 2, 3.5, 5, 100),
                labels = c('olive',
                           'teal',
                           'purple',
                           'yellow',
                           'red'
                ),include.lowest = TRUE
            )]
            
        })
        
        
        
        # create infobox 1----------------------------------------------------------
        
        output$fatigue_value1 <- renderInfoBox({
            req(input$selectemp[1])
            
            infoBox(
                title = paste('Fatigue Value -',
                              scaled_value()[emp_code == input$selectemp[1], emp_code]),
                scaled_value()[emp_code == input$selectemp[1], scales],
                subtitle = as.character(scaled_value()[emp_code == input$selectemp[1], subs]),
                color = as.character(scaled_value()[emp_code == input$selectemp[1], colors]),
                fill = TRUE
            )
            
        })
        
        # create infobox 2----------------------------------------------------------
        
        output$fatigue_value2 <- renderInfoBox({
            req(input$selectemp[2])
            
            infoBox(
                title = paste('Fatigue Value -',
                              scaled_value()[emp_code == input$selectemp[2], emp_code]),
                scaled_value()[emp_code == input$selectemp[2], scales],
                subtitle = as.character(scaled_value()[emp_code == input$selectemp[2], subs]),
                color = as.character(scaled_value()[emp_code == input$selectemp[2], colors]),
                fill = TRUE
            )
            
        })
        
        # create infobox 3----------------------------------------------------------
        
        output$fatigue_value3 <- renderInfoBox({
            req(input$selectemp[3])
            
            infoBox(
                title = paste('Fatigue Value -',
                              scaled_value()[emp_code == input$selectemp[3], emp_code]),
                scaled_value()[emp_code == input$selectemp[3], scales],
                subtitle = as.character(scaled_value()[emp_code == input$selectemp[3], subs]),
                color = as.character(scaled_value()[emp_code == input$selectemp[3], colors]),
                fill = TRUE
            )
            
        })
        
        
        # scale_table -------------------------------------------------------------
        
        output$scale_table <- renderDataTable(scaled_value()[
            emp_code %in% as.numeric(input$selectemp),
            .(emp_code, scales, subs)],
            options = list(dom = ''))
        
        
        
        # emp_data ----------------------------------------------------------------
        
        emp_data <- reactive({
            req(input$selectemp)
            
            main_data()[emp_code %in% input$selectemp,]
            
        })
        
        
        # name_data ---------------------------------------------------------------
        
        name_data <- reactive({
            req(input$selectemp)
            
            emp_data() %>% setDT()
            
            emp_data()[, .(
                start = min(date),
                end = max(date),
                OT = sum(overtime)
            ), .(name)]
            
        })
        
        # renderTable -------------------------------------------------------------
        
        output$detail_table <- renderDataTable(name_data(),
                                               options = list(dom = ''))
        
        
        # chart -------------------------------------------------------------------
        
        
        moving_chart <- reactive({
            req(input$selectemp)
            
            (emp_data() %>% 
                    ggplot(aes(date,moving_avg,group=emp_code,color=(emp_code)))+
                    geom_line()+
                    geom_hline(yintercept = 2,color=I('orange'),linetype=2)+
                    geom_hline(yintercept = 5,color=I('red'),linetype=2)
            ) %>% 
                ggplotly() %>% 
                layout(plot_bgcolor='transparent',
                       paper_bgcolor='transparent')
            
        })
        
        
        
        # plot_ly plot ------------------------------------------------------------
        
        bar_plot <- reactive({
            req(input$selectemp)
            
            main_data()[emp_code %in% input$selectemp][,
                .(date, overtime, emp_code = as.factor(emp_code))] %>%
                plot_ly(
                    x =  ~ date,
                    y =  ~ overtime,
                    color = ~ emp_code
                ) %>%
                add_bars()%>%
                layout(plot_bgcolor = 'transparent',
                       paper_bgcolor = 'transparent')


        })
        
        
        # generate plot -----------------------------------------------------------
        
        
        output$output_MA <- renderPlotly(moving_chart())
        
        
        output$output_bar <- renderPlotly(bar_plot())
        

        # Download_Data -----------------------------------------------------------
        
        output$download_button <- downloadHandler(
            filename = function() {
                "Fatigue_Scale.csv"
            },
            content = function(file) {
                fwrite(scaled_value()[scales<=2,], file, row.names = FALSE)
            }
        )
        
        # end_server --------------------------------------------------------------
        
        
        # # disconnect database -----------------------------------------------------
        # onSessionEnded(function() {
        #     dbDisconnect(con_string())
        # })
        # 
        
        
    }