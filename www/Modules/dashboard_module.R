
# load_libraries ----------------------------------------------------------

# library(shiny)
# library(shinydashboard)
# library(plotly)
# library(data.table)
# library(lubridate)
# library(dbplyr)
# library(dplyr)
# library(ggplot2)
# library(magrittr)
# library(DBI)
# library(crosstalk)
# library(RSQLite)
# library(DT)


# UI_function -------------------------------------------------------------



mod_dashboard_ui<-function(id){


# namespace ---------------------------------------------------------------

    
    ns<-NS(id)

# ui_code -----------------------------------------------------------------

    tagList(
    fluidRow(infoBoxOutput(ns('max_ot')),
             column(
                 8, plotlyOutput(ns('overtime_daywise'),
                                 height = '150px')
             )),
    fluidRow(column(
        9,
        plotlyOutput(ns('overtime_plot'),
                     width = "100%", height = "530px")
    ),
    column(3, dataTableOutput(ns('top_employee'))))
)

# end_ui ------------------------------------------------------------------
}


# server_function ---------------------------------------------------------

mod_dashboard_serve<-function(input,output,session,sqlite,data){
    

    # namespace ---------------------------------------------------------------

    ns<-session$ns
    
    # generate_data -----------------------------------------------------------


    
    data_top_10 <- reactive({
        temp10 <- data()[, .(cum = sum(overtime),
                                 date = max(date)),
                             .(emp_code, name)][order(-cum), ][1:10,]
        
        setkey(temp10, emp_code)
        setorder(temp10, -cum)
        
    })
    
    
    data_last_date <- reactive({
        data()[, .(cum = sum(overtime),
                       date = max(date)),
                   .(emp_code, name)]
    })
    
    
    
    
    
    # overTime_daily_plot -----------------------------------------------------
    
    output$overtime_plot <- renderPlotly({
        plot_ot_colored()
    })
    
    plot_ot_colored <- reactive({
        req(data())
        
        (
            data() %>%
                ggplot(aes(
                    date,
                    cum,
                    group = emp_code,
                    text = paste('Name    : <b>', name ,'</b> \n',
                                 'Emp_ID  : <b>', emp_code ,'</b> \n',
                                 'Total_OT: <b>', cum ,'</b> \n',
                                 'Date    : <b>', date ,'</b>'
                                 )
                )) +
                geom_line(aes(
                    color = line_color,
                    alpha = line_color
                )) +
                scale_colour_manual(
                    values =
                        c(
                            black = 'black',
                            green = 'green',
                            orange = 'orange',
                            red = 'red'
                        )
                ) +
                scale_alpha_manual(values = c(
                    black = .07,
                    green = .15,
                    orange = .4,
                    red = .6
                )) +
                geom_point(
                    data = data_top_10(),
                    aes(date,
                        cum,
                        fill = I('steelblue')),
                    size = 3,
                    alpha = I(.4)
                ) +
                geom_point(
                    data = data_last_date(),
                    aes(date,
                        cum,
                        fill = I('royalblue')),
                    alpha = I(.12),
                    size = I(.7)
                ) +
                
                geom_hline(
                    yintercept = 50,
                    color = 'red',
                    size = I(.7),
                    alpha = I(.7),
                    linetype = 'dashed'
                ) + ylab('OverTime')
        ) %>%
            ggplotly(tooltip = 'text') %>%
            hide_legend() %>%
            layout(plot_bgcolor = 'transparent',
                   paper_bgcolor = 'transparent')
        
        
    })
    
    
    # employee_table_top10 ----------------------------------------------------
    
    output$top_employee <- renderDataTable(data_top_10()[, .(name, OT =
                                                                 cum, date)],
                                           options = list(
                                               dom = '',
                                               pageLength = 10,
                                               searching = FALSE
                                               
                                           ))
    
    
    
    # info_max_Ot -------------------------------------------------------------
    
    data_max_ot <- reactive({
        data()[, .(overtime = sum(overtime)), date][which.max(overtime),]
    })
    
    output$max_ot <- renderInfoBox(infoBox(
        paste('Maximum Overtime',
              data_max_ot()[, date]),
        data_max_ot()[, overtime],
        icon = icon('users'),
        fill = TRUE
    ))
    
    
    
    # plotly_overtime_daywise -------------------------------------------------
    
    data_ot_days <- reactive({
        data()[, .(overtime = sum(overtime)), date]
    })
    
    output$overtime_daywise <- renderPlotly({
        data_ot_days() %>%
            plot_ly(x =  ~ date, y =  ~ overtime) %>%
            add_bars() %>%
            hide_legend() %>%
            layout(plot_bgcolor = 'transparent'
                   , paper_bgcolor = 'transparent')
        
    })
}


