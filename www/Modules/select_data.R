# UI_function -------------------------------------------------------------


mod_select_data_ui <- function(id) {
    # namespace ---------------------------------------------------------------
    
    ns <- NS(id)
    
    
    # data_panel --------------------------------------------------------------
    
    
    sidebarLayout(
        sidebarPanel(
            uiOutput(ns('date_range_filter')),
            uiOutput(ns('org_select_filter')),
            downloadButton(ns('download_button'),
                           'Download File')
        ),
        mainPanel(DTOutput(ns('sql_data')))
    )
    
    
    # endUI -------------------------------------------------------------------
    
    
}


# server_function ---------------------------------------------------------


mod_select_data_serve <- function(input, output, session,sqlite) {
    # namespace ---------------------------------------------------------------
    
    ns <- session$ns

    # download_data -----------------------------------------------------------
    
    fulldata <- reactive({
        req(input$sql_date_filter)
        req(input$sql_org_filter)
        
        minDate <- input$sql_date_filter[1]
        maxDate <- input$sql_date_filter[2]
        org <- input$sql_org_filter
        
        data1 <- sqlite() %>%
                 tbl('employee_data') %>%
                 filter(between(date
                               , minDate
                               , maxDate)) %>%
                 filter(organization %in% org) %>%
                 collect()
        
        data1 %>% setDT()
        
        data1[, date := as.Date(date)]
        data1[, emp_code := as.factor(emp_code)]
        
        setkey(data1, emp_code)
        
        data1[is.na(overtime), overtime := 0]
        setorder(data1, date)
        data1[, cum := cumsum(overtime), emp_code]
        suppressWarnings(data1[,line_color:= ''])
        data1[, line_color := ifelse(cum <= 20,'black',
                ifelse(cum %between% c(20.01, 40),'green',
                ifelse(cum %between% c(40.01, 49), 'orange', 'red')
                                     ))]
        
        setkey(data1, emp_code)
        
        data1<-data1[(data1[,.(overtime=sum(overtime)),emp_code][
            ,.(emp_code,scales=scale(overtime))
            ])]
        
        
        
        data1[,new_scales:=(scales+mean(cum))/(mean(cum))]
        
        
        data1[,new_week:=weekdays(as.Date(date),abbreviate=TRUE)]
        
        
        data1[new_week %in% c('Sun','Sat'), new_overtime:= overtime * 2]
        
        data1[(!(new_week %in% c('Sun','Sat')))
              , new_overtime:= ifelse(overtime>4,overtime * 2,
                                      overtime
              )]
        
        
        data1[,new_overtime:=(new_overtime * new_scales)]
        
        
        data1[,moving_avg:= TTR::WMA(new_overtime,9),emp_code]
        
        data1[is.na(moving_avg),moving_avg:=0]
        
        data1[,moving_avg:=round(moving_avg,2)]

        data1

    })
    
    # sql_data ----------------------------------------------------------------
    
    output$sql_data <- renderDT(
        # put CSV, XLS, and PDF in a collection
        fulldata()[1:1000, ],
        extensions = 'Buttons',
        filter = 'top',
        options = list(
            dom = 'TlBfrtip',
            scrollX = TRUE,
            scrollY = 400,
            scrollCollapse = TRUE,
            lengthMenu = c(5, 10, 50, 100, 200),
            pageLength = 10,
            buttons =
                list(
                    'colvis',
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    )
                )
            )
    )
    
    
    
    # daterange_filter --------------------------------------------------------
    
    output$date_range_filter <- renderUI({
        dateRangeInput(
            ns('sql_date_filter'),
            label = 'Please select the Date Range',
            end = '2018-03-31',
            start = '2018-01-01'
        )
    })
    
    
    
    # organization_filter -----------------------------------------------------
    
    
    output$org_select_filter <- renderUI({
        selectInput(
            ns('sql_org_filter'),
            label = 'Choose an Organization',
            multiple = TRUE,
            selectize = TRUE,
            choices = c('honda', 'nkid'),
            selected = 'honda'
        )
    })
    
    
    
    # download_file -----------------------------------------------------------
    
    output$download_button <- downloadHandler(
        filename = function() {
            "Main_Employee.csv"
        },
        content = function(file) {
            fwrite(fulldata(), file, row.names = FALSE)
        }
    )
    

    return(fulldata)
    
# endServer ----------------------------------------------------------------
    
}