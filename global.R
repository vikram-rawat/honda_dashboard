# Use ID like This
#
#
#          userid   password       date_inserted    role
# 1: system_admin GreyValley 2018-05-28 21:08:52   Admin
# 2:      visitor  simple123 2018-05-29 12:24:04 Visitor
# 3:       viewer  simple123 2018-07-11 14:31:42 Visitor

# library -----------------------------------------------------------------

library("data.table")
library("DBI")
library("tidyverse")
library("shinyjs")
library("dbplyr")
library("DT")
library("flexdashboard")
library("lubridate")
library("plotly")
library("pool")
library("RSQLite")
library("safer")
library("shiny")
library("shinyBS")
library("shinydashboard")

# setDefaults -------------------------------------------------------------

setDTthreads(4)

# sources -----------------------------------------------------------------

source('www/Modules/Main_app.R')
source('www/Modules/select_data.R')
source('www/Modules/dashboard_module.R')
source('www/Modules/emp_data.R')
source('www/Modules/CRUD_db.R')
source('www/Modules/mod_login.R')
source('www/Modules/mod_SQL_functions.R')
