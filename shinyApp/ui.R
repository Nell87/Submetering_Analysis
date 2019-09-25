
#### 0. LOAD PACKAGES ####
library(readxl)
library(shiny)
# library(data.table)
#library(bsts)
library(readr)
library(shinydashboard)
library(xts)
library(highcharter)
library(dplyr)
library(lubridate)
library(plotly)
library(forecast)


#### 1. LOAD OBJECTS ####
# Prepared data
data<-readRDS("data.rds")
prices <- read_excel("Electricity_prices.xlsx")
prices <-data.frame(prices)
prices$Month<-month(prices$Month, label=TRUE, abbr=FALSE)

# Group by day, week, month, year
vars<-c("ActiveEnergy", "Kitchen", "Laundry", "EWAC")

data_bydays<-data%>%
  group_by(Year=year(DateTime), Month=month(DateTime, label=TRUE, abbr=FALSE),
           Week=week(DateTime),Day=day(DateTime)) %>%
  summarise_at(vars(vars), funs(sum)) %>%
  mutate(Date=ymd(paste(Year, Month, Day))) %>% 
  ungroup()

data_bydays<-merge(data_bydays, prices,  by=c("Year","Month"))

data_byweeks<-data_bydays%>%
  group_by(Year, Month, Week)%>%
  summarise_at(vars(vars), funs(sum)) %>% 
  ungroup()

data_bymonths<-data%>%
  group_by(Year=year(DateTime), Month=month(DateTime, label=TRUE, abbr=FALSE))%>%
  summarise_at(vars(vars), funs(sum))%>% 
  ungroup()

data_byyears<-data%>%
  group_by(Year=year(DateTime))%>%
  summarise_at(vars(vars), funs(sum))%>% 
  ungroup()

# Create time series
# tsYear<-ts(data_byyears[vars],frequency=1, start=2007, end=2010)
tsMonth<-ts(data_bymonths["ActiveEnergy"],frequency =  12, start=c(2007,1),  end=c(2010,11))
# tsWeek<-ts(data_byweeks[vars], frequency = 52, start=c(2007,1),    end=c(2010,48))
# tsDay<-ts(data_bydays[c(vars, "Price")], frequency = 356, start=c(2007,1),    end=c(2010,300))
rm(vars)

#### 2.  USER INTERFACE ####
ui <- dashboardPage(
  dashboardHeader(title = "Energy Monitor",
                  titleWidth=400
  ),
  ## Sidebar content
  dashboardSidebar(
    
    sidebarMenu(id="menu",
                menuItem("Customer Layout", tabName = "customer", icon =icon("bar-chart-o"),
                         startExpanded = TRUE, 
                         menuSubItem("consumption", tabName="consumption", selected = TRUE),
                         selectInput(inputId = "cust_var", label = "Variable",
                                     choices=c("ActiveEnergy","Kitchen", "Laundry", 
                                               "EWAC")),
                         selectInput("Select Year",inputId="cust_year", choices=c(2008, 2009, 2010)),
                         selectInput("Select Month",inputId="cust_month", choices=unique(data_bymonths$Month))
                ),
                
                menuItem("Analyst Layout", tabName = "analyst", icon =icon("line-chart"), 
                         startExpanded = FALSE,
                         # selectInput(inputId = "VariableAnalyst", label = "Variable",
                         #             choices=c("ActiveEnergy", "Kitchen", "Laundry", "EWAC")),
                         menuSubItem("Predictions", tabName = "cust_predictions")

                )
    )
  ),
  
  dashboardBody(tabItems(
    # First Tab Content
    tabItem(tabName = "consumption",
            
            # FIRST ROW
            fluidRow(
              column(width = 6,
                    box(plotlyOutput("customer_plot"), width = NULL)
              ),
              
              column(width=3,
                     infoBoxOutput("box_total_money",width=12),
                     infoBoxOutput("box_comp_lastmonth",width=12)
              ),
              
              column(width=3,
                     infoBoxOutput("box_total_energy", width=12),
                     infoBoxOutput("box_comp_lastyear",width=12)
                     
                     
              )
                     
            ),
            
            #SECOND ROW
            fluidRow(

              highchartOutput("cust_all")
            )
            
    ),
    
    tabItem(tabName = "cust_predictions",
            
            column(width=8,          
              highchartOutput("analyst_plot")),
            
            column(width=4,
              textOutput("analyst_text")
            )

    

    )
  )
  
  )
)

