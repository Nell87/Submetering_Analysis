
#### 0. LOAD PACKAGES ####
library(readxl)
library(shiny)
# library(data.table)
#library(bsts)
library(readr)
library(shinydashboard)
# library(xts)
# library(highcharter)
library(dplyr)
library(lubridate)
library(plotly)

#### 1. LOAD OBJECTS ####
# Prepared data
data<-readRDS("data.rds")
ListGraphsMonths<- readRDS("ListGraphMonths.rds")
ListGraphsWeeks<- readRDS("ListGraphsWeeks.rds")
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
tsYear<-ts(data_byyears[vars],frequency=1, start=2007, end=2010)
tsMonth<-ts(data_bymonths[vars],frequency =  12, start=c(2007,1),  end=c(2010,11))
tsWeek<-ts(data_byweeks[vars], frequency = 52, start=c(2007,1),    end=c(2010,48))
tsDay<-ts(data_bydays[c(vars, "Price")], frequency = 356, start=c(2007,1),    end=c(2010,300))
rm(vars)

#### 3.  SERVER ####

server <- function(input, output, session) {
  
  # OBJECTS FOR CUSTOMER
  cust_data<-reactive({
    data_bydays %>% filter(Month %in% input$cust_month & Year %in% input$cust_year) %>%
      select(Day, Month, Year, Variable=starts_with(input$cust_var))
  })
  
  cust_energy_thismonth<-reactive({
    data_bymonths %>% filter(Month %in% input$cust_month & Year %in% input$cust_year) %>%
    select(Variable=starts_with(input$cust_var))
  })
  
  # CUSTOMER: TOTAL CONSUMPTION  
  output$customer_plot<-renderPlotly({
    cust_data<-cust_data()
    plot_ly(x=~cust_data$Day, y=~cust_data$Variable, type="bar") %>%
      layout(title = "Total consumption",
             xaxis = list(title = as.character(input$cust_var)),
             yaxis = list(title = "Day"))
      
  })  
  
  # CUSTOMER: BOXES
  output$box_total_energy<-renderInfoBox({
    infoBox(
      "Total Energy", "80%", icon = icon("lightbulb"),
      color = "blue"
    )
  })
    
    output$box_total_money<-renderInfoBox({
      infoBox(
        "Total Money", "80%", icon = icon("euro-sign"),
        color = "blue"
      )
    
  })
    
    output$box_comp_lastmonth<-renderInfoBox({
      cust_energy_thismonth<-cust_energy_thismonth()
      
      past<-data_bymonths %>% filter(lead(Month %in% input$cust_month) & Year %in% input$cust_year) %>%
        select(Variable=starts_with(input$cust_var))
      
      color="green"
      if(cust_energy_thismonth>past) color="red"
      
      infoBox(
        "Last Month", "80%", icon = icon("arrow-down"),
        color = color
      )
      
    })
    
    output$box_comp_lastyear<-renderInfoBox({

      infoBox(
        "Last January", "80%", icon = icon("arrow-up"),
        color = "red"
      )
      
    })
  
}

shinyApp(ui, server)


# server <- function(input, output, session) {
#   
#   
#   
#   get.data <- reactive({
#     switch(input$Frequency,
#            "Year" = data_byyear,
#            "Month" = data_bymonths,
#            "Week" = data_byweeks,
#            "Day of Week" = Data_ByWDay,
#            "Day" = data_bydays,
#            "Hour" = Data_ByHour)
#   })
#   
#   get.ts <- reactive({
#     switch(input$FrequencyAnalyst,
#            "Year" = tsYear,
#            "Month" = tsMonth,
#            "Week" = tsWeek,
#            "Day" = tsDay)
#   })
#   
#   # get.variable <- reactive({
#   #   switch(input$Variable,
#   #          "ActiveEnergy" = ActiveEnergy,
#   #          "ReactiveEnergy" = ReactiveEnergy,
#   #          "Kitchen" = Kitchen,
#   #          "Laundry" = Laundry,
#   #          "EWAC" = EWAC,
#   #          "OtherRooms" = OtherRooms)
#   # })
#   
#   #### filtro el data set (yo le llamo df en la query de dplyr) que tu has creado con el if statement por las fechas de start y end que he seleccionado
#   #en el calendario
#   
#   filteredData <- reactive({
#     get.data() %>% dplyr::filter(Date>=input$dateRange[1] & Date<input$dateRange[2])
#     
#   })
#   
#   
#   output$table <- renderHighchart ({
#     
#     test<-filteredData()
#     test$ActiveEnergy<-round(test$ActiveEnergy,2)
#     
#     highchart() %>% 
#       hc_chart(type = "column") %>% 
#       hc_title(text = "Energy consumption (Watt-hour)", align="center") %>% 
#       hc_xAxis(categories = test$Time) %>% 
#       hc_add_series(data = test$ActiveEnergy,
#                     name = "Active Enerhy")%>%
#       hc_add_series(data = test$Kitchen,
#                     name = "Kitchen")%>% 
#       hc_add_series(data = test$Laundry,
#                     name = "LAundry")%>%
#       hc_add_series(data = test$EWAC,
#                     name = "Water Heater & A/C")%>% 
#       hc_add_theme(hc_theme_538())
#     
#   })
#   
#   output$table2 <- renderHighchart ({
#     dfts <- xts(round(data_bydays$ActiveEnergy,2), zoo::as.Date( data_bydays$Date, format='%m/%d/%Y'))
#     hchart(dfts) %>%
#       hc_title(text = "Energy consumption (Watt-hour)", align="center") %>%
#       hc_add_theme(hc_theme_538())
#   })
#   
#   output$BoxReactive <- renderInfoBox({
#     ts<-filteredData()
#     
#     infoBox(
#       "Reactive", paste0(round(mean(ts$ReactiveEnergy, na.rm = TRUE),2), "Kw"), icon = icon("list"),
#       color = "purple", fill = TRUE
#     )
#   })
#   
#   output$BoxVoltage <- renderInfoBox({
#     ts<-filteredData()
#     
#     infoBox(
#       "Voltage", paste0(round(mean(ts$Voltage, na.rm = TRUE),2), "Volts"), icon = icon("list"),
#       color = "maroon", fill = TRUE
#     )
#   })
#   output$BoxIntensity <- renderInfoBox({
#     ts<-filteredData()
#     
#     infoBox(
#       "Intensity", paste0(round(mean(ts$Intensity, na.rm = TRUE),2), "Amps"), icon = icon("list"),
#       color = "blue", fill = TRUE
#     )
#   })
#   output$BoxPrice <- renderInfoBox({
#     ts<-filteredData()
#     
#     infoBox(
#       "Price", paste0(round(sum((ts$Price * ts$ActiveEnergy)/1000, na.rm = TRUE),2), "Euros"), icon = icon("list"),
#       color = "green", fill = TRUE
#     )
#   })
#   
#   output$TimeSeries<-renderPlot({
#     
#     VariableTS<-as.character(input$VariableAnalyst)
#     
#     # Filter Variables by Year
#     if(input$FrequencyAnalyst=="Year"){
#       
#       plot.ts(tsYear[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Years")
#     }
#     
#     # Filter Variables by Month
#     if(input$FrequencyAnalyst=="Month"){
#       
#       plot.ts(tsMonth[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Months")
#     }
#     
#     # Filter Variables by Week
#     if(input$FrequencyAnalyst=="Week"){
#       
#       plot.ts(tsWeek[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Weeks")
#     }    
#     
#     # Filter Variables by Day
#     if(input$FrequencyAnalyst=="Day"){
#       
#       plot.ts(tsDay[, VariableTS],  ylab = paste0(VariableTS, "(Wh)"), xlab="Per Days")
#     }})
#   
#   output$TimeSeriesAll<-renderPlot({
#     
#     # Filter Variables by Year
#     if (input$FrequencyAnalyst=="Year"){
#       
#       autoplot(tsYear[, c(3,4,5,6,7,8)], facets = FALSE, ylab = "Energy (Wh)", xlab="Years")
#       
#     }
#     
#     # Filter Variables by Month
#     else if(input$FrequencyAnalyst=="Month"){
#       
#       autoplot(tsMonth[, c(4,5,6,7,8,9)], facets = FALSE, ylab = "Energy (Wh)", xlab="Months")
#     }
#     
#     # Filter Variables by Week
#     else if(input$FrequencyAnalyst=="Week"){
#       
#       autoplot(tsWeek[, c(4,5,6,7,8,9)], facets = FALSE, ylab = "Energy (Wh)", xlab="Weeks")
#     }    
#     
#     # Filter Variables by Day
#     else if(input$FrequencyAnalyst=="Day"){
#       
#       autoplot(tsDay[, c(5,6,7,8,9,10)], facets = FALSE, ylab = "Energy (Wh)", xlab="Days")
#     }})
#   
#   output$PlotModels<-renderPlot({
#     
#     if(input$FrequencyAnalyst=="Month"){
#       
#       if(input$VariableAnalyst=="ActiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonths[[1]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonths[[1]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonths[[1]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsMonths[[1]][[4]][[1]])
#                              ))))}   
#       
#       if(input$VariableAnalyst=="ReactiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonths[[2]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonths[[2]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonths[[2]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsMonths[[2]][[4]][[1]])
#                              ))))}         
#       
#       if(input$VariableAnalyst=="Kitchen"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonths[[3]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonths[[3]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonths[[3]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsMonths[[3]][[4]][[1]])
#                              ))))}         
#       
#       if(input$VariableAnalyst=="Laundry"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonths[[4]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonths[[4]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonths[[4]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsMonths[[4]][[4]][[1]])
#                              ))))}         
#       
#       if(input$VariableAnalyst=="EWAC"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsMonths[[5]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsMonths[[5]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsMonths[[5]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsMonths[[5]][[4]][[1]])
#                              ))))   
#       }}
#     
#     if (input$FrequencyAnalyst=="Week"){
#       
#       if(input$VariableAnalyst=="ActiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[1]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[1]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[1]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[1]][[4]][[1]])
#                              ))))} 
#       
#       
#       if(input$VariableAnalyst=="ReactiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[2]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[2]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[2]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[2]][[4]][[1]])
#                              ))))}         
#       
#       if(input$VariableAnalyst=="Kitchen"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[3]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[3]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[3]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[3]][[4]][[1]])
#                              ))))}         
#       
#       if(input$VariableAnalyst=="Laundry"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[4]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[4]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[4]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[4]][[4]][[1]])
#                              ))))}         
#       
#       if(input$VariableAnalyst=="EWAC"){
#         
#         ifelse(input$SelectModel=="Naive",  plot(ListGraphsWeeks[[5]][[1]][[1]]),
#                ifelse(input$SelectModel== "SNaive",plot(ListGraphsWeeks[[5]][[2]][[1]]),
#                       ifelse(input$SelectModel=="HoltWinters",plot(ListGraphsWeeks[[5]][[3]][[1]]),
#                              ifelse(input$SelectModel=="Arima",plot(ListGraphsWeeks[[5]][[4]][[1]])
#                              ))))   
#       }}
#     
#   })
#   
#   output$ErrorModel<-renderTable({
#     
#     if(input$FrequencyAnalyst=="Month"){
#       
#       if(input$VariableAnalyst=="ActiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive",  ListGraphsMonths[[1]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsMonths[[1]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsMonths[[1]][[3]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsMonths[[1]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="ReactiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsMonths[[2]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsMonths[[2]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsMonths[[2]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsMonths[[2]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="Kitchen"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsMonths[[3]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsMonths[[3]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsMonths[[3]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsMonths[[3]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="Laundry"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsMonths[[4]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsMonths[[4]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsMonths[[4]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsMonths[[4]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="EWAC"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsMonths[[5]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsMonths[[5]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsMonths[[5]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsMonths[[5]][[4]][2]
#                              ))))}
#     }
#     
#     else if(input$FrequencyAnalyst=="Week"){
#       
#       if(input$VariableAnalyst=="ActiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive",  ListGraphsWeeks[[1]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[1]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[1]][[3]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[1]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="ReactiveEnergy"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[2]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[2]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[2]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[2]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="Kitchen"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[3]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[3]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[3]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[3]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="Laundry"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[4]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[4]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[4]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[4]][[4]][2]
#                              ))))}
#       
#       else if(input$VariableAnalyst=="EWAC"){
#         
#         ifelse(input$SelectModel=="Naive", ListGraphsWeeks[[5]][[1]][2],
#                ifelse(input$SelectModel== "SNaive",ListGraphsWeeks[[5]][[2]][2],
#                       ifelse(input$SelectModel=="HoltWinters",ListGraphsWeeks[[5]][[2]][2],
#                              ifelse(input$SelectModel=="Arima",ListGraphsWeeks[[5]][[4]][2]
#                              ))))}
#       
#     }
#   })
#   
#   #   output$readme<-renderImage({
#   #     outfile <- tempfile(fileext = "team.png")
#   #   })
#   
#   output$Insights<-renderText({
#     "Insights"
#   })
#   
#   
# }

