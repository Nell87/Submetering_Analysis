
#### 0. INCLUDES  --------------------------------------------------------------------

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(rstudioapi, dplyr, lubridate, magrittr, imputeTS, zoo, forecast, ggplot2, zoo)

# Setwd (1º current wd where is the script, then we move back to the 
# general folder)
current_path = getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("..")
rm(current_path)

# Load Data
data<- read.table("./data/household_power_consumption.gz", sep=",", na.strings=c("?", "", " ", "NA"), 
                  header = TRUE, stringsAsFactors = FALSE)

#### A. FIRST CHECKS ---------------------------------------------------------
# Dimension Variables 
dim(data)    # <- 2,075,259 x 10    

names(data)  # <- 1.X     2. Date   3. Time   4. Global_active_power
             # <- 5. Global_reactive_power    6. Voltage
             # <- 7. Global_intensity         8. Sub_metering_1
             # <- 9. Sub_metering_2           10. Sub_metering_3 

# Remove repeated rows in df_datatrain             
data<-distinct(data)  #<- No repeated rows

         
# Missing Values  
sum(is.na(data))    #<-181,853  Many missing values!!

#### B. TRANSFORMATIONS ----------------------------------------------------------
# Transform some variables to factor/numeric/datetime
numeric<-c("Global_active_power","Global_reactive_power","Voltage",
           "Global_intensity", "Sub_metering_1", "Sub_metering_2",
           "Sub_metering_3")

data[,numeric]<-lapply(data[,numeric], as.numeric)
rm(numeric)

# Rename variables
data<-data %>% dplyr::rename(ActiveEnergy=Global_active_power, ReactiveEnergy=Global_reactive_power, 
                             Intensity=Global_intensity, Kitchen=Sub_metering_1, Laundry=Sub_metering_2, 
                             EWAC=Sub_metering_3) 

# We modify the measurements units: Kilowatts to Watts/Hour
data<-data %>% mutate(ActiveEnergy = ActiveEnergy*1000/60,
                      ReactiveEnergy = ReactiveEnergy*1000/60)

# Fix time & date
data$DateTime<-dmy_hms(paste(data$Date, data$Time))
data$Date<-dmy(data$Date)
data$Time<-hms(data$Time)

# Remove x
data %<>% select(-X, -Time)

#### C. MISSING VALUES & DAYLIGHT SAVING   ---------------------

# Daylight saving time__________________________________________________
DST_dates<-data.frame(Start=c( "2007-3-25 02:00:00", "2008-3-30 02:00:00","2009-3-29 02:00:00","2010-3-28 02:00:00"),
                     End=c( "2007-10-28 1:59:00", "2008-10-26 1:59:00","2009-10-25 1:59:00", "2010-10-31 1:59:00") ,stringsAsFactors=FALSE)
DST_dates$Start<-as_datetime(DST_dates$Start)
DST_dates$End<-as_datetime(DST_dates$End)
 
data<-data %>%
  mutate(DateTime = ifelse(data$DateTime %in% unlist(Map(`:`, DST_dates$Start, DST_dates$End)),
                            DateTime +3600, DateTime))
 
data$DateTime<-as_datetime(data$DateTime,origin= "1970-01-01", tz="UTC")

rm(DST_dates)

# Exploring missing values _________________________________________________
#Download the 'calendarHeat' function from revolutionanalytics.com
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
 
calendarHeat(data$Date, data$ActiveEnergy, 
             varname="Missing Data", color="w2b")

# Plot by day with inputeTS package
group_bytime_variables<-c("Date", "ActiveEnergy", "ReactiveEnergy", "Kitchen", "Laundry", "EWAC")

Data_ByDayts<-data[group_bytime_variables] %>%
  group_by(year(Date), week(Date),day(Date)) %>% summarise_all(funs(mean(., na.rm = TRUE))) %>%
  ungroup()

plotNA.distribution(Data_ByDayts$ActiveEnergy)
rm(Data_ByDayts,group_bytime_variables, calendarHeat)

# Group the missing values per type ____________________________________

# We create a table with the missing values
missingdata<-data %>%
  filter(is.na(ActiveEnergy)) %>%
  select(DateTime, ActiveEnergy)

# Let's see how many consecutive values we have
number_na=1
missingdata$number_na<-number_na

for (i in 2:nrow(missingdata)){
  
  if (lag(missingdata$DateTime)[i] == (missingdata$DateTime[i] -60 )){
    
    number_na <- number_na +1
    missingdata$number_na[i]<- number_na
  }
  
  else {
    number_na= 1
    missingdata$number_na[i] <- number_na
    
  }
}

# Replace the missing values depending on the type (<30 o >30) - IN DEVELOPMENT- 
rm(missingdata, i, number_na)

# Filling Missing values ______________________________________________
data<-arrange(data, DateTime)
data <- na.locf(data, maxgap = 60, na.rm=FALSE) # We replace gap <60m with the previous value 
sum(is.na(data$ActiveEnergy))     # 25,979 to  25,691

interp_var<-c("ActiveEnergy", "ReactiveEnergy", "Voltage", "Intensity","Kitchen", "Laundry", "EWAC")
data[,interp_var]<-lapply(data[,interp_var], forecast::na.interp)

sapply(data, function(x)sum(is.na(x)))  # 25,691 to 0
rm(interp_var)

#### D. ADDING SEASON & OTHER VARIABLES -----------------------------------
# Season __________________________________________________________________
winter<-as_date("2008-12-21")
spring <- as_date("2008-03-20")
summer <- as_date("2008-06-21")
fall <- as_date("2008-09-22")

# We remove year 2006
data<-data %>%
  filter(year(DateTime) != 2006)

mydates <- as_date(format(data$Date, "%2008-%m-%d"))

data$Season<-ifelse(mydates >= as_date(spring) & mydates < as_date(summer), "spring",
                    ifelse (mydates >= summer & mydates < fall, "summer",
                            ifelse (mydates >= fall & mydates < winter, "fall",
                                    "winter")))

data<-data %>% mutate(Season= as.factor(Season))
rm(fall, spring, summer, winter, mydates)

# Other variables _________________________________________________________ - IN DEVELOPMENT- 
# Adding day of the week
data$Wday<-lubridate::wday(data$DateTime, label=TRUE, abbr=TRUE)

# We create two new variables: Other_Places (the active energy consumed in other places) 
# and Power_Factor (the efficiency of electric system)
data<-data %>% mutate(OtherRooms = ActiveEnergy - Kitchen - Laundry - EWAC)
data<-data %>% mutate(PowerFactor = ActiveEnergy/(Voltage*Intensity))

#### D. GROUPING & TIME SERIES ---------------------
# Grouping in different granularities _____________________________________
group_bytime_variables<-c("DateTime", "ActiveEnergy", "ReactiveEnergy", 
                          "Kitchen", "Laundry", "EWAC")

data_byyears<-data[group_bytime_variables] %>%
  group_by(Year=year(DateTime)) %>% 
  select(-DateTime) %>%
  summarise_all(funs(sum(.))) %>% 
  ungroup() 

data_bymonths<-data[group_bytime_variables] %>%
  mutate(Year=year(DateTime), Month=month(DateTime)) %>%
  group_by(Year, Month) %>%
  select(-DateTime) %>%
  summarise_all(funs(sum(.))) %>%
  ungroup() 

data_byweeks<-data[group_bytime_variables] %>%
  group_by(Year=year(DateTime), Week=week(DateTime)) %>%
  select(-DateTime) %>%
  summarise_all(funs(sum(.))) %>%
  ungroup() 
  
data_bydays<-data[group_bytime_variables] %>%
  group_by(Year=year(DateTime), Month=month(DateTime),Week=week(DateTime),Day=day(DateTime)) %>% 
  select(-DateTime) %>%
  summarise_all(funs(sum(.))) %>%
  ungroup()

rm(group_bytime_variables)

# TS & MSTS per Month (Active Energy) _______________________________________
tsMonth<-ts(data_bymonths$ActiveEnergy,frequency =  12, start=c(2007,1))
mstsMonth<-msts(data_bymonths$ActiveEnergy,seasonal.periods=c(1,12))

x<-tsMonth %>% stl(s.window="periodic") %>% autoplot() 
mstsMonth %>% mstl(s.window="periodic") %>% autoplot() 

# TS & MSTS per Day (Active Energy) ________________________________________
tsDay<-ts(data_bydays$ActiveEnergy, frequency = 356.25, start=c(2007,1)) 
mstsDay<-msts(data_bydays$ActiveEnergy, seasonal.periods = c(7,365.25))

tsDay %>% stl(s.window="periodic") %>% autoplot() 
mstsDay %>% mstl(s.window="periodic") %>% autoplot() 

#### E. STUDYING ACTIVE PER MONTH ####
# Exploring the main components  -s.windows controls how rapidly the seasonal component can change-
stl_month<-tsMonth %>% stl(s.window="periodic") 
autoplot(stl_month)

# Comparing the variance of each component with respect to the variance of the original series.
# We can see that it's the seasonality what explains most of the variance in the data
apply(stl_month$time.series,2,var)/var(tsMonth)  # seasonal       # trend          # remainder 
                                                 # 0.865040866    # 0.003539877    # 0.115176355 

# We expect to have the same results with the ACF and PACF tests
acf(tsMonth,lag.max = 24)   # we have a clear seasonality 
pacf(tsMonth,lag.max = 24)
Box.test(tsMonth,lag=24, type="Ljung-Box")    #  p < 0.05, so it's not statinary

library(urca)
tsMonth %>% ur.kpss() %>% summary()

##### F. FORECASTING ACTIVE PER MONTH---------------------

# HOLT WINTERS _______________________________________________________________

# Creating train & Test
ActiveMonth_train<-window(tsMonth, end=c(2010,1))
ActiveMonth_test<-window(tsMonth, start=c(2010,2))

ActiveMonth_HW <- HoltWinters(ActiveMonth_train,  seasonal = "additive")
checkresiduals(ActiveMonth_HW)
ActiveMonth_HW_pred <- forecast:::forecast.HoltWinters(ActiveMonth_HW, h=10, level = .95)
mean(ActiveMonth_HW_pred$upper - ActiveMonth_HW_pred$lower)

# ARIMA _____________________________________________________________________
ActiveMonth_ARIMA <- auto.arima(ActiveMonth_train)
checkresiduals(ActiveMonth_ARIMA)
ActiveMonth_ARIMA_pred <- forecast(ActiveMonth_ARIMA, h=10, level = 0.95)
mean(ActiveMonth_ARIMA_pred$upper - ActiveMonth_ARIMA_pred$lower)

# Ploting Models + errors _______________________________________________________________
autoplot(tsMonth, series="Real data") + 
  autolayer(ActiveMonth_HW_pred$mean, series="HW", PI= FALSE) + 
  autolayer(ActiveMonth_ARIMA_pred$mean, PI =FALSE, series = "ARIMA") 


ActiveMonth_HW_Error<-accuracy(ActiveMonth_HW_pred,ActiveMonth_test)
ActiveMonth_Arima_Error<-accuracy(ActiveMonth_ARIMA_pred,ActiveMonth_test)

#### ARIMA (SPECIFING PARAMETERS) (IN PROGRESS)-------------------------------------------------
# It requieres stationaty time series (errors uncorrelated, normally
# distributed with mean = 0 and constant variance) 

# We know that our time series is not stationary. Let's check first if we need a seasonal differencing
nsdiffs(tsMonth)    # <- 1

# Now, let's check if we need a first difference
ndiffs(tsMonth)     # <- 0

# Let's remove the seasonal component
decompose_month<-decompose(tsMonth)
adjust_month<-tsMonth - decompose_month$seasonal
acf(adjust_month)

nsdiffs(adjust_month)    # <- 0
ndiffs(adjust_month)     # <- 0

Box.test(adjust_month,lag=24, type="Ljung-Box")   # It seems stationary

# Another idea is appling a seasonal difference (DON'T KNOW IF IT'S CORRECT. 
# Check what is differences and lag)
acf(diff(tsMonth, lag = 12, differences = 1))

# We split in training and test
ActiveMonth_train_arima<-window(adjust_month, end=c(2010,1))
ActiveMonth_test_arima<-window(adjust_month, start=c(2010,2))

# Let's get the parameters for the ARIMA model. ACF is going to give us 


#### Z. OTHERS  ----------------------------------------------------------

