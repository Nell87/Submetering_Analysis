# SUBMETERING ANALYSIS
Based on the measurements of electric power consumption over four years in a real household located in Sceaux, we’re going to analzye past behaviour and we’re going to forecast future energy consumption.

For this purpose, we're going to use the 
"Individual household electric power consumption Data Set". It covers over four years in a real household located in Sceaux. You can read more about this dataset [here](https://archive.ics.uci.edu/ml/datasets/individual+household+electric+power+consumption).

# TECHNICAL APPROACH
This is only a summary of the process, If you want to know more details, you can read the report in [saramarlop.com](http://saramarlop.com/)

## 1. FIRST STEPS

- **Exploratory analysis**
- **Cleaning and preparing datasets**:
  - Data transformations
  - Missing values imputation
  - Daylight saving
  - Time series creation and decomposition
  
## 2. FEATURE ENGINEERING
It’s about creating new input features from the existing ones (we will use them in our predictions):
- Season
- Weekday
- Unmeasured energy

## 3. FORECASTING & ERROR ANALYSIS 
- We run two different models: ARIMA and Holt-Winters
- We analyzed the errors based 
