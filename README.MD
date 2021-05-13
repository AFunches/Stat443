# Stat 443 Consulting Project - Forecasting PM2.5 for Wanshouxigong District

**Description**<br/>
The objective of this consulting project is to create a model that can accurately predict the daily levels of particulate matter with a diameter less than 2.5 micrometers (PM2.5) and produce a user interface alert system that will allow its user to input values for the independent variables and receive a numeric response for the PM2.5 prediction and indicate if it is at a safe level. The Beijing Multi-Site Air-Quality dataset was extracted from the UC Irvine Machine Learning Repository, it is a multivariate time series dataset that has taken data from different sources like its air-quality from the Beijing Municipal Environmental Monitoring Center and meteorological data from the China Meteorological Administration.

**Data Description**<br/>
The dataset we are provided has 35,064 observations and 18 variables that describe the pollutant gas levels in the Wanshouxigong station. Each observation uses the observation number, weather station, and time related variables like month, day, and year as reference points to organize and separate the recorded observations.
Dataset contains 35,064 observations (hourly data from the years 2013-2017) and 19 variables <br/>

16 numeric variables <br/>
* 11 continuous variables: PM2.5, PM10, SO2, NO2, CO, etc <br/>
* 5 discrete variables: No, year, month, day, hour <br/>
2 categorical variables: wind direction, station  <br/>
1 character variable: timestamp <br/>

CO: carbon monoxide (ug/m^3) <br/>
Day: the number of day (1-30 or 31) <br/>
DEWP: dew point (degree Celsius) <br/>
Hour: hour of the day (1-24) <br/>
Month: month of the year (1-12) <br/>
NO2: nitrogen dioxide  (ug/m^3) <br/>
O3: ozone  (ug/m^3) <br/>
PRES: pressure (hPa) <br/>
RAIN: rain (mm) <br/>
Season: season of the year <br/>
SO2: sulfur dioxide  (ug/m^3) <br/>
Temp: temperature (Celcius) <br/>
Wd: wind direction (N, S, E, W) <br/>
Weekday: day of the week (1- sunday, 7, saturday) <br/>
WSPM: wind speed (m/s) <br/>

**PM2.5 Guidelines**<br/>
Highly Acceptable: 0-35 ug/m^3 <br/>
Acceptable: 35-75 ug/m^3 <br/>
Slightly Unacceptable: 75-150 ug/m^3 <br/>
Unacceptable: 150+ ug/m^3 <br/>





