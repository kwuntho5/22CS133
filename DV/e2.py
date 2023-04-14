# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 
​
library(data.table) #reading in the data
library(dplyr) #dataframe manipulation
library(ggplot2) #viz
library(ranger) #the random forest implementation
library(plotly) #3D plotting
library(tidyr) #dataframe manipulation
library(FNN) #k nearest neighbors algorithm
library(xgboost)
library(tidyverse)
library(lubridate)
​
nyc <- fread("../input/dataset_TSMC2014_NYC.csv")
tky <- fread("../input/dataset_TSMC2014_TKY.csv")

head(nyc)

ggplot(nyc, aes(latitude, longitude )) +
    geom_point(aes(color = venueId)) + 
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("New York Check-ins colored by Venue_Id")

ggplot(tky, aes(latitude, longitude )) +
    geom_point(aes(color = venueId)) + 
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("Tokyo Check-ins colored by Venue_Id")

# Fix all the dates and times
nyc$dateTime <- as.POSIXct(strptime(gsub("\\+0000 ", "", nyc$utcTimestamp), '%c', tz='US/Eastern') + hours(nyc$timezoneOffset/60))
nyc$date <- as_date(nyc$dateTime)
nyc$time <- strftime(nyc$dateTime, '%H:%M:%S', tz='US/Eastern')
nyc$day<-as.factor(substr(nyc$utcTimestamp,1,3))

tky$dateTime <- as.POSIXct(strptime(gsub("\\+0000 ", "", tky$utcTimestamp), '%c', tz='Japan')) + hours(tky$timezoneOffset/60)
tky$date <- as_date(tky$dateTime)
tky$time <- strftime(tky$dateTime, '%H:%M:%S', tz='Japan')
tky$day<-as.factor(substr(tky$utcTimestamp,1,3))

head(nyc)

data.frame(sapply(nyc,class))[1,]