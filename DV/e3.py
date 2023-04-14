#NYC foursquare checkins

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(knitr)
fsnyc <- read_csv("../input/dataset_TSMC2014_NYC.csv")
head(fsnyc)


head(table(fsnyc$venueCategory))
head(table(fsnyc$longitude))
head(table(fsnyc$venueId))
head(table(fsnyc$userId))

#by usr-location
fsnyc%>%
  select(userId,longitude,latitude)%>%
  group_by(userId)->fsnyc_by_usr
print("Checkin's by User ID")

kable(fsnyc_by_usr[1:10,])
#by venue-location
fsnyc%>%
  select(venueCategory,longitude,latitude)%>%
  group_by(venueCategory)->fsnyc_by_cat

print("Checkin's by category")

kable(fsnyc_by_cat[1:20,])
print("dimensions:")
dim(fsnyc_by_cat)

#drawing random sample of 5000
index <- sample(1:nrow(fsnyc_by_cat), 5000)

nu_fsnyc_cat<-fsnyc_by_cat[index,]

head(nu_fsnyc_cat)

t<-as.data.frame(table(fsnyc$venueCategory))

t<- t[order(-(t$Freq)),] 
colnames(t)[1]<-'Category'
colnames(t)[2]<-'Freq'
print("Top 20 Checkin Spots were")
kable(t[1:20,])

plot(t$Category,t$Freq)