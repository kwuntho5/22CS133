##Setup
```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)

nyc <- read.csv('../input/dataset_TSMC2014_NYC.csv', header = TRUE)
tky <- read.csv('../input/dataset_TSMC2014_TKY.csv', header = TRUE)
```

##Preparing the data
```{r, message=FALSE, warning=FALSE}
# Fix all the dates and times
nyc$dateTime <- as.POSIXct(strptime(gsub("\\+0000 ", "", nyc$utcTimestamp), '%c', tz='US/Eastern') + hours(nyc$timezoneOffset/60))
nyc$date <- as_date(nyc$dateTime)
nyc$time <- strftime(nyc$dateTime, '%H:%M:%S', tz='US/Eastern')

tky$dateTime <- as.POSIXct(strptime(gsub("\\+0000 ", "", tky$utcTimestamp), '%c', tz='Japan')) + hours(tky$timezoneOffset/60)
tky$date <- as_date(tky$dateTime)
tky$time <- strftime(tky$dateTime, '%H:%M:%S', tz='Japan')

# Organise times into different periods
library(chron)
breaks <- c(0, 5, 11, 13, 18, 24) / 24
labels <- c("Early Morning", "Morning", "Lunch", "Afternoon", "Evening")
nyc$timePeriod <- cut(times(nyc$time), breaks, labels, include.lowest = TRUE)
tky$timePeriod <- cut(times(tky$time), breaks, labels, include.lowest = TRUE)

# Summarise numbers across time periods and venues
nyc.venues <- nyc %>% 
    group_by(venueCategory, timePeriod) %>% 
    summarise(count = n()) %>%
    arrange(desc(count))

tky.venues <- tky %>% 
  group_by(venueCategory, timePeriod) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

#Grab the top 30 venues
nyc.top30 <- nyc %>% 
  group_by(venueCategory) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(30)

tky.top30 <-tky %>% 
  group_by(venueCategory) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(30)
```

##Plotting the data
```{r}
nyc.venues[nyc.venues$venueCategory %in% nyc.top30$venueCategory,] %>%
ggplot(aes(x=reorder(venueCategory, count), y=count, fill=timePeriod)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_grey() +
  labs(title="Chilling at the bar in NYC", x="", y="")


tky.venues[tky.venues$venueCategory %in% tky.top30$venueCategory,] %>%
ggplot(aes(x=reorder(venueCategory, count), y=count, fill=timePeriod)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_grey() +
  labs(title="Riding the train in Tokyo", x="", y="")

``` 




```{r}
# Removing train and subway to see more clearly
tky.venues[tky.venues$venueCategory %in% tky.top30[3:30,]$venueCategory,] %>%
ggplot(aes(x=reorder(venueCategory, count), y=count, fill=timePeriod)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_grey() +
  labs(title="Chowing down noodles in Tokyo", x="", y="")
```

#Location hotspots
```{r, eval=FALSE, include=TRUE}
map1 <- ggmap(get_map("manhattan", zoom=10, maptype="terrain"))
map1 + geom_point(data=nyc[1:50000,], mapping = aes(y=latitude, x=longitude, col=timePeriod, alpha=0.5))


map2 <- ggmap(get_map("manhattan", zoom=12, maptype="toner"))
map2 + stat_density2d(data=nyc[1:10000,], 
                   mapping = aes(y=latitude, x=longitude, fill=..level.., alpha=..level..), 
                   geom="polygon", bins=100) +
                   scale_fill_gradient(low="blue", high="red") +
                   theme(legend.position = "none")

```

![Map1](http://puu.sh/vGXtV/9366f86ede.jpg)
![Map2](http://puu.sh/vGXt4/5a2c394169.jpg)

and now lets check out Tokyo

```{r, eval=FALSE, include=TRUE}
map3 <- ggmap(get_map("tokyo", zoom=10, maptype="terrain"))
map3 + geom_point(data=tky[1:50000,], mapping = aes(y=latitude, x=longitude, col=timePeriod, alpha=0.5))

map4 <- ggmap(get_map("tokyo", zoom=11, maptype="toner"))
map4 + stat_density2d(data=tky[1:10000,], 
                      mapping = aes(y=latitude, x=longitude, fill=..level.., alpha=..level..), 
                      geom="polygon", bins=100) +
                      scale_fill_gradient(low="blue", high="red") +
                      theme(legend.position = "none")
```