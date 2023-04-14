```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggmap)
theme_set(theme_few())
```



```{r}
nyc_raw <- read_csv('../input/dataset_TSMC2014_NYC.csv',
                    col_types = cols(
                      utcTimestamp = col_datetime(format = '%.%.%. %b %d %H:%M:%S %z %Y')
                    ))
```


```{r}
nyc <- nyc_raw %>% 
  mutate(time = utcTimestamp + minutes(timezoneOffset)) %>% 
  select(-timezoneOffset, -utcTimestamp)
```



```{r}
dat <- bind_rows(list(`New York` = nyc), .id = 'city')
```

```{r}
daily <- dat %>%
  group_by(date = make_date(year(time), month(time), day(time)),
           weekday = wday(date, label = TRUE),
           city) %>% 
  summarize(count = n())

ggplot(daily, aes(date, count, color = city)) +
geom_line(alpha = 0.7) +
geom_smooth(se = FALSE) +
scale_color_brewer(palette = 'Set1') +
labs(title = 'Daily check-ins decrease over time',
     x = 'Date')
```


---------------------------------


```{r, eval = F}
nycmap <- get_map(location = 'manhattan', color = 'bw')
ggmap(nycmap, base_layer = ggplot(nyc, aes(longitude, latitude))) +
coord_cartesian() +
geom_hex(bins = 100, alpha = 0.7) +
scale_fill_distiller(palette = 'YlGnBu', values = c(0, 0.2, 1), limits = c(0, 5000))
```


```{r, eval = F}
nycmap_zoomed <- get_map(location = c(lon = -73.971776, lat = 40.772774), zoom = 12, color = 'bw')
ggmap(nycmap_zoomed, 
      base_layer = ggplot(filter(nyc, 
                                 latitude > 40.695, 
                                 latitude < 40.85, 
                                 longitude > -74.05, 
                                 longitude < -73.9), 
                          aes(longitude, latitude))) +
coord_cartesian() +
geom_hex(bins = 100, alpha = 0.7) +
scale_fill_distiller(palette = 'YlGnBu', limits = c(0, 800), values = c(0, 0.2, 1))
```


# Patterns in check-in behavior


```{r}
summarize_by_location <- function(df, location = NULL) {
  if (!is.null(location)) df <- filter(df, city %in% location)
  else df <- group_by(df, city)
  df <- df %>% 
    group_by(datetime = make_datetime(year(time), month(time), day(time), hour(time)),
             week = week(time),
             add = TRUE) %>% 
    summarize(count = n()) %>% 
    mutate(weeklytime = make_datetime(year = 1978,
                                      day = wday(as_date(datetime)),
                                      hour = hour(datetime))) %>% 
    ungroup()
  if (is.null(location)) df <- group_by(df, city)
  df %>% 
    group_by(weeklytime, add = TRUE) %>% 
    summarize(weeklycount = mean(count))
}

summarize_by_venue <- function(df, venue = NULL) {
  if (!is.null(venue)) df <- filter(df, venueCategory %in% venue)
  df <- df %>% 
    group_by(venueCategory,
             datetime = make_datetime(year(time), month(time), day(time), hour(time)),
             week = week(time)) %>% 
    summarize(count = n()) %>% 
    mutate(weeklytime = make_datetime(year = 1978,
                                      day = wday(as_date(datetime)),
                                      hour = hour(datetime))) %>% 
    ungroup()
  if (!is.null(venue)) df <- group_by(df, venueCategory)
  df %>% 
    group_by(weeklytime, add = TRUE) %>% 
    summarize(weeklycount = mean(count))
}

weekly_by_hour <- function(data, location = NULL, venue = NULL) {
  if (is.null(venue)) return(summarize_by_location(data, location))
  if (is.null(location)) return(summarize_by_venue(data, venue))
  data %>% 
    filter(venueCategory %in% venue,
           city %in% location) %>% 
    group_by(venueCategory,
             city,
             datetime = make_datetime(year(time), month(time), day(time), hour(time)),
             week = week(time)) %>% 
    summarize(count = n()) %>% 
    mutate(weeklytime = make_datetime(year = 1978,
                                      day = wday(as_date(datetime)),
                                      hour = hour(datetime))) %>% 
    group_by(weeklytime, city, venueCategory) %>% 
    summarize(weeklycount = mean(count))
}
```

```{r, fig.width = 10}
dat %>% 
  weekly_by_hour() %>% 
  ggplot(aes(weeklytime, weeklycount, color = city)) +
  geom_line() +
  scale_x_datetime(date_labels = '%a', date_breaks = '1 day') +
  scale_color_brewer(palette = 'Set1') +
  labs(title = 'Average Hourly Check-ins over a Typical Week',
       subtitle = 'Very different check-in patterns on weekdays vs weekends',
       x = 'Day',
       y = 'Check-in count')
```


```{r}
hourly2 <- nyc %>% 
  group_by(weekday = wday(time, label = TRUE),
           hour = hour(time)) %>% 
  summarize(count = n()) %>% 
  mutate(hourlytime = make_datetime(hour = hour),
         weekend = factor(ifelse(weekday %in% c('Sat', 'Sun'),
                                 as.character(weekday),
                                 'Weekday'))) 

hourly2 %>% 
  filter(weekend == 'Weekday') %>% 
  ggplot(aes(hourlytime, count, group = weekday, color = weekend)) +
  geom_line(size = 0.2) +
  geom_line(data = filter(hourly2, weekend == 'Sat'), size = 1) +
  geom_line(data = filter(hourly2, weekend == 'Sun'), size = 1) +
  scale_x_datetime(date_breaks = '4 hours',
                   date_labels = '%I %p') +
  scale_color_manual(breaks = c('Weekday', 'Sat', 'Sun'),
                     values = c('Weekday' = 'grey50', 'Sat' = '#377EB8', 'Sun' = '#E41A1C')) +
  labs(title = 'Average hourly check-ins by day of week, NYC',
       subtitle = 'Saturdays and Sundays have very different patterns than the work week',
       x = 'Time',
       y = 'Check-ins') +
  theme(legend.title = element_blank())
```

```{r}


# Check-in locations


```{r}
count(nyc, venueCategory, sort = TRUE)
```



```{r}
nyc %>% 
  weekly_by_hour(venue = c('Home (private)', 'Office')) %>% 
  ggplot(aes(weeklytime, weeklycount, color = venueCategory)) +
  geom_line() +
  scale_x_datetime(date_labels = '%a', date_breaks = '1 day') +
  scale_color_brewer(name = 'Venue category', palette = 'Set1') +
  labs(title = 'Average Hourly Check-ins over a Week',
       subtitle = 'New York',
       x = 'Day',
       y = 'Check-in count')
```

```{r}
dat %>% 
  weekly_by_hour(location = c('New York'), venue = 'Office') %>% 
  ggplot(aes(weeklytime, weeklycount, color = city)) +
  geom_line() +
  scale_x_datetime(date_labels = '%a', date_breaks = '1 day') +
  scale_color_brewer(name = 'City', palette = 'Set1') +
  labs(title = 'Average Hourly Check-ins over a Week',
       subtitle = 'Offices',
       x = 'Day',
       y = 'Check-in count')
```


```{r}
dat %>% 
  weekly_by_hour(location = c('New York'), venue = c('Train Station', 'Subway')) %>% 
  ggplot(aes(weeklytime, weeklycount, color = venueCategory)) +
  geom_line() +
  facet_grid(city ~ ., scales = 'free_y') +
  scale_x_datetime(date_labels = '%a', date_breaks = '1 day') +
  scale_color_brewer(name = 'Venue', palette = 'Set1') +
  labs(title = 'Average Public Transportation Check-ins',
       subtitle = 'By city',
       x = 'Day',
       y = 'Check-in count')
```


# Daily Variation in Check-in Counts


```{r}
ggplot(daily, aes(weekday, count, fill = city)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Set1')
```


```{r}
ggplot(daily, aes(date, count, color = city)) +
  geom_line() +
  facet_wrap(~weekday) +
  scale_color_brewer(palette = 'Set1')
```
