setwd("C:/SEM 3.1/GOOGLE CERTIFICATE/course 2/CYCLIST_CASE_STUDY_1/DATASETS")
 tripdata_202004 <- read.csv("202004-divvy-tripdata.csv")
 View(tripdata_202004)
 tripdata_202005 <- read.csv("202005-divvy-tripdata.csv")
 tripdata_202006 <- read.csv("202006-divvy-tripdata.csv")
 tripdata_202007 <- read.csv("202007-divvy-tripdata.csv")
 tripdata_202008 <- read.csv("202008-divvy-tripdata.csv")
 tripdata_202009 <- read.csv("202009-divvy-tripdata.csv")
 tripdata_202010 <- read.csv("202010-divvy-tripdata.csv")
 tripdata_202011 <- read.csv("202011-divvy-tripdata.csv")
tripdata_202012 <- read.csv("202012-divvy-tripdata.csv")
 tripdata_202101<- read.csv("202101-divvy-tripdata.csv")
 tripdata_202102<- read.csv("202102-divvy-tripdata.csv")
 tripdata_202103<- read.csv("202103-divvy-tripdata.csv")
 tripdata_202104<- read.csv("202105-divvy-tripdata.csv")
 tripdata_202106 <- read.csv("202106-divvy-tripdata.csv")
 tripdata_202104<- read.csv("202104-divvy-tripdata.csv")
 tripdata_202105<- read.csv("202105-divvy-tripdata.csv")
 tripdata_202107<- read.csv("202107-divvy-tripdata.csv")


View(tripdata_202107)
colnames(tripdata_202004)
colnames(tripdata_202005)
colnames(tripdata_202006)
colnames(tripdata_202107)
colnames(tripdata_202008)
colnames(tripdata_202009)
colnames(tripdata_202010)
colnames(tripdata_202011)
colnames(tripdata_202012)
colnames(tripdata_202101)
colnames(tripdata_202102)
colnames(tripdata_202103)
colnames(tripdata_202104)
colnames(tripdata_202105)
colnames(tripdata_202106)
colnames(tripdata_202107)

str(tripdata_202004)
str(tripdata_202005)
str(tripdata_202006)
str(tripdata_202107)
str(tripdata_202008)
str(tripdata_202009)
str(tripdata_202010)
str(tripdata_202011)
str(tripdata_202012)
str(tripdata_202101)
str(tripdata_202102)
str(tripdata_202103)
str(tripdata_202104)
str(tripdata_202105)
str(tripdata_202106)
str(tripdata_202107)
tripdata_202004 <- tripdata_202004 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202005 <- tripdata_202005 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202006 <- tripdata_202006 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202008 <- tripdata_202008 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202009 <- tripdata_202009 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202010 <- tripdata_202010 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202011 <- tripdata_202011 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
all_trips <- bind_rows(tripdata_202004,tripdata_202005,tripdata_202006,tripdata_202107,tripdata_202008,tripdata_202009,tripdata_202010,tripdata_202011,tripdata_202012,
                       tripdata_202101,tripdata_202102,tripdata_202103,tripdata_202104,tripdata_202105,tripdata_202106,tripdata_202107)
str(all_trips)
all_trips <- bind_rows(tripdata_202004,tripdata_202005,tripdata_202006,tripdata_202107,tripdata_202008,tripdata_202009,tripdata_202010,tripdata_202011,tripdata_202012,
tripdata_202101,tripdata_202102,tripdata_202103,tripdata_202104,tripdata_202105,tripdata_202106,tripdata_202107)
str(all_trips)
all_trips[['started_at']] <- ymd_hms(all_trips[['started_at']])
all_trips[['ended_at']] <- ymd_hms(all_trips[['ended_at']])
str(all_trips)
all_trips <- all_trips %>%
  select(-c(start_lat:end_lng))
glimpse(all_trips)
all_trips <- all_trips %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)
glimpse(all_trips)
all_trips$day_of_the_week <- format(as.Date(all_trips$start_time),'%a')
all_trips$month <- format(as.Date(all_trips$start_time),'%b_%y')
all_trips$time <- format(all_trips$start_time, format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")
all_trips$trip_duration <- (as.double(difftime(all_trips$end_time, all_trips$start_time)))/60
glimpse(all_trips)




# checking for trip lengths less than 0
nrow(subset(all_trips,trip_duration < 0))



 #library(tidyverse)
 #library(ggplot2)
 #library(lubridate)
# library(dplyr)
#library(readr)
# library(janitor)
 #library(data.table)
 #library(tidyr)

#remove test rides
all_trips_v2<- all_trips_v2[!((all_trips_v2$start_station_name %like% "TEST" | all_trips_v2$start_station_name %like% "test")),]


#check dataframe
glimpse(all_trips_v2)


#checking for testrides that were made by company for quality checks
nrow(subset(all_trips, start_station_name %like% "TEST"))
nrow(subset(all_trips, start_station_name %like% "test"))
nrow(subset(all_trips, start_station_name %like% "Test"))

# remove negative trip durations 
all_trips_v2 <- all_trips[!(all_trips$trip_duration < 0),]

#remove test rides
all_trips_v2<- all_trips_v2[!((all_trips_v2$start_station_name %like% "TEST" | all_trips_v2$start_station_name %like% "test")),]

#check dataframe
glimpse(all_trips_v2)

# checking count of distinct values
table(all_trips_v2$customer_type)


#aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips_v2, sum), c("customer_type", "total_trip_duration(mins)"))


# statictical summary of trip_duration for all trips
summary(all_trips_v2$trip_duration)
#statistical summary of trip_duration by customer_type
all_trips_v2 %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))
# fix the order for the day_of_the_week and month variable so that they show up 
# in the same sequence in output tables and visualizations

all_trips_v2$day_of_the_week <- ordered(all_trips_v2$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Apr_20", "May_20", "Jun_20", "Jul_20", "Aug_20", "Sep_20", "Oct_20",
                                                           "Nov_20", "Dec_20", "Jan_21", "Feb_21", "Mar_21", 
                                                           "Apr_21", "May_21", "Jun_21", "Jul_21"))
all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))
# Visualisation 
all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
 # arrange(customer_type, day_of_the_week)
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5))+ 
 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
 # Average number of trips by customer type and month
unique(all_trips$month)

all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))
# Visualisation 
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
 # Visualizaton of average trip duration by customer type Vs. month
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))
# Visualizaton of bike demand over 24 hr period (a day)
all_trips_v2 %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")
 # Visualizaton of ride type Vs. number of trips by customer type
all_trips_v2 %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")
  # Creating a csv file of the clean data for futher analysis or visualizations in other tools like SQL, Tableau, Power BI, etc.
clean_data <- aggregate(all_trips_v2$trip_duration ~ all_trips_v2$customer_type + all_trips_v2$day_of_the_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)  
