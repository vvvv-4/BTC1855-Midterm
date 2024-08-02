#1. Run the ETA for trip and weather data for the dataset
#Following codes and steps given by the "datasciencehero" 

#Let's call/install some packages/files first 

library(funModeling) 
library(tidyverse) 
library(Hmisc)

###############Trip Data###################
#We'll start with trip data
#To get started... 

trip.data <- read.csv("trip.csv")

#To get a brief idea of what we are working with 
#We have in trip data:
#326339 observations with 11 columns
glimpse(trip.data)

#From status, we can see that there are no NA in this dataset. 
#There are some zeros, 50 empty cells in the zipcode column. 
print(status(trip.data))

#We find San Francisco Caltrain is the most frequent start and end station. 
#There are 74 station names but 70 station ids. This is weird and should be further investigated
freq(trip.data$start_station_name) 
freq(trip.data$end_station_name) 

#Most users are suscribers
freq(trip.data$subscription_type) 

plot(freq(trip.data))
print(profiling_num(trip.data))

plot_num(trip.data)
#This is not as meaningful as id, start station, end station, bike_id does not mean much 
#Duration is what we could look as instead

hist(trip.data$duration, col = 'skyblue3')
#Histogram very stretched out on the x-axis, suggest some extreme values. 

##############Weather data################
#Now lets continue with the other dataset

wt.data <- read.csv("weather.csv")

glimpse(wt.data)
#1825 observations, with 15 columns

print(status(wt.data))
#Some cells have NAs and empty cells, but for legit reasons, 
#Columns such as precipitation has many empty cells, 
#it there are no rain, there would be recorded value. 

hist(wt.data$mean_temperature_f) 
#Mean temperature is around 60 - 70 degrees farenheit per day

hist(wt.data$mean_wind_speed_mph) 
#Mean wind speed is usually between 5-10mph per 

describe(wt.data)
#This gives a good summary of the data 

#2. Find number of cancelled trips and identify the indices.
#Duration of trips are given in seconds 
#To approach this questions, we are going to find the trip duration that are 
#smaller than 3*60 secs (180sec), and those that start and end in the same station 

cancelled <- which(trip.data$start_station_id == trip.data$end_station_id & trip.data$duration < 180)
#Vector of indices for trips that start and ends at the same station while shorted than 3 min

cancelled.id <- trip.data$id[c(cancelled)]
#All the trip id removed to give trip.data1

trip.data1 <- trip.data[-c(cancelled),]
#this new dataset now have cancelled trips removed. 

#3 Identify the outliers
#lets find outliers in durations. identify those that are either very short or very long 
#Outliers are defined based on its distances from the interquartile range
#Anything beyond 1.5*IQR from the 1st and 3rd quantile is considered an outlier.

quantile(trip.data1$duration)
#1st Quantile is 342,  3rd quantile is 747

IQR(trip.data1$duration)
#IQR is found to be 405, 1.5*405 = 607.5

out <- which(trip.data1$duration > 1.5*IQR(trip.data1$duration) + 747|trip.data1$duration < 342 - 1.5*IQR(trip.data1$duration))
#These are the identified outliers for durations in trip data 

out.id <- trip.data1$id[c(out)]
#trip id of the outliers. 

trip.data2 <- trip.data1[-c(out),]
#This is the dataset with duration outliers removed 

#4 identify rushhours，frequent stations during those hours and for weekends 
#Covert hours to posixs, make a new dataframe and leave trip.data2 alone

library(dplyr)
library(lubridate)

#Convert both to POSIX form with lubridate functions
start.d <- mdy_hm(trip.data2$start_date)
ends.d <- mdy_hm(trip.data2$end_date)
start.t <- as_hms(start.d)
end.t <- as_hms(ends.d)
start.date <- date(start.d)
end.date <- date(ends.d)
wod <- wday(start.date, label = TRUE)

#Now lets put together everything 
#Now we have date and time separated

trip.data3 <- trip.data2%>%
  mutate(start_date = start.date)%>%
  mutate(end_date = end.date)%>%
  mutate(start_time = start.t)%>%
  mutate(end_time = end.t)%>%
  mutate(DoW = wod)

#First we subset for only observations on weekdays, DoW doesn't equal to Sat or Sun
trip.data.3.1 <- subset(trip.data3, DoW != "Sun"& DoW !="Sat")

#To identify Rush hours, make a histogram for end time and start time, keep it to the hours
#So that we can use hist() 

s.hours <- as.numeric(strftime(trip.data.3.1$start_time, "%H"))
#Start Hours
e.hours <- as.numeric(strftime(trip.data.3.1$end_time, "%H"))
#end Hours 
h1 <- hist(s.hours, breaks = c(0:24))

h2 <- hist(e.hours, breaks = c(0:24))

plot( h1, col=rgb(0,0,1,1/4), xlim=c(0,24), xlab = 
        "Hours of the Day", main = "Frequency of start vs. end times of Rides in a day")  # first histogram
plot( h2, col=rgb(1,0,0,1/4), xlim=c(0,24), add=T, )

legend("topright", c("Start", "End"), fill= c('lightblue', 'pink'))

#From this overlaid histogram I would consider the rush hours being from 6 to 8 in the morning
#In the afternoon Rush hours would be from 15 to 17.
#Now we would like to find the most frequent starting and ending station during rush hours,
#Given that we are only working with weekdays 

trip.data.3.2 <- subset(trip.data.3.1, (s.hours < 9 & s.hours > 5)|(s.hours < 18 & s.hours > 14))
#trip.data.3.2, subset observations with starting time during rush hours

table(trip.data.3.2$start_station_name) %>%
  as.data.frame() %>% 
  arrange(desc(Freq))

#Top 10 busiest start stations during rush hours. 
#1       San Francisco Caltrain (Townsend at 4th) 14072
#2  Temporary Transbay Terminal (Howard at Beale)  7907
#3        San Francisco Caltrain 2 (330 Townsend)  7185
#4           Harry Bridges Plaza (Ferry Building)  6554
#5                                2nd at Townsend  5807
#6                              Steuart at Market  5624
#7                                Townsend at 7th  5058
#8                              Market at Sansome  4872
#9                         Embarcadero at Sansome  4385
#10                                Market at 10th  3870

trip.data.3.3 <- subset(trip.data.3.1, (e.hours < 9 & e.hours > 5)|(e.hours < 18 & e.hours > 14))
#trip.data.3.3 contains all obervations during rush hours. 

#Top 10 busiest end stations during rush hours. 
table(trip.data.3.3$end_station_name) %>%
  as.data.frame() %>% 
  arrange(desc(Freq))
#1       San Francisco Caltrain (Townsend at 4th) 12843
#2  Temporary Transbay Terminal (Howard at Beale)  7126
#3        San Francisco Caltrain 2 (330 Townsend)  6493
#4           Harry Bridges Plaza (Ferry Building)  5642
#5                                2nd at Townsend  5381
#6                              Steuart at Market  5063
#7                                Townsend at 7th  4748
#8                              Market at Sansome  4504
#9                         Embarcadero at Sansome  4145
#10                             2nd at South Park  3570

#10 most frequent starting/ending station during weekdays.
trip.data.3.4 <- subset(trip.data3, DoW == "Sun"| DoW =="Sat")
#Observations during weekend in df trip.data.3.4

table(trip.data.3.4$start_station_name) %>%
  as.data.frame() %>% 
  arrange(desc(Freq))
#1                         Embarcadero at Sansome 2145
#2           Harry Bridges Plaza (Ferry Building) 1924
#3                                  Market at 4th 1266
#4                                2nd at Townsend 1232
#5                          Embarcadero at Bryant 1232
#6                             Powell Street BART 1147
#7       San Francisco Caltrain (Townsend at 4th) 1080
#8                Grant Avenue at Columbus Avenue 1028
#9                                 Market at 10th  877
#10       San Francisco Caltrain 2 (330 Townsend)  871
#top 10 most frequent starting station during weekends. 

table(trip.data.3.4$end_station_name) %>%
  as.data.frame() %>% 
  arrange(desc(Freq))
#1           Harry Bridges Plaza (Ferry Building) 2344
#2                         Embarcadero at Sansome 1664
#3                                  Market at 4th 1507
#4                             Powell Street BART 1378
#5       San Francisco Caltrain (Townsend at 4th) 1355
#6                                2nd at Townsend 1269
#7                          Embarcadero at Bryant 1125
#8                              Steuart at Market  976
#9                                Townsend at 7th  922
#10                             Market at Sansome  914
#Top 10 busiest end stations during the weekend. 

#Utilization of Bikes/Month 
