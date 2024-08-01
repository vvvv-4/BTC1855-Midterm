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

