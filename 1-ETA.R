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