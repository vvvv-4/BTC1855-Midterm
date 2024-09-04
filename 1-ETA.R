########Plan of Action############
#1. Do ETA for Weather and trip data by following steps given by "datasciencehero" 
#2. Find outliers and cancelled rides, remove them from the dataset 
#3. With the cleaned up dataset, find rush hours and the busiet stations 
#4. Calculate utilization by grouping observations in months, 
#5. Merged relevant trip data columns with weather data, then call on corrplot for correlation

##################################

#1. Run the ETA for trip and weather data for the dataset
#Following codes and steps given by the "datasciencehero" 

#Let's call/install some packages/files first 

library(funModeling) 
library(tidyverse) 
library(Hmisc)

###############Trip Data###################
#We'll start with trip data
#To get started... 

# SK Although technically possible, I advise against using periods
# in variable/object names. Try using underscore instead. Reserve dot
# notation for methods.
trip.data <- read.csv("trip.csv")

#To get a brief idea of what we are working with 
#We have in trip data:
#326339 observations with 11 columns
glimpse(trip.data)

#From status, we can see that there are no NA in this dataset. 
#There are some zeros, 50 empty cells in the zipcode column. 
# SK (Points taken) The output of the code below shows 70 unique start/end station IDs, and 
# 74 unique start/end station names. This is a discrepancy worth looking into.
print(status(trip.data))

boxplot(trip.data$duration, main = "Boxplot for the Duration of Bike Rides in Bay Area", xlab = "Duration of Rides")

mean(trip.data$duration)

median(trip.data$duration)


#We find San Francisco Caltrain is the most frequent start and end station. 
freq(trip.data$start_station_name) 
freq(trip.data$end_station_name) 

#Most users are suscribers
freq(trip.data$subscription_type) 

print(profiling_num(trip.data))

plot_num(trip.data)
#This is not as meaningful as id, start station, end station, bike_id does not mean much 
#Duration is what we could look as instead

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

plot_num(wt.data[, -c(14)])
#Mean temperature is around 60 - 70 degrees farenheit per day
#Mean wind speed is usually between 5-10mph per 

describe(wt.data)
#This gives a good summary of the data 

#2. Find number of cancelled trips and outlier remove them from dataset.
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

boxplot(trip.data2$duration, main = "Boxplot for the Duration of Bike Rides in Bay Area (Removed)", xlab = "Duration of Rides")
#Lets visualize everything again.

#4 identify rushhours，frequent stations during those hours and for weekends 
#Convert hours to posixs, make a new dataframe and leave trip.data2 alone

library(dplyr)
library(lubridate)
library(hms)

#Convert both to POSIX form with lubridate functions
start.d <- mdy_hm(trip.data2$start_date)
ends.d <- mdy_hm(trip.data2$end_date)
start.t <- as_hms(start.d)
end.t <- as_hms(ends.d)
#Start and end time
start.date <- date(start.d)
end.date <- date(ends.d)
#Isolate date 
wod <- wday(start.date, label = TRUE)

#Now lets put together everything 
#Now we have date and time separated
# SK This approach of stripping out columns from a dataframe,
# modifying them and then writing them back to the dataframe 
# makes me nervous. You have to be 1000% sure that the order
# of the dataframe and extracted vectors do not change at any time.
# It would be a lot more safer if you made the above transformations
# directly on the dataframe columns using mutate.
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

# SK Great idea to overlay trip start and end hours.

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
#(total time used/total time in month).
#Trip.data 3 have date and time separate, lets make a column with the month of the obersvation

trip.data.3.5 <- trip.data3 %>%
  mutate(month = month(trip.data3$start_date))%>%
  group_by(month)%>%
# SK (Points taken) Why is the denominator the length of the bike id? 
# Bike id is an identifier and should not be used in any analysis.
  summarize(sum = sum(duration)/length(unique(bike_id)))
#Summed the total duration of bike use, trip.data.3.5 gives total duration of bikesa used grouped by months 


trip.data.3.6 <- trip.data.3.5%>%
  group_by(month)%>%
  summarize(ratio = sum/(days_in_month(month)*24*60*60))
#trip.data.3.6 gives the utilization ratio for months 

barplot(trip.data.3.6$ratio, col = "pink", main = "Utilization Ratio of Bikes by Month")

#Correlation between weather, visibility and bike rental. 
#First inspect the weather data to see how it is stored. 
#wt.entries are store in days, we have to coerce trip data by dates 
#I would make a new df keeping duration as well as tallying # of trips / day 

#I am going to group obervations based on date as well as starting city. 
#To gain data on starting city, I would need data from station.csv.
#First step would be to add a city column based on stating station in trip.data3

station.dt <- read.csv("station.csv")
id.city <- station.dt[, c(1,6)]

colnames(id.city) <- c("start_station_id","city")

trip.data4 <- merge(id.city, trip.data3, on = "start_station_id")
#Now we added city based on start station using merge

trip.data5 <- trip.data4[,c(2,4,5)]%>%#We only want duration, date and city for the purpose of correlation 
  group_by(start_date, city)%>%
  summarize(total.duration = sum(duration), num.trips = n())

#Trip.data5 is ready to be joint with weather data!!! yeaaaaa
#Before merging have to make sure than the keys have the same column names and format 

colnames(trip.data5)[colnames(trip.data5) == "start_date"] <- "date"

wt.data1 <- wt.data %>%
  mutate(date = mdy(date))
#Do this to change format of date in the original wt.dt so it its compatible with those in trip.data5

merged.dt <- merge(trip.data5, wt.data1, on = c("city", "date"))
####Yea our stuff merged perfectly!!! whoooooAAAAAA 

# SK (Points taken) What happened to the 'T' values in precipitation column? 
# Did you examine the EDA results of the weather dataset closely?
merged.dt$precipitation_inches <- as.numeric(merged.dt$precipitation_inches)
#Make this into some numeric value 
merged.dt$events <- as.factor(merged.dt$events)
#Make this into a factor 

library(corrplot)

compare <- c(merged.dt$total.duration, merged.dt$num.trips, merged.dt$max_temperature_f)

M <- cor(merged.dt[,-c(1,2,16,17)], use = "complete.obs")
corrplot(M, method = 'color')

summary(merged.dt$events)
#I still want to include where events in my plot, im going to make events into numeric codes!!!
#Looking at it there should be only 4 levels however one obervation had "rain" instead of "Rain"
# SK This is why we do EDA. If you study the results closely, you would be able to identify 
# issues like this early on.
#Find that value and substitute it 

which(merged.dt$events == "rain")
#row 271 
merged.dt$events[271] <- "Rain"
merged.dt$events <- factor(merged.dt$events, levels = c("","Fog","Fog-Rain", "Rain"))

summary(merged.dt$events)
#ok now we only have 4 levels, great!
# SK I wonder where the 'Rain-Thunderstorm' event went?
merged.dt$events <- as.numeric(factor(merged.dt$events, levels = c("","Fog","Fog-Rain", "Rain")))
#Now put this into numeric form so we can actually use corplot on it.   

M <- cor(merged.dt[,-c(1,2,17)], use = "complete.obs")
colnames(M) <- c("Total.Duration", "#trips", "Max_temp", "Mean_temp", "Min_temp", "Max_vis", 
                 "Mean_vis", "Min_vis", "Max_Windspeed", "Mean_Windspeed", "Max_gust", "Precipitation",
                 "Cloud_Cover", "Events")
rownames(M) <- colnames(M)
corrplot(M, method = "color")
