#Loading necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

#loading data files
m4_2020=read.csv(file.choose(),header = TRUE)
m5_2020=read.csv(file.choose(),header = TRUE)
m6_2020=read.csv(file.choose(),header = TRUE)
m7_2020=read.csv(file.choose(),header = TRUE)
m8_2020=read.csv(file.choose(),header = TRUE)
m9_2020=read.csv(file.choose(),header = TRUE)
m10_2020=read.csv(file.choose(),header = TRUE)
m11_2020=read.csv(file.choose(),header = TRUE)
m12_2020=read.csv(file.choose(),header = TRUE)
m1_2021=read.csv(file.choose(),header = TRUE)
m2_2021=read.csv(file.choose(),header = TRUE)
m3_2021=read.csv(file.choose(),header = TRUE)

#Check if all columns have similar data
colnames(m4_2020)==colnames(m5_2020)
colnames(m4_2020)==colnames(m6_2020)
colnames(m4_2020)==colnames(m7_2020)
colnames(m4_2020)==colnames(m8_2020)
colnames(m4_2020)==colnames(m9_2020)
colnames(m4_2020)==colnames(m10_2020)
colnames(m4_2020)==colnames(m11_2020)
colnames(m4_2020)==colnames(m12_2020)
colnames(m4_2020)==colnames(m1_2021)
colnames(m4_2020)==colnames(m2_2021)
colnames(m4_2020)==colnames(m3_2021)

#check the dataframes
str(m4_2020)
str(m5_2020)
str(m6_2020)
str(m7_2020)
str(m8_2020)
str(m9_2020)
str(m10_2020)
str(m11_2020)
str(m12_2020)
str(m1_2021)
str(m2_2021)
str(m3_2021)

#cleaning the NAs

m12_2020=na.omit(m12_2020)
m1_2021=na.omit(m1_2021)
m2_2021=na.omit(m2_2021)
m3_2021=na.omit(m3_2021)

#changing data type of start_station_id from char to int 
m12_2020=mutate(m12_2020,start_station_id=as.integer(start_station_id),
                +                 end_station_id=as.integer(end_station_id))
m1_2021=mutate(m1_2021,start_station_id=as.integer(start_station_id),
               end_station_id=as.integer(end_station_id))
m2_2021=mutate(m2_2021,start_station_id=as.integer(start_station_id),
       end_station_id=as.integer(end_station_id))
m3_2021=mutate(m3_2021,start_station_id=as.integer(start_station_id),
               end_station_id=as.integer(end_station_id))
#Combining all data
trips=bind_rows(m4_2020,m5_2020,m6_2020,m7_2020,m8_2020,m9_2020,m10_2020,
                m11_2020,m1_2021,m2_2021,m3_2021)
#removing unnecessary columns
trips=trips %>% select(-c(start_lat,end_lat,start_lng,end_lng))

#Inspecting the new data frame
str(trips)
colnames(trips)
summary(trips)

#Separate columns for date,year, month, day
trips$date=as.Date(trips$started_at)
trips$month=format(as.Date(trips$started_at),'%m')
trips$day=format(as.Date(trips$started_at),'%d')
trips$year=format(as.Date(trips$started_at),'%Y')
trips$day_of_the_week=format(as.Date(trips$started_at),'%A')

#total ride length
trips$ride_time=difftime(trips$ended_at,trips$started_at)

#changing data type of ride_time
trips$ride_time=as.numeric(as.character(trips$ride_time))

#Removing invalid data
trips=trips[!(trips$start_station_name=="HQ QR" | trips$ride_time<0),]

#compare member and non-member(casual) rides
aggregate(trips["ride_time"], by= trips["member_casual"], FUN = "mean")
aggregate(trips["ride_time"], by= trips["member_casual"], FUN = "median")
aggregate(trips["ride_time"], by= trips["member_casual"], FUN = "max")
aggregate(trips["ride_time"], by= trips["member_casual"], FUN = "min")

#arranging alphabetically the days of the week
trips$day_of_the_week=ordered(trips$day_of_the_week,
levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#comparing rides for different categories on different days of the week
aggregate(trips$ride_time~trips$member_casual+trips$day_of_the_week,
          FUN="mean")

#Analysing type of ridership and weekday
trips_1=trips %>%mutate(weekday=wday(started_at,label = TRUE))%>%
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides = n(), average_duration = mean(ride_time)) %>%   
  arrange(member_casual, weekday) 

ggplot(data=trips_1,mapping=aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position="dodge")   #Visualizing the average duration

#Saving the file
analysis=aggregate(trips$ride_time ~ trips$member_casual +
                               trips$day_of_the_week, FUN = mean)


write.csv(analysis,file="E:\\Google data analyst\\analysis.csv")
