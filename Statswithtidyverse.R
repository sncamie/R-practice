library(nycflights13)

library(dplyr)
#library(skimr)

library(knitr)

library(ggplot2)

#Using the view function to explore data
alaska_flights <- flights %>% filter(carrier=="AS")
View(alaska_flights)

#when there are too many points, shake the plot to cuase a jitter 
ggplot(data=alaska_flights, mapping = aes(x=dep_delay, y=arr_delay))+
  geom_jitter(width = 30, height = 30)

#using the help files
?weather

#we can filter the data according the rows that we want to specify
early_january_weather <- weather %>%
  filter(origin=="EWR", month==1 & day <= 15)


#A line plot of the filtered datat
ggplot(data = early_january_weather,mapping = aes(x=time_hour,y=temp))+
  geom_line()

#A histogram with facetting for side by side comparison 
ggplot(data=weather, mapping = aes(x=temp))+
  geom_histogram(bins = 40, color="white")+
  facet_wrap(~month,nrow=4)

#A regular boxplot
ggplot(data = weather, mapping = aes(x=factor(month), y=temp))+
  geom_boxplot()

#A bargraph with side by side comparison 
ggplot(data = flights, mapping=aes(x=carrier, fill=origin))+
  geom_bar(position = position_dodge(preserve = "single"))

#Faceting on a bar graph
ggplot(data=flights, mapping = aes(x=carrier))+
  geom_bar()+
  facet_wrap(~origin, ncol = 1)

#Data wrangling and filtering 
portland_flights <- flights %>%
  filter(dest=="PDX")

View(portland_flights)

# Using the & or operators, the & operator can be omitted in this case
btv_seaflights_fall<- flights %>%
  filter(origin=="JFK" & (dest=="BTV" | dest=="SEA")& month >= 10)
View(btv_seaflights_fall)

#the not function !

not_btv_sea <- flights %>%
  filter(!(dest=="SEA" | dest =="BTV"))
View(not_btv_sea)

#When there are too many options, we must concatenate with c (combibe)

many_airpots <- flights %>%
  filter(dest %in%c("SEA", "SFO", "PDX", "BTV", "BDL"))

View(many_airpots)


#summary statistics 

summary_temp <- weather %>%
  summarise(mean=mean(temp, na.rm = TRUE),
            std_dev=sd(temp,na.rm = TRUE))
summary_temp


#using group_by 

summary_monthly_temp <- weather %>%
  group_by(month)%>%
  summarise(mean=mean(temp,na.rm = TRUE),
            std_dev=sd(temp, na.rm = TRUE))
summary_monthly_temp

diamonds %>%
  group_by(cut)


#counting the number of flights from each airport 

by_origin <- flights %>%
  group_by(origin) %>%
  summarise(count=n())
by_origin

#multiple groups 
by_origin_monthly <- flights %>%
  group_by(origin, month) %>%
  filter(origin=="JFK") %>%
  summarise(count=n())
by_origin_monthly

ggplot(data=by_origin_monthly, mapping = aes(x=month, y=count()))+
  geom_bar()

# manipulating data

weather <- weather %>%
  mutate(weather_in_C=(temp-32)/1.8)

summary_monthly_temp_FC <-weather %>%
  group_by(month)%>%
  summarise(mean_temp_C=mean(weather_in_C, na.rm = TRUE),
            mean_temp_F=mean(temp, na.rm = TRUE))
summary_monthly_temp_FC


flights <- flights %>%
  mutate(gain=dep_delay-arr_delay)
View(flights)

gain_summary<- flights %>%
  summarise(gain)

gain_summary

ggplot(data = flights, mapping = aes(x=gain))+
  geom_histogram(color="white", bins = 20)

#create multiple variables in mutate 

flights <- flights %>%
  mutate(gain=dep_delay-arr_delay,
         hours=air_time/60,
         gain_per_hour=gain/hours)
View(flights)


#sorting 
freq_dest <-flights %>%
  group_by(dest) %>%
  summarise(num_flights=n())
View(freq_dest)

freq_dest %>%
  arrange(desc(num_flights))

#joining dataframes 

flights_joined <- flights %>%
  inner_join(airlines, by="carrier")

View(flights)
View(flights_joined)


#different key variable names 

named_dests <- flights %>%
  group_by(dest) %>%
  summarise(num_flights=n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by=c("dest"="faa")) %>%
  rename(airport_name=name)
named_dests
View(named_dests)


#multiple key variables 

flight_weather_joined <- flights %>%
  inner_join(weather, by= c("year", "month", "day", "hour", "origin"))
View(flight_weather_joined)


#select a few columns 

flights %>%
  select(carrier, flight)

#removing a variable 

flights_no_year <- flights %>%
  select(-year)

#range columns 

flight_arr_time <- flights %>% 
  select(month:day, arr_time:sched_arr_time)
View(flight_arr_time)

#select and reorder 

flights_reorder <- flights %>%
  select(year, month, day, hour, minute, time_hour, everything())

View(flights_reorder)


#rename variables 
flights_time_new <- flights %>%
  select(dep_time,arr_time) %>%
  rename(departure_time=dep_time, arrival_time=arr_time)
glimpse(flights_time_new)

#top n variables 

named_dests %>% top_n(n=10,wt=num_flights)

