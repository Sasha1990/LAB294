#Econ 294 Assignment4#
#problem 0#
print("Xiaoxi Sasha Zhu")
print(1505138)
print("xzhu25@ucsc.edu")

#problem 1#
library(foreign)
flights <-read.csv("/Users/sashazhu/Desktop/flights.csv", stringsAsFactors = FALSE)
airports <-read.csv("/Users/sashazhu/Desktop/airports.csv", stringsAsFactors = FALSE)
weather <-read.csv("/Users/sashazhu/Desktop/weather.csv", stringsAsFactors = FALSE) 
planes   <-read.csv("/Users/sashazhu/Desktop/planes.csv", stringsAsFactors = FALSE) 

#problem 2#
flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)
########################
#except the first one, the others have error reports.##

#problem 3#
flights.2a<-subset(flights,dest=="SFO"|dest=="OAK")
nrow(flights.2a)
##I get 3508 obs in here.##

flights.2b<-subset(flights,dep_delay>=60)
nrow(flights.2b)
##I get 10474 obs in here.##

flights.2c<-subset(flights,arr_delay>=2*dep_delay)
nrow(flights.2c)
##I get 70772 obs in here.##

#problem 4#
require(dplyr)
flights.4a.delay1<-flights%>%dplyr::select(dep_delay,arr_delay)
flights.4b.delay2<-select(flights,ends_with("delay"))
flights.4b.delay3<-select(flights,contains("delay"))


#problem 5#
flights.5a<-flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)
flights.5b<-flights%>%mutate(
  catchuptime=(dep_delay-arr_delay)
)%>%arrange(desc(catchuptime))%>%head(5)

#problem 6#
##############################################
##unclear about the meaning of the question#
flights.6a<-flights %>%
  dplyr::mutate(speed = dist / (time/60))%>%arrange(desc(speed)) %>%head(5)

delta<-flights %>% mutate(delta=dep_delay-arr_delay)
View(delta)

flights.6b<-delta%>%arrange(desc(delta))%>%head(5)
flights.6c<-delta%>%arrange(delta)%>%head(1)
#########################

#problem 7#
flights.7a<-delta %>% group_by(carrier) %>%
  summarise (
    cancelled = sum(cancelled),
    total_flights = n(),
    percent_cancelled= (cancelled/total_flights),
    min = min(delta, na.rm = T),
    quantile1 = quantile(delta, .25, na.rm = T),
    quantile2 = quantile(delta, .75, na.rm = T),
    mean = mean(delta, na.rm = T),
    median = median(delta, na.rm = T),
    quantile90 = quantile(delta, .90, na.rm = T),
    max = max(delta, na.rm = T)
  )

##in this question, I sum up cancelled, it works,but I want to know if we can get.##
##a command in here that can count the number directly.##

flights.7a1<-flights.7a%>%arrange(desc(percent_cancelled))
print(flights.7a1)

###############################################
#This is the original code given by Curtise.##
day_delay <- dplyr::filter(summarize(group_by(dplyr::filter(flights,!is.na(dep_delay)),date),delay = mean(dep_delay),n = n()),n > 10)
##fliter is very similar to subset##
################################################
day_delay1<- dplyr::filter(flights,!is.na(dep_delay))%>% group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n = n())%>%filter(n>10)

#########################
#why I have one more variable in day_delay1, it seeems I have to do one more filter in here, but how to do it?

#problem 8#
flights.8<- mutate(day_delay, difference = delay-lag(delay)) %>%arrange(desc(difference))%>%head(5)
##lag can put the variables into the next line one by one, then use subtraction to get the difference.##

#problem 9#
dest_delay<-flights %>% group_by(dest) %>%
  summarise (
    mean = mean(arr_delay, na.rm = T),
    number_flights=n()
  )

airports<-select(airports,
                 dest = iata, name = airport , city,state, lat, long)
##the new name should be put in front of the old name when we doing the rename procedure.##


#tbl_df must run before the left_join.##
dest_delay %>% tbl_df
df.9a<- airports%>% 
  left_join(dest_delay,by="dest")
df.9a %>% arrange(desc(mean)) %>% head(5)
##I get 3376 obs and 8 variables in this merge.##

df.9b<- airports%>% 
  inner_join(dest_delay,by="dest")
##Not match. They are different.##
##I get 114 obs and 8 variables in this merge.##
##It is easy to find out the result of left_join and inner_join is different.##

df.9c<- airports%>% 
  right_join(dest_delay,by="dest")
##I get 116 obs and 8 variables in this merge,no NA appear in delay.##
###########################################
##delay is mean in here.##

df.9d<- airports%>% 
  full_join(dest_delay,by="dest")
##I get 3378 obs and 8 variables in this merge.##
##there are 3262 NA in delay.##
##number of rows for the two tables are the same.##

#problem 10#
hourly_delay <- flights %>%filter(!is.na(dep_delay)) %>%
  group_by(date,hour) %>%summarize(delay=mean(dep_delay),
                                   n= n()) %>%filter(n>10)

weather.10<-hourly_delay %>%
  inner_join(weather,by=c("date","hour")) 

weather.10a<-weather.10 %>%arrange(desc(delay)) %>%
  tbl_df
for(i in 1:5) {print(weather.10a$conditions[i])}


####weather %>% tbl_df
####df.10<- hourly_delay%>% 
####  full_join(weather)
####join by date and hour##

####df.10.worstweather<-df.10%>%group_by(conditions)%>%summarise(a=max(delay,na.rm=T))
##condition related to biggest is NA,wired!!!!##

#problem 11#
##11 a.##
require(tidyr)
require(dplyr)

##########################################
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
##########################################

df %>% gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)


##11.b##
#################################################
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df
#################################################

df %>% spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)

##11.c##
################################################
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
################################################
df %>% separate(demo, into = c('sex','age','state') , sep = '_')

##11.d##
##################################################
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df
###################################################
df <- df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df

