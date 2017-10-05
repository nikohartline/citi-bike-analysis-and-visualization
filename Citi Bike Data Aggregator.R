list.of.packages <- c("data.table","lubridate","dplyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#Checks and installs/loads all packages used in this script

FactorizeNumerizeDatify=function(x){
  for(i in 1:ncol(x)){
    if(names(x)[i]=="Date"){x[,i]=as.Date(x[[i]])}
    else if(names(x)[i]%in%c("BikeID","DayNumber","StartStationID","EndStationID")){x[,i]=as.factor(x[[i]])}
    else x[,i]=as.numeric(x[[i]])
    }
  return(x)
} #slightly unfortunate function to correct for classification as "character" of some numeric columns

windspeedbins=seq(0,14,2)

fread("Output/Weather and Bike Share Data Combined.csv") %>%
  FactorizeNumerizeDatify %>%
  mutate(.,AvgWindSpeedRange=cut(AverageWindSpeed_mph,breaks=windspeedbins, right=FALSE,labels=c("0 to <2","2 to <4","4 to <6","6 to <8","8 to <10","10 to <12","12 to <14"))) %>%
  mutate(.,DayoftheWeek=wday(Date,label=TRUE,abbr=TRUE)) -> classifiedtable
#Classifies columns of the bike data and creates bins for days of the week and wind speeds

#Creates a day of the week aggregates table with day of the week (M/T/W etc.), average trip duration, and number of trips.
aggregate(TripDuration~DayoftheWeek,data=classifiedtable,FUN="mean") %>%
  setnames(.,"TripDuration","AvgTripDuration") %>%
  mutate(.,NumberofTrips=as.numeric(table(classifiedtable$DayoftheWeek))) %>%
  write.csv("Output/Day of the Week Aggregates.csv",row.names=FALSE)

#Note for the output to work below, the weather data from New York must be between 2013-07-01 to 2014-02-28
fread("NYC Weather Data/weatherNYC.csv") %>%
  subset(.,NAME=="NY CITY CENTRAL PARK, NY US",select=c("AWND","TMAX")) %>%
  mutate(.,AvgWindSpeedRange=cut(as.numeric(AWND),breaks=windspeedbins, right=FALSE,labels=c("0 to <2","2 to <4","4 to <6","6 to <8","8 to <10","10 to <12","12 to <14"))) -> binnedwindspeed

#Creates a wind speed aggregates table with average wind speed ranges (bins), Trip duration (s), # of days at each wind speed bin, and average total daily trip duration (s/day)
aggregate(TripDuration~AvgWindSpeedRange,data=classifiedtable,FUN="sum") %>%
  mutate(.,DaysatWindSpeedBin=as.numeric(table(binnedwindspeed$AvgWindSpeedRange))) %>%
  mutate(.,AvgTotalDailyTripDuration=TripDuration/DaysatWindSpeedBin) %>%
  mutate(.,AverageNumberofTrips=as.numeric(table(classifiedtable$AvgWindSpeedRange))/DaysatWindSpeedBin) %>%
  mutate(.,AverageTripDuration=AvgTotalDailyTripDuration/AverageNumberofTrips) %>%
  write.csv("Output/Wind Speed Aggregates.csv",row.names=FALSE)

#Some statistical tests examining the relationship between temperature, wind, and number of bike trips taken. Shows an issue of multicollinearity, that the negative relationship between temperature and wind speed is a better explanation for the drop in bike trips.
binnedwindspeed$tripcount=as.numeric(table(classifiedtable$DayNumber))
binnedwindspeed$AWND=as.numeric(binnedwindspeed$AWND)
binnedwindspeed$TMAX=as.numeric(binnedwindspeed$TMAX)
cor.test(binnedwindspeed$AWND,binnedwindspeed$TMAX,na.rm=T)
summary(lm(tripcount~AWND,data=binnedwindspeed))
summary(lm(tripcount~AWND+TMAX,data=binnedwindspeed))


