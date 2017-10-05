list.of.packages <- c("data.table","lubridate","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#Checks and installs/loads all packages used in this script

dir.create("Output", showWarnings = FALSE)
#Creates an output folder for joined and trimmed data if it doesn't exist

datread=function(csvfilelist){
  datalist=list()
  for (i in csvfilelist){
    datalist[[i]]=fread(i)
  }
  return(datalist)
}

#Data from NOAA Climate Data Online https://www.ncdc.noaa.gov/cdo-web/ that are put into a folder labeled "NYC Weather Data". Note that this is inflexible as the data is being specifically selected from Central Park by name
#Variables of interest:
#AWND- Average Wind Speed (mph)
#PRCP- Precipitation (in)
#TMAX- Max Temperature (F)
#TMIN- Min Temperature (F)

fread("NYC Weather Data/weatherNYC.csv") %>%
  subset(.,NAME=="NY CITY CENTRAL PARK, NY US",select=c("DATE","AWND","PRCP","TMAX","TMIN")) %>%
  transform(.,YYYYMMDD=date(DATE)) %>%
  transform(.,absoluteday=row_number(DATE))-> weatherNYCjoin


list.files(pattern="*.csv")%>% #make sure that the working directory folder has all the data in .csv format to import
  datread() %>%
  do.call("rbind",.) %>%
  transform(.,YYYYMMDD=date(starttime)) %>%
  left_join(.,weatherNYCjoin) %>%
  select(.,c("absoluteday","YYYYMMDD","tripduration","start station id","end station id","bikeid","AWND","PRCP","TMAX","TMIN")) %>% #Removes some redundant variables (station name vs id) and variables that won't be analyzed herein.
  setnames(.,c("absoluteday","YYYYMMDD","tripduration","start station id","end station id","bikeid","AWND","PRCP","TMAX","TMIN"),c("DayNumber","Date","TripDuration","StartStationID","EndStationID","BikeID","AverageWindSpeed_mph","Precipitation_in","MaximumTemperature_F","MinimumTemperature_F")) %>%
  write.csv("Output/Weather and Bike Share Data Combined.csv",row.names=FALSE)
  

