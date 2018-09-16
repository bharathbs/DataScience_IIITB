##########################################################################################################################
#################################                                #########################################################
#################################     UBER CASE STUDY SOLUTION   #########################################################
#################################                                #########################################################
##### TEAM MEMBERS #######################################################################################################
##### BHARATH KUMAR B.S      #############################################################################################
##########################################################################################################################

##INSTALL AND LOAD PACKAGES
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("lubridate")
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


###STEP 1 : READ FILE AND CHECK THE STRUCTURE OF DATA ------------------------------------------------------------------------------

uber_data<-read.csv("uber request data.csv",stringsAsFactors = FALSE)

str(uber_data)

head(uber_data)

#### We can see that the Request Timestamp and Drop Timestamp have different formats.Next step will be to make it uniform in the dataset.

##cHECK FOR NA 


###STEP 2: MAKE THE DATE FORMAT UNIFORM IN THE DATASET ------------------------------------------------------------------------------

uber_data$Request.timestamp <- parse_date_time(x = uber_data$Request.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"))

uber_data$Drop.timestamp <- parse_date_time(x = uber_data$Drop.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"))


###STEP 3: DERIVE DATE AND TIME AND SAVE IT SEPERATELY IN DATASET.-------------------------------------------------------------------

uber_data$Request.Date <- format(as.POSIXct(uber_data$Request.timestamp,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")

uber_data$Request.Time <- format(as.POSIXct(uber_data$Request.timestamp,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")

uber_data$Drop.Date <- format(as.POSIXct(uber_data$Drop.timestamp,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")

uber_data$Drop.Time <- format(as.POSIXct(uber_data$Drop.timestamp,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")


###STEP 4: DERIVE HOUR FROM TIME STAMP.IT WILL BE HELPFUL IN GROUPING THE DATA ON HOUR FORMAT.----------------------------------------

uber_data$Request.Hour <-as.integer(format(uber_data$Request.timestamp,"%H"))

uber_data$Drop.Hour <-as.integer(format(uber_data$Drop.timestamp,"%H"))


#### DATA CLEANING AND DERIVED METRICS IS DONE.---------------------------------------------------------------------------------------



#### PROBLEM STATEMENT 1 -------------------------------------------------------------------------------------------------------------
#### FIGURE OUT THE FREQUENCY OF REQUEST THAT WERE CANCELLED\SHOW NO CARS AVAILABLE

### 1.FILTER DATA SET TO CONTAIN ONLY CANCELLED oR NO CARS AVAILABLE.-------------------------------------------------------------------

uber_frequency =  uber_data%>%filter(Status=='Cancelled' | Status== "No Cars Available")


### 2.PLOT THE FREQUENCY OF CANCELLATION OR NO CARS AVAILABLE ON HOURLY BASIS FOR FIVE DAYS-------------------------------------------------

uber_frequency%>%ggplot(aes(x=uber_frequency$Request.Hour,fill=factor(uber_frequency$Status))) +
  geom_bar() + labs(x="Requested Hour",y="No of Requests(Cancelled/No Cars Available)",title="FREQUENCY OF CANCELLATION/NO CARS AVAILABLE ON HOURLY BASIS(FIVE DAYS PERIOD)")


### FROM THE PLOT WE CAN CLEARLY CONCLUDE THAT IN THE EVENINGS AND EARLY MORNINGS WE HAVE LOT OF CANCELLATIONS\NO CARS AVAILABLE.


###3.FIGURE OUT DURING WHICH HOURS WE HAVE ISSUE IN AIRPORT AND CITY RESPECTIVELY.-------------------------------------------------------------
### i.e - DO A UNIVARIATE ANALYSIS ON AIRPORT AND CITY !!!


### FILTER DATA WITH PICKUP POINT AS AIRPORT
ggplot(uber_frequency%>%filter(uber_frequency$Pickup.point=="Airport"),aes(x=Request.Hour,fill=factor(Status)))+
  geom_bar() + labs(x="Requested Hour",y="No of Requests",title="PICKUP POINT -- AIRPORT")

### WE CAN CONCLUDE THAT DURING EVENING(5-10PM) WE HAVE LOT OF CANCELLATIONS FROM AIRPORT.



### NOW LETS CHECK FOR CITY BY FILTERING PICKUP POINT
ggplot(uber_frequency%>%filter(uber_frequency$Pickup.point=="City"),aes(x=Request.Hour,fill=factor(Status)))+
  geom_bar() + labs(x="Requested Hour",y="No of Requests",title="PICKUP POINT -- CITY")


### WE CAN CONCLUDE THAT DURING MORNING(5-9AM) WE HAVE LOT OF CANCELLATION FROM CITY.




###4. LETS PERFORM SEGMENTED UNIVARIATE ANALYSIS BY GROUPING HOURS INTO TIMESLOTS ACROSS A DAY-------------------------------------------------------
## 2AM - 5AM => EARLY MORNING
## 5AM - 12PM => MORNING
## 12PM - 4PM => AFTERNOON
## 4PM - 10PM  => EVENING
## 10PM - 2AM => NIGHT

uber_frequency$Time.Slot <- ifelse(uber_frequency$Request.Hour>=2 & uber_frequency$Request.Hour < 5,"Early Morning",ifelse(uber_frequency$Request.Hour>=5 & uber_frequency$Request.Hour<12,"Morning",ifelse(uber_frequency$Request.Hour>=12 & uber_frequency$Request.Hour<16,"Afternoon",ifelse(uber_frequency$Request.Hour>=16 & uber_frequency$Request.Hour<22,"Evening","Night"))))


###5. PLOT CANCELLATION AGAINST TIME SLOTS -------------------------------------------------------------------------------------------------------
ggplot(uber_frequency,aes(x=Time.Slot,fill=factor(Status)))+
  geom_bar() + labs(x="Time Slots",y="No of Requests",title="CANCELLATION / No CARS AVAILABLE BASED ON TIME SLOTS")


## WE CAN CONCLUDE THAT MORNING AND EVENING SLOTS HAVE MAXIMUM CANCELLATIONs.


###6. CHECK FOR WHICH SLOTS HAS MAXIMUM CANCELLATION FROM AIRPORT AND CITY VICE VERSA ---------------------------------------------------------------

### FILTER ON AIRPORT AS PICKUP POINT AND TIME SLOT FOR X AXIS
ggplot(uber_frequency%>%filter(uber_frequency$Pickup.point=="Airport"),aes(x=Time.Slot,fill=factor(Status)))+
  geom_bar() + labs(x="Time Slots",y="Number of Requests",title="PICKUP POINT- AIRPORT")

### WE CAN CLEARLY POINT OUT EVENING HAS MAXIMUM CANCELLATION FROM AIRPORT



### FILTER ON CITY AS PICKUP POINT AND TIME SLOT FOR X AXIS
ggplot(uber_frequency%>%filter(uber_frequency$Pickup.point=="City"),aes(x=Time.Slot,fill=factor(Status)))+
  geom_bar() + labs(x="Time Slots",y="Number of Requests",title="PICKUP POINT- CITY")

### WE CAN CLEARLY POINT OUT MORNING HAS MAXIMUM CANCELLATION FROM CITY.


###### CONCLUSION -----------------------------------------------------------------------------------------------------------------------------

## WE CAN CLEARLY CONCLUDE THE BELOW BASED ON HOUR BASIS FOR FIVE DAYS
##1. WE HAVE MAXIMUM CANCELLATIONS FROM AIRPORT TO CITY DURING 5PM -10PM
##2. WE HAVE MAXIMUM CANCELLATIONS FROM CITY TO AIRPORT DURING 5AM - 10AM

## WE CAN CLEARLY CONCLUDE THE BELOW BASED ON TIME SLOTS.
##1. WE HAVE MAXIMUM CANCELLATIONS FROM AIRPORT TO CITY DURING EVENING SLOT
##2. WE HAVE MAXIMUM CANCELLATIONS FROM CITY TO AIRPORT DURING MORNING SLOT



##### PROBLEM STATEMENT 2 ----------------------------------------------------------------------------------------------------------------------
##### FIGURE OUT THE GAP BETWEEN SUPPLY AND DEMAND ---------------------------------------------------------------------------------------------
#### WE WILL TRY TO FIND THE GAP BASED ON SLOTS.

#### SUPPLY WILL BE CONSIDERED BASED ON TRIPS COMPLETED STATUS
#### DEMAND WILL BE CONSIDERED BASED ON ALL THE STATUS TAKEN TOGETHER.


##1.LETS PERFORM SEGMENTED UNIVARIATE ANALYSIS BY GROUPING HOURS INTO TIMESLOTS ACROSS A DAY ON THE MAIN DATASET ----------------------------------
## 2AM - 5AM => EARLY MORNING
## 5AM - 12PM => MORNING
## 12PM - 4PM => AFTERNOON
## 4PM - 10PM  => EVENING
## 10PM - 2AM => NIGHT

uber_data$Time.Slot <- ifelse(uber_data$Request.Hour>=2 & uber_data$Request.Hour < 5,"Early Morning",ifelse(uber_data$Request.Hour>=5 & uber_data$Request.Hour<12,"Morning",ifelse(uber_data$Request.Hour>=12 & uber_data$Request.Hour<16,"Afternoon",ifelse(uber_data$Request.Hour>=16 & uber_data$Request.Hour<22,"Evening","Night"))))


##Demand and supply depending upon on different time slots.

ggplot(uber_data,aes(x=Time.Slot,fill=factor(Status)))+
  geom_bar(position ="fill") + labs(x="Time Slots",y="Frequency of Requests",title="Demand And Supply Based on Time Slots")

## WE CAN CLEARLY MAKE OUT THAT EARLY MORNING AND EVENING TIME SLOTS HAVE LARGE GAP IN DEMAND AND SUPPLY.

#### QUANTIFY THE GAP BETWEEN IN DEMAND AND SUPPLY.

earlyMorning.Demand <- length(which(uber_data$Time.Slot == "Early Morning"))
earlyMorning.Supply <- length(which(uber_data$Time.Slot=="Early Morning" & uber_data$Status == "Trip Completed"))
earlyMorning.Percentage <- (earlyMorning.Supply/earlyMorning.Demand)*100;##37.81%


Morning.Demand <- length(which(uber_data$Time.Slot == "Morning"))
Morning.Supply <- length(which(uber_data$Time.Slot=="Morning" & uber_data$Status == "Trip Completed"))
Morning.Percentage <- (Morning.Supply/Morning.Demand)*100;##43.10%


Afternoon.Demand <- length(which(uber_data$Time.Slot == "Afternoon"))
Afternoon.Supply <- length(which(uber_data$Time.Slot=="Afternoon" & uber_data$Status == "Trip Completed"))
Afternoon.Percentage <- (Afternoon.Supply/Afternoon.Demand)*100;##61.44%


Evening.Demand <- length(which(uber_data$Time.Slot == "Evening"))
Evening.Supply <- length(which(uber_data$Time.Slot=="Evening" & uber_data$Status == "Trip Completed"))
Evening.Percentage <- (Evening.Supply/Evening.Demand)*100;##34.98%


Night.Demand <- length(which(uber_data$Time.Slot == "Night"))
Night.Supply <- length(which(uber_data$Time.Slot=="Night" & uber_data$Status == "Trip Completed"))
Night.Percentage <- (Night.Supply/Night.Demand)*100;##47.21%

###AFTER QUANTIFYING ALSO WE CAN CONFIRM THAT IN EVENING AND EARLY MORNING WE HAVE GAP IN SUPPLY AND DEMAND.



##2.FINDING OUT TYPE OF REQUESTS WHERE WE MAX GAP IN SUPPLY AND DEMANDS ----------------------------------------------------------------------

## FROM AIRPORT
ggplot(uber_data%>%filter(uber_data$Pickup.point=="Airport"),aes(x=Time.Slot,fill=factor(Status))) +
  geom_bar(position ="fill") + labs(x="Time Slots",y="Frequency of Requests",title="Demand And Supply Based on Time Slots From Airport")

## WE HAVE MAX GAP Between DEMAND AND SUPPLY IN EVENING

### QUANTIFYING THE GAP DURING EVENING TIME SLOT FOR AIRPORT.

airport.EveningDemand <- length(which(uber_data$Time.Slot == "Evening" & uber_data$Pickup.point== "Airport"))
airport.EveningSupply <- length(which(uber_data$Time.Slot=="Evening" & uber_data$Status == "Trip Completed" & uber_data$Pickup.point== "Airport"))
airport.EveningPercentage <- (airport.EveningSupply/airport.EveningDemand)*100;##22.08%


#### CONCLUSION 
## THE GAP BETWEEN SUPPLY AND DEMAND IN AIRPORT IN EVENING IS BECAUSE OF THE BELOW REASONS
## 1.We have around 1861 requests in evening from airport to city but we have only 411 requests which was succesfully processed.
## 2.We have only 22.08% request which were succesfully processed.
## 3.We can clearly see that the gap is due to No Cars Availability.
## 4.We can clearly conclude that we have issue with numbers of cars to pickup in evening at the airport.
 


## FROM CITY

ggplot(uber_data%>%filter(uber_data$Pickup.point=="City"),aes(x=Time.Slot,fill=factor(Status)))+
    geom_bar(position ="fill") + labs(x="Time Slots",y="Frequency of Requests",title="Demand And Supply Based on Time Slots from City")

## WE HAVE MAX GAP Between DEMAND AND SUPPLY IN MORNING

### QUANTIFYING THE GAP DURING MORNING TIME SLOT FOR CITY.

city.MorningDemand <- length(which(uber_data$Time.Slot == "Morning" & uber_data$Pickup.point== "City"))
city.MorningSupply <- length(which(uber_data$Time.Slot=="Morning" & uber_data$Status == "Trip Completed" & uber_data$Pickup.point== "City"))
city.MorningPercentage <- (city.MorningSupply/city.MorningDemand)*100;##34.74%

#### CONCLUSION 
## THE GAP BETWEEN SUPPLY AND DEMAND IN CITY IN MORNING IS BECAUSE OF THE BELOW REASONS
## 1.We have around 1952 requests in morning from city but we have only 601 requests which was succesfully processed.
## 2.We have only 30.78% request which were succesfully processed.
## 3.We can clearly see that the gap is due to more number of cancellations in Morning at city.


#---------------------------------------------------------------------------------------------------------------------------------------------------

#### RECOMMENDATIONS
##### City Trips
## 1. Drivers can be given extra bonus for each trip they complete from City to Airport in Morning Slot . This will reduce the number of cancellations.
## 2. Uber can also pay some incentives for drivers for their Waiting Time at Airport in Morning Time.

#####Airport Trips
## 1. Uber should increase the number of cabs in Airport during Evening. This will reduce the number of Non Availability.
## 2. Drivers are paid extra bonus to complete trips from Airport in Evening Slot.



