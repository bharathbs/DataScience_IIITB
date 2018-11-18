##########################################################################################################################
#################################                                #########################################################
################################# LINEAR REGRESSION CASE STUDY SOLUTION   ################################################
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
#install.packages("car")
#install.packages("MASS")

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(car)
library(MASS)

##read csv
carData<-read.csv("carPrice_Assignment.csv",stringsAsFactors = FALSE)

str(carData)


#### STEP 1 : DATA CLEANSING ################################################################################################

####1.1 Check for Duplicates. 
nrow(unique(carData))==nrow(carData) 
##No Duplicates

####1 2 Check for NAs
sum(is.na(carData))
## No Nulls.

####1.3 Split Car Company Name and model name and consider only car company name
##Help of Stackoverflow!!!!
carData$CarName<-tolower(carData$CarName)
carData$CarName<-sapply(strsplit(carData$CarName," "), `[`, 1)



####1.4 Clean Data - Company name -toyota,mazda,vw,nissan,porsche
carData$CarName[which(carData$CarName=="maxda")]<-"mazda"
carData$CarName[which(carData$CarName=="vokswagen")] <- "volkswagen"
carData$CarName[which(carData$CarName=="vw")] <- "volkswagen"
carData$CarName[which(carData$CarName=="toyouta")] <- "toyota"
carData$CarName[which(carData$CarName=="porcshce")] <- "porsche"


####1.5 REMOVE CAR ID - Its a primary key.
carData<-carData[ , -which(names(carData) %in% c("car_ID"))]


####1.6 Convert all categorical variables into Factor
## Will be helpful in creating dummy variables.
carData$fueltype<-as.factor(carData$fueltype)
carData$aspiration<-as.factor(carData$aspiration)
carData$doornumber<-as.factor(carData$doornumber)
carData$carbody<-as.factor(carData$carbody)
carData$drivewheel<-as.factor(carData$drivewheel)
carData$enginelocation<-as.factor(carData$enginelocation)
carData$cylindernumber<-as.factor(carData$cylindernumber)
carData$enginetype<-as.factor(carData$enginetype)
carData$fuelsystem<-as.factor(carData$fuelsystem)
carData$CarName<-as.factor(carData$CarName)
carData$symboling<-as.factor(carData$symboling)


str(carData)
## We have only Factors,num and int data types.


####1.7 CHECK OUTLIERS

##CHECK QUARTILE FOR wheelbase
quantile(carData$wheelbase,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 99%
carData$wheelbase[which(carData$wheelbase>115.544)]<-115.544


##CHECK QUARTILE FOR enginesize
quantile(carData$enginesize,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 96%
carData$enginesize[which(carData$enginesize>209)]<-209.00




# CHECK QUARTILE FOR CURBWEIGHT
quantile(carData$curbweight,seq(0,1,0.01))
# WE CAN SEE WE HAVE A LOWER OUTLIER AT 1%.
carData$curbweight[which(carData$curbweight<1819.72)]<-1819.72



##CHECK QUARTILE FOR STROKE
quantile(carData$stroke,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 2% and 95%.
carData$stroke[which(carData$stroke<2.64)]<-2.64
carData$stroke[which(carData$stroke>3.64)]<-3.64


##CHECK QUARTILE FOR horsepower
quantile(carData$horsepower,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 97%.
carData$horsepower[which(carData$horsepower>184.00)]<-184.00

##CHECK QUARTILE FOR compressionratio
quantile(carData$compressionratio,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 90%.
carData$compressionratio[which(carData$compressionratio>10.9400)]<-10.9400


##CHECK QUARTILE FOR citympg
quantile(carData$citympg,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 98%.
carData$citympg[which(carData$citympg>38.00)]<-38.00


##CHECK QUARTILE FOR highwaympg
quantile(carData$highwaympg,seq(0,1,0.01))
## WE CAN SEE WE HAVE A OUTLIER AT 96%.
carData$highwaympg[which(carData$highwaympg>43.00)]<-43.00

#############DATA CLEANSING ENDS##############################################################################

####2 DATA PREPARATION ##############################################################################################

####2.1 - CREATION OF DUMMY VARIABLES.

## ALL THE BELOW VARIABLES ARE HAVING ONLY 2 LEVELS.SO WE CAN CONVERT THEM DIRECTLY

carData$fueltype <- as.numeric(levels(carData$fueltype)<-c(1,0))[carData$fueltype]
carData$aspiration <- as.numeric(levels(carData$aspiration)<-c(1,0))[carData$aspiration]
carData$doornumber <- as.numeric(levels(carData$doornumber)<-c(1,0))[carData$doornumber]
carData$enginelocation <- as.numeric(levels(carData$enginelocation)<-c(1,0))[carData$enginelocation]



## ALL BELOW VARIABLES ARE HAVING MULTIPLE LEVELS. WE have to create dummy variables for them


### CAR BODY
str(carData$carbody)

## We have 5 levels of CarBody
dummy1<-data.frame(model.matrix(~carbody, data=carData))
dummy1<-dummy1[,-1]
carData<-cbind(carData[,-6],dummy1)




###DRIVE WHEEL
str(carData$drivewheel)

## We have 3 levels of Drive Wheel
dummy2<-data.frame(model.matrix(~drivewheel, data=carData))
dummy2<-dummy2[,-1]
carData<-cbind(carData[,-6],dummy2)


### Engine Type
str(carData$enginetype)

## We have 7 levels of Engine Type.We will bin it.
levels(carData$enginetype)[1:3]<-"engineType1"
levels(carData$enginetype)[2:3]<-"engineType2"
levels(carData$enginetype)[3:4]<-"engineType3"

dummy3<-data.frame(model.matrix(~enginetype,data=carData))
dummy3<-dummy3[,-1]
carData<-cbind(carData[,-12],dummy3)


### CYLINDER NUMBER
str(carData$cylindernumber)

## We have 7 levels of cylinders.We will bin it.
levels(carData$cylindernumber)[1:3]<-"cylinderType1"
levels(carData$cylindernumber)[2:3]<-"cylinderType2"
levels(carData$cylindernumber)[3:4]<-"cylinderType3"

dummy4<-data.frame(model.matrix(~cylindernumber,data=carData))
dummy4<-dummy4[,-1]
carData<-cbind(carData[,-12],dummy4)

## FUEL SYSTEM
str(carData$fuelsystem)

##We have 8 levels of Fuelsystem.We will bin it
levels(carData$fuelsystem)[1:3]<-"fuelSystemType1"
levels(carData$fuelsystem)[2:4]<-"fuelSystemType2"
levels(carData$fuelsystem)[3:5]<-"fuelSystemType3"

dummy5<-data.frame(model.matrix(~fuelsystem,data=carData))
dummy5<-dummy5[,-1]
carData<-cbind(carData[,-13],dummy5)


## CAR NAME
str(carData$CarName)

## We have 22 levels of Car Name.We will bin it.
levels(carData$CarName)[1:6]<-"CarNameGroup1"
levels(carData$CarName)[2:7]<-"CarNameGroup2"
levels(carData$CarName)[3:7]<-"CarNameGroup3"
levels(carData$CarName)[4:8]<-"CarNameGroup4"


dummy6<-data.frame(model.matrix(~CarName,data=carData))
dummy6<-dummy6[,-1]
carData<-cbind(carData[,-2],dummy6)

## SYMBOLING
str(carData$symboling)

## We have 6 levels of Symboling.We will bin it.

levels(carData$symboling)[1:2]<-"Safest"
levels(carData$symboling)[2:3]<-"Safe"
levels(carData$symboling)[3:4]<-"Risky"


dummy7<-data.frame(model.matrix(~symboling,data=carData))
dummy7<-dummy7[,-1]
carData<-cbind(carData[,-1],dummy7)


#### DATA PREPERATION ENDS##################################################################################



####3 MODELLING ###########################################################################################

##3.1 CREATE TEST DATA AND TRAIN DATA

set.seed(100)

trainDataset.indices <- sample(1:nrow(carData),0.7*nrow(carData))
trainDataset = carData[trainDataset.indices,]

testDataset = carData[-trainDataset.indices,]

##3.2 MODELLING

##3.2.1 SINCE WE ARE TRYING TO BUILD A MODEL TO DERIVE PRICE,IT WILL BE OUR DEPENDENT VARIABLE.

model_1 <- lm(trainDataset$price~.,data = trainDataset)

summary(model_1)

step <- stepAIC(model_1,direction = "both")

step

##3.2.2 - AFTER RUNNNING STEPAIC,WE HAVE OBTAINED OUR FIRST MODEL WHICH WE WILL REGINE GOING FORWARD.


model2<-lm(formula = trainDataset$price ~ fueltype + enginelocation + 
             carwidth + carheight + curbweight + enginesize + boreratio + 
             stroke + horsepower + citympg + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)

summary(model2)
sort(vif(model2))

cor(trainDataset$curbweight,trainDataset$enginesize)

#### MODEL 2 OUTPUT
#### Multiple R-squared:  0.9353,	Adjusted R-squared:  0.9241 
#### WE HAVE ENGINE SIZE AND CURBWEIGHT WITH HIGH VIFs but a SIGNIFICANT P VALUE(p<0.05)
#### AFTER WE FOUND CORRELATION BETWEEN THEM TO 86% WE DECIDE TO REMOVE ENGINE SIZE SINCE IT IS LESS SIGNIFICANT COMPARED TO CURB WEIGHT

#### DECISION - REMOVING ENGINE SIZE AND CONTINUE MODEL BUILDING

model3<-lm(formula = trainDataset$price ~ fueltype + enginelocation + 
             carwidth + carheight + curbweight  + boreratio + 
             stroke + horsepower + citympg + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)

summary(model3)
sort(vif(model3))

cor(trainDataset$horsepower,trainDataset$curbweight)

#### MODEL3 OUTPUT
#### Multiple R-squared:  0.9273,	Adjusted R-squared:  0.9154 
#### WE HAVE HORSEPOWER AND CURBWEIGHT WITH HIGH VIFs but a SIGNIFICANT P VALUE(p<0.05)
#### AFTER WE FOUND CORRELATION BETWEEN THEM TO 77% WE DECIDE TO REMOVE HORSEPOWER SINCE IT IS LESS SIGNIFICANT COMPARED TO CURB WEIGHT

#### DECISION - REMOVING HORSE POWER AND CONTINUE MODEL BUILDING

model4<-lm(formula = trainDataset$price ~ fueltype + enginelocation + 
             carwidth + carheight + curbweight  + boreratio + 
             stroke  + citympg + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)

summary(model4)
sort(vif(model4))


#### MODEL4 OUTPUT
#### Multiple R-squared:  0.9186,	Adjusted R-squared:  0.9061 
#### WE HAVE CITY MPG AND CURBWEIGHT WITH HIGH VIFs but CITY MPG HAS A HIGHER P VALUE(>0.05)
#### DECISION - REMOVING CITY MPG AND CONTINUE MODEL BUILDING


model5<-lm(formula = trainDataset$price ~ fueltype + enginelocation + 
             carwidth + carheight + curbweight  + boreratio + 
             stroke   + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)

summary(model5)
sort(vif(model5))

cor(trainDataset$carbodysedan,trainDataset$drivewheelfwd)
cor(trainDataset$carbodysedan,trainDataset$curbweight)
cor(trainDataset$carbodysedan,trainDataset$carwidth)
cor(trainDataset$carbodysedan,trainDataset$carbodyhatchback)
cor(trainDataset$curbweight,trainDataset$carbodyhatchback)

#### MODEL5 OUTPUT
#### Multiple R-squared:  0.9168,	Adjusted R-squared:  0.9047 
#### WE HAVE CARWIDTH,CARBODYSEDAN,CARBODYHATCHBACK,CARBODYWAGON,DRIVEWHEELFWD,CURBWEIGHT AND BORERATIO.BORE RATIO  WITH HIGH VIFs but BORE RATIO HAS A HIGHER P VALUE(>0.05)
#### ALSO THEIR IS NO CO-RELATION BETWEEN OTHER VARIABLES.
#### DECISION - REMOVING BORERATIO AND CONTINUE MODEL BUILDING

model6<-lm(formula = trainDataset$price ~ fueltype + enginelocation + 
             carwidth + carheight + curbweight   + 
             stroke   + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)

summary(model6)
sort(vif(model6))

cor(trainDataset$carbodysedan,trainDataset$carbodyhatchback)
cor(trainDataset$carbodysedan,trainDataset$carbodywagon)
cor(trainDataset$carbodysedan,trainDataset$carwidth)
cor(trainDataset$curbweight,trainDataset$carwidth)

#### MODEL6 OUTPUT
#### Multiple R-squared:  0.9168,	Adjusted R-squared:  0.9054 
#### WE HAVE CARWIDTH,CARBODYSEDAN,CARBODYHATCHBACK,CARBODYWAGON,DRIVEWHEELFWD AND CURBWEIGHT.ALL OF THEM HAVE A LOWER P VALUE
#### CAR WIDTH AND CURBWEIGHT ARE CO RELATED.HENCE DROPPING CAR WIDTH
#### DECISION - REMOVING CAR WIDTH AND CONTINUE MODEL BUILDING


model7<-lm(formula = trainDataset$price ~ fueltype + enginelocation + 
              carheight + curbweight   + 
             stroke   + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)


summary(model7)
sort(vif(model7))

#### MODEL7 OUTPUT
#### Multiple R-squared:  0.9026,	Adjusted R-squared:  0.8903
#### WE HAVE VARIABLES WITH HIGH VIF BUT SIGNIFICANT P VALUES and ARE NOT CORELATED TO EACH OTHER
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### FUEL TYPE HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING FUEL TYPE  AND CONTINUE MODEL BUILDING


model8<-lm(formula = trainDataset$price ~ enginelocation + 
             carheight + curbweight   + 
             stroke   + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)




summary(model8)

#### MODEL8 OUTPUT
#### Multiple R-squared:  0.9024,	Adjusted R-squared:  0.8909
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### CAR HEIGHT HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING CAR HEIGHT  AND CONTINUE MODEL BUILDING


model9<-lm(formula = trainDataset$price ~ enginelocation + 
             curbweight   + 
             stroke   + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelfwd + cylindernumbercylinderType2 + 
             cylindernumbercylinderType3 + CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
             CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)



summary(model9)


#### MODEL9 OUTPUT
#### Multiple R-squared:  0.9018,	Adjusted R-squared:  0.8911
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### CYLINDER NUMBER CYLINDER TYPE2 HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING CYLINDER NUMBER CYLINDER TYPE2  AND CONTINUE MODEL BUILDING

model10<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              stroke   + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelfwd + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4 + symbolingSafe + peakrpm, data = trainDataset)




summary(model10)

#### MODEL10 OUTPUT
#### Multiple R-squared:  0.8995,	Adjusted R-squared:  0.8902
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### SYMBOLINGSAFE HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING SYMBOLINGSAFE  AND CONTINUE MODEL BUILDING

model11<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              stroke   + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelfwd + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4  + peakrpm, data = trainDataset)



summary(model11)

#### MODEL11 OUTPUT
#### Multiple R-squared:  0.8997,	Adjusted R-squared:  0.8891
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### CARBODYSEDAN HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING CARBODYSEDAN  AND CONTINUE MODEL BUILDING

model12<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              stroke   + carbodyhatchback  + 
              carbodywagon + drivewheelfwd + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4  + peakrpm, data = trainDataset)


summary(model12)

#### MODEL12 OUTPUT
#### Multiple R-squared:  0.8934,	Adjusted R-squared:  0.8853
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### carbodyhatchback HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING carbodyhatchback  AND CONTINUE MODEL BUILDING

model13<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              stroke  + 
              carbodywagon + drivewheelfwd + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4  + peakrpm, data = trainDataset)


summary(model13)

#### MODEL13 OUTPUT
#### Multiple R-squared:  0.8915,	Adjusted R-squared:  0.8841
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### peakrpm HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING peakrpm  AND CONTINUE MODEL BUILDING


model14<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              stroke  + 
              carbodywagon + drivewheelfwd + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4, data = trainDataset)


summary(model14)

#### MODEL14 OUTPUT
#### Multiple R-squared:  0.8874,	Adjusted R-squared:  0.8807
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### drivewheelfwd HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING drivewheelfwd  AND CONTINUE MODEL BUILDING

model15<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              stroke  + 
              carbodywagon  + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4, data = trainDataset)



summary(model15)


#### MODEL15 OUTPUT
#### Multiple R-squared:  0.8843,	Adjusted R-squared:  0.8783
#### WE WILL START CONSIDERING P VALUES TO REMOVE VARIABLES
#### STROKE HAS A HIGHER P VALUE AND WE WILL REMOVE IT
#### DECISION - REMOVING STROKE  AND CONTINUE MODEL BUILDING

model16<-lm(formula = trainDataset$price ~ enginelocation + 
              curbweight   + 
              carbodywagon  + 
              CarNameCarNameGroup2 + CarNameCarNameGroup3 + 
              CarNameCarNameGroup4, data = trainDataset)

summary(model16)


#### MODEL16 OUTPUT
#### Multiple R-squared:  0.8777,	Adjusted R-squared:  0.8723
#### ALL OUR VARIABLES HAVE HIGH SIGNIFICANCE.HENCE WE CANNOT DROP ANYONE OF THEM

#### LETS CHECK WHETHER THEY ARE CORRELATED TO ONE OTHER

cor(trainDataset$curbweight,trainDataset$enginelocation)
cor(trainDataset$curbweight,trainDataset$carbodywagon)
cor(trainDataset$enginelocation,trainDataset$carbodywagon)
cor(trainDataset$enginelocation,trainDataset$CarNameCarNameGroup2)
cor(trainDataset$enginelocation,trainDataset$CarNameCarNameGroup3)
cor(trainDataset$enginelocation,trainDataset$CarNameCarNameGroup4)
cor(trainDataset$carbodywagon,trainDataset$CarNameCarNameGroup4)


#### ALL OF THEM ARE UNRELATED TO EACH OTHER.

### SO MODEL 16 is our final MODEL
#### ADJUSTED R SQUARED = 0.8723

### NOW LETS TEST WITH TEST DATA ON OUR MODEL

Predict1<-predict(model16, testDataset[, -18])
cor(testDataset$price,Predict1)^2

### TESTED R SQUARE - 0.794

##### WE SHOULD IDEALLY HAVE A DIFFERENCE OF 5% BETWEEN ADJUSTED R SQUARE AND TESTED SQUARE
#### BUT IN OUR CASE WE HAVE AROUND 7% DIFFERENCE WHICH MIGHT BE DUE TO SMALLER DATA SET.

#### FINAL VARIABLES IMPACTING PRICE
###1. CAR NAMES
###2. CAR BODY- WAGON
###3. ENGINE LOCATION
###4. CURB WEIGHT



