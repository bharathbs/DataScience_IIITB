##########################################################################################################################
#################################                                #########################################################
################################# LOGISTICS REGRESSION CASE STUDY SOLUTION  ##############################################
#################################                                #########################################################
##### TEAM MEMBERS #######################################################################################################
##### BHARATH KUMAR B.S      #############################################################################################
##### MUKUL TAMTA            #############################################################################################
##### SURYA PRAKASH TRIPATHI #############################################################################################
##### VARUN  AHALAWAT        #############################################################################################
##########################################################################################################################
##########################################################################################################################

# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("MASS")
# install.packages("car")
# install.packages("e1071")
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("caTools")
# install.packages("ggplot2")


library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(caTools)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ROCR)

#Importing 5 datasets
employee_survey <- read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv")
manager_survey <- read.csv("manager_survey_data.csv")
out_time <- read.csv("out_time.csv")

#structure of data shows all the data types
str(employee_survey)
str(general_data)
str(in_time)
str(manager_survey)
str(out_time)

#Viewing datasets

View(employee_survey)
View(general_data)
View(in_time)
View(manager_survey)
View(out_time)


####DATA CLEANSING AND MERGING ################################################################################################

#### Remove columns which have NAs for all rows in in_time & out_time
in_time<-in_time[, -which(colMeans(is.na(in_time)) == 1)]
out_time<-out_time[, -which(colMeans(is.na(out_time)) == 1)]

##### Convert datetime dataframe and subtract to find no of hours each employee has worked in a year.
it <- in_time
ot <- out_time
it[,2:250]<- as.data.frame(lapply(it[,2:250],function(x) parse_date_time(x,orders = c("ymd H:M:S", "dmy H:M:S", "mdy H:M:S","ymd H:M", "dmy H:M", "mdy H:M"))))
ot[,2:250] <- as.data.frame(lapply(ot[,2:250],function(x) parse_date_time(x,orders = c("ymd H:M:S", "dmy H:M:S", "mdy H:M:S","ymd H:M", "dmy H:M", "mdy H:M"))))
ot[,2:250]<-ot[,2:250]-it[,2:250]

#### Replacing NA data WITH 0

ot[is.na(ot)] <- 0


##### Changing data type of data set to numeric
ot[,2:250]<- as.data.frame(lapply(ot[,2:250],function(x) as.numeric(x,units="hours")))
str(ot)

#####Rounding off the hours 2 digits after decimal

ot[,2:250]<- as.data.frame(lapply(ot[,2:250],function(x) round(x,2)))

#####Creating Average hours column - Derived Matrices..

ot$averageHrs <- rowMeans(ot[,2:250])

##### Renaming column 1 and dropping columns 2 to 250

colnames(ot)[1]<- c("EmployeeID")
ot <- ot[,-(2:250)]


##### NOW WE HAVE AVERAGE WORKING HOURS OF EACH EMPLOYEE IN A DATA FRAME.

#### BEFORE MERGING CHECK WHETHER WE HAVE A UNIQUE KEY IN EACH DATA FRAME AND DATA IS NOT DUPLICATED.
nrow(unique(employee_survey))==nrow(employee_survey) 
nrow(unique(general_data))==nrow(general_data)
nrow(unique(manager_survey))==nrow(manager_survey)
nrow(unique(in_time))==nrow(in_time)
nrow(unique(out_time))==nrow(out_time)

#Merging datasets
mf1 <- merge(employee_survey,general_data,by.x=c('EmployeeID'),by.y=c('EmployeeID'))
mf2 <- merge(mf1,manager_survey,by.x=c('EmployeeID'),by.y=c('EmployeeID'))
master_frame <- merge(mf2,ot,by.x=c('EmployeeID'),by.y=c('EmployeeID'))

#Check for NA values
sapply(master_frame,function(x) sum(is.na(x)))
sum(is.na(master_frame))

#Check before removing columns from the data set since its of no use to us in modelling.
length(unique(master_frame$EmployeeID))
length(duplicated(master_frame$EmployeeCount))
length(duplicated(master_frame$Over18))
length(duplicated(master_frame$StandardHours))

#Remove EmployeedID column since it is a unique id of each observation
#Remove Employee Count,Over18,StandardHours columns since it is duplicate for each row
master_frame<-master_frame[,-which(names(master_frame) %in% c("EmployeeID","EmployeeCount","Over18","StandardHours"))]

##### NULL VARIABLES REPLACEMENT FOR OTHER VARIABLES

##CATEGORICAL VARIABLES -- CHANGING IT TO POINT TO MODE

Mode <- function(x) {
  test <- unique(x)
  test[which.max(tabulate(match(x, test)))]
}

master_frame$EnvironmentSatisfaction[which(is.na(master_frame$EnvironmentSatisfaction))]<-Mode(master_frame$EnvironmentSatisfaction)
master_frame$JobSatisfaction[which(is.na(master_frame$JobSatisfaction))]<-Mode(master_frame$JobSatisfaction)
master_frame$WorkLifeBalance[which(is.na(master_frame$WorkLifeBalance))]<-Mode(master_frame$WorkLifeBalance)

## Other Numerical Variables
## In case of NumCompaniesWorked,whenver TOtalWorkingYears is equal to Years at Company then it is his\her first company.We will point it to 0.
## In case its not equal assign Median value to it.
master_frame$NumCompaniesWorked<- ifelse((master_frame$TotalWorkingYears == master_frame$YearsAtCompany & is.na(master_frame$NumCompaniesWorked)),0,master_frame$NumCompaniesWorked)
master_frame$NumCompaniesWorked[which(is.na(master_frame$NumCompaniesWorked))]<- median(master_frame$NumCompaniesWorked,na.rm = TRUE)


### Total Working Years is less than the mean\median\mode hence we are replacing it with years at company
master_frame$TotalWorkingYears[which(is.na(master_frame$TotalWorkingYears))]<-master_frame$YearsAtCompany[which(is.na(master_frame$TotalWorkingYears))]


###### OUTLIER TREATMENT ###################

###CHECK OUTLIERS FOR TotalWorkingYears
ggplot(master_frame, aes(x="",master_frame$TotalWorkingYears))+ geom_boxplot(width=0.1)
quantile(master_frame$TotalWorkingYears,seq(0,1,0.01))

master_frame$TotalWorkingYears[which(master_frame$TotalWorkingYears>quantile(master_frame$TotalWorkingYears,0.91))]<-quantile(master_frame$TotalWorkingYears,0.91)

###CHECK OUTLIERS FOR DistanceFromHome
ggplot(master_frame, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)
quantile(master_frame$DistanceFromHome,seq(0,1,0.01))## NO OUTLIERS

###CHECK OUTLIERS FOR Age
ggplot(master_frame, aes(x="",y=Age))+ geom_boxplot(width=0.1)
quantile(master_frame$Age,seq(0,1,0.01))## NO OUTLIERS

###CHECK OUTLIERS FOR MonthlyIncome
ggplot(master_frame, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)
quantile(master_frame$MonthlyIncome,seq(0,1,0.01))

master_frame$MonthlyIncome[which(master_frame$MonthlyIncome>quantile(master_frame$MonthlyIncome,0.9))]<-quantile(master_frame$MonthlyIncome,0.9)

###CHECK OUTLIERS FOR NumCompaniesWorked
ggplot(master_frame, aes(x="",y=master_frame$NumCompaniesWorked))+ geom_boxplot(width=0.1)
quantile(master_frame$NumCompaniesWorked,seq(0,1,0.01))

master_frame$NumCompaniesWorked[which(master_frame$NumCompaniesWorked>quantile(master_frame$NumCompaniesWorked,0.96))]<-quantile(master_frame$NumCompaniesWorked,0.96)


###CHECK OUTLIERS FOR PercentSalaryHike
ggplot(master_frame, aes(x="",y=master_frame$PercentSalaryHike))+ geom_boxplot(width=0.1)
quantile(master_frame$PercentSalaryHike,seq(0,1,0.01))

###CHECK OUTLIERS FOR StockOptionLevel
ggplot(master_frame, aes(x="",y=master_frame$StockOptionLevel))+ geom_boxplot(width=0.1)
quantile(master_frame$StockOptionLevel,seq(0,1,0.01))

master_frame$StockOptionLevel[which(master_frame$StockOptionLevel>quantile(master_frame$StockOptionLevel,0.94))]<-quantile(master_frame$StockOptionLevel,0.94)

###CHECK OUTLIERS FOR TrainingTimesLastYear

ggplot(master_frame, aes(x="",y=master_frame$TrainingTimesLastYear))+ geom_boxplot(width=0.1)
quantile(master_frame$TrainingTimesLastYear,seq(0,1,0.01))
master_frame$TrainingTimesLastYear[which(master_frame$TrainingTimesLastYear>quantile(master_frame$TrainingTimesLastYear,0.87))]<-quantile(master_frame$TrainingTimesLastYear,0.87)
master_frame$TrainingTimesLastYear[which(master_frame$TrainingTimesLastYear<quantile(master_frame$TrainingTimesLastYear,0.05))]<-quantile(master_frame$TrainingTimesLastYear,0.05)

###CHECK OUTLIERS FOR YearsAtCompany
ggplot(master_frame, aes(x="",y=master_frame$YearsAtCompany))+ geom_boxplot(width=0.1)
quantile(master_frame$YearsAtCompany,seq(0,1,0.01))

master_frame$YearsAtCompany[which(master_frame$YearsAtCompany>quantile(master_frame$YearsAtCompany,0.92))]<-quantile(master_frame$YearsAtCompany,0.92)


###CHECK OUTLIERS FOR YearsSinceLastPromotion

ggplot(master_frame, aes(x="",y=master_frame$YearsSinceLastPromotion))+ geom_boxplot(width=0.1)
quantile(master_frame$YearsSinceLastPromotion,seq(0,1,0.01))
master_frame$YearsSinceLastPromotion[which(master_frame$YearsSinceLastPromotion>quantile(master_frame$YearsSinceLastPromotion,0.92))]<-quantile(master_frame$YearsSinceLastPromotion,0.92)

###CHECK OUTLIERS FOR YearsWithCurrManager
ggplot(master_frame, aes(x="",y=master_frame$YearsWithCurrManager))+ geom_boxplot(width=0.1)
quantile(master_frame$YearsWithCurrManager,seq(0,1,0.01))

master_frame$YearsWithCurrManager[which(master_frame$YearsWithCurrManager>quantile(master_frame$YearsWithCurrManager,0.99))]<-quantile(master_frame$YearsWithCurrManager,0.99)



# Bivariate for categorical features

ggplot(master_frame,aes(x=master_frame$JobSatisfaction,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Job satisfaction Vs Attrition") +
  xlab("Job satisaction") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

#Conclusion: As job satisfaction increases from scale of 1-5 attrition rate decreases

ggplot(master_frame,aes(x=master_frame$EnvironmentSatisfaction,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Environment Satisfaction Vs Attrition") +
  xlab("Environment Satisfaction") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

#Conclusion: As environment satisfaction increases from scale of 1-5 attrition rate decreases.

ggplot(master_frame,aes(x=master_frame$Age,fill=master_frame$Attrition))+geom_histogram(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" age Vs Attrition") +
  xlab("age") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

ggplot(master_frame,aes(x=master_frame$BusinessTravel,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Business Travel Vs Attrition") +
  xlab("Business travel") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

#Conclusion
# Attrition rate is more for the employess who travel frequently and non travel has very less
# attrition rate as compare to other type of business travel

ggplot(master_frame,aes(x=master_frame$Department,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Department Vs Attrition") +
  xlab("Department") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 
# Conclusion
# Human resources have more attrition rate as compare to other departments.


ggplot(master_frame,aes(x=master_frame$DistanceFromHome,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" distance from home Vs Attrition") +
  xlab("distance from home") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

ggplot(master_frame,aes(x=master_frame$Education,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Education Vs Attrition") +
  xlab("Education") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic()
# Conclusion
# Employees who have college education (2) for that attrition is more as compare to other level of education.


ggplot(master_frame,aes(x=master_frame$EducationField,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Education field Vs Attrition") +
  xlab("Education field") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic()

# Conclusion
# Employees who have education field human resources for that attrition is more as compare to other level of education
# and employess who have education field technical for that attrition is least as compare to other level of education


ggplot(master_frame,aes(x=master_frame$Gender,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Gender Vs Attrition") +
  xlab("Gender") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic()

# Conclusion
# Both male and female have almost equal attrition rate so it does not look strong predictor for our analysis.

ggplot(master_frame,aes(x=master_frame$JobInvolvement,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Job Involvement Vs Attrition") +
  xlab("Job Involvement") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic()
# Conclusion
# Attrition rate for JOb involvement low and very high is more.

ggplot(master_frame,aes(x=master_frame$JobLevel,fill=master_frame$Attrition))+geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Job Level Vs Attrition") +
  xlab("Job Level") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic()

#attrition is high for job level 2 and 4.

ggplot(master_frame,aes(x=master_frame$JobRole,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Job Role Vs Attrition") +
  xlab("Job Role") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() + coord_flip()

# Conclusion
# Attrition rate is more for research director job role as compare to other job role.

ggplot(master_frame,aes(x=master_frame$MaritalStatus,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Marital Status Vs Attrition") +
  xlab("Marital Status") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

# Attrition rate is more for employees who have marital status single.


ggplot(master_frame,aes(x=master_frame$MonthlyIncome,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Monthly Income Vs Attrition") +
  xlab("Monthly Income") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

ggplot(master_frame,aes(x=master_frame$NumCompaniesWorked,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" No of companies worked Vs Attrition") +
  xlab("No of companies Worked") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 
# Attrition rate is more if no of companies worked is 5.

ggplot(master_frame,aes(x=master_frame$PercentSalaryHike,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" % salary hike Vs Attrition") +
  xlab("% salary hike") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

# Attrition rate is high if % salary hike is 25.

ggplot(master_frame,aes(x=factor(master_frame$PerformanceRating),fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle("Performance rating Vs Attrition") +
  xlab("Performance rating") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 
# Attrition rate is more if employee has recieved outstanding rating last year.


ggplot(master_frame,aes(x=master_frame$averageHrs,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Average working hours Vs Attrition") +
  xlab("Average working hours") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

#Scaling should be done before EDA

ggplot(master_frame,aes(x=master_frame$TotalWorkingYears,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" total working years Vs Attrition") +
  xlab("total working years") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

# Attrition rate is decreasing as total working years increases.


ggplot(master_frame,aes(x=master_frame$TrainingTimesLastYear,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" training time last years Vs Attrition") +
  xlab("training time last years") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

# If number of times training conducted last year is more than 3, attrition rate is less.


ggplot(master_frame,aes(x=master_frame$WorkLifeBalance,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Work Life Balance Vs Attrition") +
  xlab("Work Life Balance") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 
# Attrition rate is very high if work life balance is bad(1)

ggplot(master_frame,aes(x=master_frame$YearsAtCompany,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Years At Company Vs Attrition") +
  xlab("Years At Company") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 
# Attrition rate is decreasing if years at company is increasing.


ggplot(master_frame,aes(x=master_frame$YearsSinceLastPromotion,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Years since last promotion Vs Attrition") +
  xlab("Years since last promotion") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 

ggplot(master_frame,aes(x=master_frame$YearsWithCurrManager,fill=master_frame$Attrition))+geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Years with current manager Vs Attrition") +
  xlab("Years with current manager") + ylab("Count") + 
  labs(fill=" Attrition ") +
  theme_classic() 


################################################################

# Feature standardisation

# Normalising continuous features 

### FIELDS
### AGE
master_frame$Age<- scale(master_frame$Age) 

###DistanceFromHome
master_frame$DistanceFromHome<- scale(master_frame$DistanceFromHome) 

###MontlyIncome
master_frame$MonthlyIncome<- scale(master_frame$MonthlyIncome) 

###NumCompaniesWorked
master_frame$NumCompaniesWorked<- scale(master_frame$NumCompaniesWorked) 


####PercentSalaryHike
master_frame$PercentSalaryHike<- scale(master_frame$PercentSalaryHike) 


### StockOptionLevel????
master_frame$StockOptionLevel<- scale(master_frame$StockOptionLevel) 


###TotalWorkingYears
master_frame$TotalWorkingYears<- scale(master_frame$TotalWorkingYears) 


###TrainingTimesLastYear
master_frame$TrainingTimesLastYear<- scale(master_frame$TrainingTimesLastYear) 


###YearsAtCompany
master_frame$YearsAtCompany<- scale(master_frame$YearsAtCompany) 


###YearsSinceLastPromotion
master_frame$YearsSinceLastPromotion<- scale(master_frame$YearsSinceLastPromotion) 



###YearsWithCurrManager
master_frame$YearsWithCurrManager<- scale(master_frame$YearsWithCurrManager) 


###averageHrs
master_frame$averageHrs<- scale(master_frame$averageHrs) 

str(master_frame)


######CREATION OF DUMMY VARIABLES #######################################################

### FOR 2 LEVELS We will directly convert.

###Attrition

 master_frame$Attrition<- ifelse(master_frame$Attrition == "Yes", 1,0)

##Gender

 master_frame$Gender<-ifelse(master_frame$Gender == "Female",1,0)

 
master_chr<- master_frame[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","BusinessTravel","Department","EducationField","Education","JobRole","MaritalStatus","JobInvolvement","JobLevel","PerformanceRating")]

# converting categorical attributes to factor
master_fact<- data.frame(sapply(master_chr, function(x) factor(x)))
str(master_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_fact))[,-1]))


# Final dataset ---

master_frame<- master_frame[,-which(names(master_frame)%in% c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","BusinessTravel","Department","EducationField","Education","JobRole","MaritalStatus","JobInvolvement","JobLevel","PerformanceRating"))]
master_frame<- cbind(master_frame,dummies) 

str(master_frame)


##################################MODELLING###########################################################################
###CREATION OF TRAIN AND TEST DATA.

set.seed(100)

indices = sample.split(master_frame$Attrition, SplitRatio = 0.7)

train = master_frame[indices,]

test = master_frame[!(indices),]


########################################################################################################################
# Logistic Regression: 
#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)#AIC 2146

# Stepwise selection

model_2<- stepAIC(model_1, direction="both")


summary(model_2)#AIC 2120.4
sort(vif(model_2))

# There are very few variables with value of VIF greater than 2 but all such variables have low p-value. 
# Hence removing EducationField.xOther because it has maximum p-value.

model_3<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales + Education.x3 + 
                               Education.x4 + Education.x5 + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.x3 + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_3)#AIC 2120.4
sort(vif(model_3))

# Removing MonthlyIncome because it has high p-value among all the variables

model_4<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales + Education.x3 + 
                               Education.x4 + Education.x5 + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.x3 + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_4)#AIC 2120.9
sort(vif(model_4))

# Removing Education.x5 because it has high p-value among all the variables

model_5<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales + Education.x3 + 
                               Education.x4 + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.x3 + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_5)#AIC 2121.9
sort(vif(model_5))

# Removing Education.x3 because it has high p-value among all the variables

model_6<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + Education.x4 + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.x3 + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_6)#AIC 2122.1
sort(vif(model_6))

# Removing Education.x4 because it has high p-value among all the variables

model_7<- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.x3 + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_7)#AIC 2121.5
sort(vif(model_7))

# Removing DistanceFromHome because it has high p-value among all the variables

model_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + JobInvolvement.x3 + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_8)#AIC 2122.7
sort(vif(model_8))

# Removing JobInvolvement.x3 because it has high p-value among all the variables

model_9<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle + 
                               JobLevel.x2, family = "binomial", data = train)

summary(model_9)#AIC 2125.5
sort(vif(model_9))

# Removing JobLevel.x2 because it has high p-value among all the variables

model_10<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xLaboratory.Technician + 
                               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_10)#AIC 2128.6
sort(vif(model_10))

# Removing JobRole.xLaboratory.Technician because it has high p-value among all the variables

model_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_11)#AIC 2131.8
sort(vif(model_11))

# Removing JobRole.xResearch.Scientist because it has high p-value among all the variables

model_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_12)#AIC 2135.1
sort(vif(model_12))


# Removing JobRole.xResearch.Director because it has high p-value among all the variables

model_13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_13)#AIC 2141.3
sort(vif(model_13))

# Removing JobRole.xSales.Executive because it has high p-value among all the variables

model_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_14)#AIC 2147.3
sort(vif(model_14))


# Removing JobSatisfaction.x3 because it has high p-value among all the variables

model_15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_15)#AIC 2154
sort(vif(model_15))

# Removing JobSatisfaction.x2 because it has high p-value among all the variables

model_16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_16)#AIC 2158.4
sort(vif(model_16))

# Removing TrainingTimesLastYear because it has high p-value among all the variables

model_17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_17)#AIC 2164.3
sort(vif(model_17))

# Removing WorkLifeBalance.x4 because it has high p-value among all the variables

model_18<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                               BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_18)#AIC 2171.6
sort(vif(model_18))

# Removing WorkLifeBalance.x2 because it has high p-value among all the variables

model_19<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                               YearsSinceLastPromotion + YearsWithCurrManager + averageHrs + 
                               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                               EnvironmentSatisfaction.x4 + 
                               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                               BusinessTravel.xTravel_Frequently + 
                               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                               Department.xSales  + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_19)#AIC 2177.4
sort(vif(model_19))


########################################################################
# With 16 significant variables in the model

final_model<- model_19

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Atrrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

#######################################################################

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.001541 to 0.871217 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.011)]
View(cutoff)

# Let's choose cutoff value of 0.1536 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1536, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

View(test)
##################################################################################################

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)



#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.4614135

# Lift & Gain Chart
# plotting the lift chart
# Loading dplyr package
# Lift & Gain Chart
# plotting the lift chart

# Loading dplyr package 
require(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

#A good model is one for which the KS statistic:

#1. is equal to 40% or more
#2. lies in the top deciles, i.e. 1st, 2nd, 3rd or 4th


############CORRELATION FOR FINAL MODEL VARIABLES ##########################################################################################################
library(corrplot)
corplot_data<- cor(master_frame[,which(names(master_frame) %in% c("Attrition","Age", "NumCompaniesWorked" ,
                                                                  "TotalWorkingYears" , "YearsSinceLastPromotion" ,
                                                                  "YearsWithCurrManager" , "averageHrs" ,"EnvironmentSatisfaction.x2" , 
                                                                  "EnvironmentSatisfaction.x3" ,"EnvironmentSatisfaction.x4" , 
                                                                  "JobSatisfaction.x4" , "WorkLifeBalance.x3" , "BusinessTravel.xTravel_Frequently" ,
                                                                  "BusinessTravel.xTravel_Rarely" , "Department.xResearch...Development" , "Department.xSales"  , 
                                                                  "MaritalStatus.xSingle"))])

corrplot(corplot_data, method="circle")
View(corplot_data)

###########CONCLUSION#################################################################################################


###Higher the values of Environment Satisfaction, Job Satisfaction and Work life balance, the better the chances of employees to stay in the company.

###Attrition is more for the employees if he has higher the values of average working hours.

###If employees business travel is rarely then that is also strong predictor of attrition.

###If number of companies worked is increasing then attrition rate is also increasing.

###If  Age, total working years, years since last promotion and years with current manager are increasing then attrition rate is decreasing.

###Attrition is more in the Research & development and Sales department. 

###Attrition is more for the employees who have marital status single.

##########################################################################################################################