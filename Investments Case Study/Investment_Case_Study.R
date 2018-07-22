##########################################################################################################################
#################################                                #########################################################
################################# INVESTMENT CASE STUDY SOLUTION #########################################################
#################################                                #########################################################
##### TEAM MEMBERS #######################################################################################################
##### BHARATH KUMAR B.S      #############################################################################################
##### MUKUL TAMTA            #############################################################################################
##### SURYA PRAKASH TRIPATHI #############################################################################################
##### VARUN  AHALAWAT        #############################################################################################
##########################################################################################################################
##########################################################################################################################

####INSTALL AND LOAD PACKAGES
# install.packages("tidyr")
# install.packages("dplyr")
library(tidyr)
library(dplyr)


######## CHECK POINT 1 ###################################################################################################

######## DATA CLEANING 1 

##### Load The COMPANIES DATA txt file into DataFrame
companies<-read.delim2("https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt",stringsAsFactors = F)



##### Load the Rounds Data file into rounds Dataframe
rounds2<- read.csv("https://cdn.upgrad.com/UpGrad/temp/4c3b5ed0-e5dc-4838-89a2-173d8707d857/rounds2.csv",stringsAsFactors = F)



##### Understand the Data Set

##### 1. Number of unique companies present in rounds2.
## Convert all entries under company_permalink column to lower case to find unique enteries

rounds2$company_permalink <- tolower(rounds2$company_permalink)
num_unique_company_permalink <- length(unique(rounds2$company_permalink))

## Display the result
num_unique_company_permalink 




##### 2.Number of unique companies present in companies
## Convert all enteries under permalink column to lower case to find unique enteries

companies$permalink <- tolower(companies$permalink)
num_unique_permalink <- length(unique(companies$permalink))

## Display the result
num_unique_permalink



##### 3. In the companies data frame, which column can be used as the unique key for each company?453
##### Write the name of the column.

## Permalink column has to be the unique key since other columns have N/A or blank Values





##### 4.Are there any companies in the rounds2 file which are not present in companies 

differenceInSets <- (!rounds2$company_permalink %in% companies$permalink)

##Display the result.
length(which(differenceInSets=='FALSE'))




##### 5.Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.
#####   How many observations are present in master_frame ?

master_frame <- merge(x=rounds2,y=companies,by.x=c("company_permalink"),by.y = c("permalink"))

#### Find the no of observations ..#114949 obs of 15 variables
str(master_frame)



######CHECK POINT 1 ENDS###############################################################################################################################################


######CHECK POINT 2 ###################################################################################################################################################

###### Average Values of Investments for Each of these Funding Types
### Since we have to calculate average funding amount of different type,we can create a function instead of repeating same code.

average_funding_amount<-function(funding_type)
{
  filter_funding_type <- filter(master_frame,funding_round_type==funding_type)  
  avg_funding_amount <- summarise(filter_funding_type,mean(raised_amount_usd,na.rm=T))
  avg_funding_amount
}


####1.Average funding amount of venture type

avg_venture_type_amnt <- average_funding_amount("venture")
avg_venture_type_amnt



####2.Average funding amount of angel type

avg_angel_type_amnt <- average_funding_amount("angel")
avg_angel_type_amnt



####3.Average funding amount of seed type

avg_seed_type_amnt <- average_funding_amount("seed")
avg_seed_type_amnt



####4.Average funding amount of Private Equity

avg_private_equity_amnt <- average_funding_amount("private_equity")
avg_private_equity_amnt


####5. Venture Type is most suitable since the average funding amount is in between expected 5 to 15 millions USD.


######CHECK POINT 2 ENDS###############################################################################################################################################


######CHECK POINT 3 ###################################################################################################################################################

####Country Analysis

####Step 1 -- To find the top 9 countries which have recieved highest total funding (across ALL sectors for the chosen investment type)
#### In our case chosen investment type is Venture Type

#### Filter the data for venture type investment and also remove 
venture_type_master_frame <- filter(master_frame,funding_round_type=="venture")


#### group by country code and summarise to find the sum of funding for each country
country_group <- group_by(venture_type_master_frame,country_code)
country_total_funds <- summarise(country_group,sum(raised_amount_usd,na.rm=T))
names(country_total_funds)<- c("Country_Code","Total_Funding")


#### Order the country according to Total_Funding
country_total_funds<-arrange(country_total_funds,desc(Total_Funding))

#### Extracting top 9 countries (Considering # missing as a country name
top9 <-  head(country_total_funds,9)
top9$Country_Code[3]<-"Other_Countries"
top9



######CHECK POINT 3 ENDS ###############################################################################################################################################


######CHECK POINT 4 ###################################################################################################################################################

#### SECTOR ANALYSIS


