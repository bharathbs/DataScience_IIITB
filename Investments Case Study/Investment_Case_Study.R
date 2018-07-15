##########################################################################################################################
#################################                                #########################################################
################################# INVESTMENT CASE STUDY SOLUTION #########################################################
#################################                                #########################################################
##### TEAM MEMBERS #######################################################################################################
##### BHARATH KUMAR B.S ##################################################################################################
##### MUKUL TAMTA       ##################################################################################################
##### SURYA             ##################################################################################################
##### VARUN  AHALAWAT   ##################################################################################################
##########################################################################################################################
##########################################################################################################################


######## CHECK POINT 1 ###################################################################################################

######## DATA CLEANING 1 #################################################################################################

##### Load The COMPANIES DATA txt file into DataFrame
companies<-read.delim2("companies.txt",stringsAsFactors = F)


##### Load the Rounds Data file into rounds Dataframe
rounds2<- read.csv("rounds2.csv",stringsAsFactors = F)


##### Understand the Data Set

##### 1. Number of unique companies present in rounds2.
## Convert all entries under company_permalink column to lower case to find unique enteries

rounds2$company_permalink <- tolower(rounds2$company_permalink)

unique_rounds_company <- length(unique(rounds2$company_permalink))

#### Display the result
unique_rounds_company


##### 2,Number of unique companies present in companies
## Convert all enteries under permalink column to lower case to find unique enteries

companies$permalink <- tolower(companies$permalink)

unique_companies <- length(unique(companies$permalink))

#### Display the result
unique_companies


##### 4.Are there any companies in the rounds2 file which are not present in companies 



