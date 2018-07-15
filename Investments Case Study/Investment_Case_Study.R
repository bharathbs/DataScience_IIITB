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

num_unique_company_permalink <- length(unique(rounds2$company_permalink))

#### Display the result
num_unique_company_permalink 


##### 2.Number of unique companies present in companies
## Convert all enteries under permalink column to lower case to find unique enteries

companies$permalink <- tolower(companies$permalink)

num_unique_permalink <- length(unique(companies$permalink))

#### Display the result
num_unique_permalink


##### 3. In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
## Permalink column has to be the unique key since other columns have N/A or blank Values


##### 4.Are there any companies in the rounds2 file which are not present in companies 





##### 5.Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.
#####   How many observations are present in master_frame ?



