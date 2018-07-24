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
# install.packages("stringr")
library(tidyr)
library(dplyr)
library(stringr)

######## CHECK POINT 1 ###################################################################################################

######## DATA CLEANING 1  ####
 
# Load The COMPANIES DATA txt file into DataFrame ####
companies<-read.delim2("https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt",stringsAsFactors = F)



##### Load the Rounds Data file into rounds Dataframe ####
rounds2<- read.csv("https://cdn.upgrad.com/UpGrad/temp/4c3b5ed0-e5dc-4838-89a2-173d8707d857/rounds2.csv",stringsAsFactors = F)



##### Understand the Data Set

##### 1. Number of unique companies present in rounds2. ####
## Convert all entries under company_permalink column to lower case to find unique enteries

rounds2$company_permalink <- tolower(rounds2$company_permalink)
num_unique_company_permalink <- length(unique(rounds2$company_permalink))

## Display the result
num_unique_company_permalink 




##### 2.Number of unique companies present in companies ####
## Convert all enteries under permalink column to lower case to find unique enteries

companies$permalink <- tolower(companies$permalink)
num_unique_permalink <- length(unique(companies$permalink))

## Display the result
num_unique_permalink



##### 3. In the companies data frame, which column can be used as the unique key for each company?453 ####
##### Write the name of the column.

## Permalink column has to be the unique key since other columns have N/A or blank Values





##### 4.Are there any companies in the rounds2 file which are not present in companies  ####

differenceInSets <- (!rounds2$company_permalink %in% companies$permalink)

##Display the result.
length(which(differenceInSets=='FALSE'))




##### 5.Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.
#####   How many observations are present in master_frame ? 

master_frame <- merge(x=rounds2,y=companies,by.x=c("company_permalink"),by.y = c("permalink"))

#### Find the no of observations ..#114949 obs of 15 variables  ####
str(master_frame)



######CHECK POINT 1 ENDS###############################################################################################################################################


######CHECK POINT 2 ###################################################################################################################################################

###### Average Values of Investments for Each of these Funding Types####
### Since we have to calculate average funding amount of different type,we can create a function instead of repeating same code.

average_funding_amount<-function(funding_type)
{
  filter_funding_type <- filter(master_frame,funding_round_type==funding_type)  
  avg_funding_amount <- summarise(filter_funding_type,mean(raised_amount_usd,na.rm=T))
  avg_funding_amount
}


####1.Average funding amount of venture type####

avg_venture_type_amnt <- average_funding_amount("venture")
avg_venture_type_amnt



####2.Average funding amount of angel type####

avg_angel_type_amnt <- average_funding_amount("angel")
avg_angel_type_amnt



####3.Average funding amount of seed type####

avg_seed_type_amnt <- average_funding_amount("seed")
avg_seed_type_amnt



####4.Average funding amount of Private Equity####

avg_private_equity_amnt <- average_funding_amount("private_equity")
avg_private_equity_amnt


####5. Venture Type is most suitable since the average funding amount is in between expected 5 to 15 millions USD 


######CHECK POINT 2 ENDS###############################################################################################################################################


######CHECK POINT 3 ###################################################################################################################################################

####Country Analysis####

####Step 1 -- To find the top 9 countries which have recieved highest total funding (across ALL sectors for the chosen investment type)####
#### In our case chosen investment type is Venture Type####

#### Filter the data for venture type investment and also remove ####
venture_type_master_frame <- filter(master_frame,funding_round_type=="venture")


#### group by country code and summarise to find the sum of funding for each country####
country_group <- group_by(venture_type_master_frame,country_code)
country_total_funds <- summarise(country_group,sum(raised_amount_usd,na.rm=T))
names(country_total_funds)<- c("Country_Code","Total_Funding")


#### Order the country according to Total_Funding####
country_total_funds<-arrange(country_total_funds,desc(Total_Funding))

#### Extracting top 9 countries (Considering nulls as a country name.Do we need to do consider it??)####
top9 <-  head(country_total_funds,9)
top9$Country_Code[3]<-"Other_Countries"
top9



######CHECK POINT 3 ENDS ###############################################################################################################################################


######CHECK POINT 4 ###################################################################################################################################################

#### SECTOR ANALYSIS####

####1.Extract the primary sector of each category list from the category_list column####
category_list<- strsplit(master_frame$category_list,"\\|")
primary_Sector<- sapply(category_list,function(x) x[1])
master_frame_with_primary_sector <- cbind(master_frame,primary_Sector)

####2.Map each of the primary sector with main sector ####

##Read mapping file

mapping_df = read.csv("https://cdn.upgrad.com/UpGrad/temp/231dc91c-0642-470d-a362-29ddcd7142ce/mapping.csv")

## Convert wide format into long format
## Make key value pair for category list with main sector.
## After conversion only have key value mappings.Ignore others.

mapping_sector_data<-gather(mapping_df,main_sector,is_sector,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping_sector_data<- mapping_sector_data[!(mapping_sector_data$is_sector==0),]


## Now map primary sector with main sector

master_frame_with_primary_sector$primary_Sector<-tolower(master_frame_with_primary_sector$primary_Sector)
mapping_sector_data$category_list<-tolower(mapping_sector_data$category_list)

## merged data frame with each primary sector mapped to its main sector 
master_frame_with_primary_sector<- merge(x=master_frame_with_primary_sector,y=mapping_sector_data,by.x=c("primary_Sector"),by.y =c("category_list"),all.x = TRUE)



######CHECK POINT 4 ENDS###############################################################################################################################################



######CHECK POINT 5 ###################################################################################################################################################

####Sector Analysis 2####


#### Create three separate data frames D1, D2 and D3 for each of the three countries 
#### containing the observations of funding type FT falling within the 5-15 million USD range. 

#### Remove is_sector column since we need to have only Master_Frame with Primary Sector and Main Sector
master_frame_with_primary_sector$is_sector<-NULL

# Filter DataFrame for  FT as Venture and 5-15 million uSD.

master_frame_with_primary_sector <- subset(master_frame_with_primary_sector,funding_round_type == "venture" 
                                                                            & raised_amount_usd <= 15000000 
                                                                            & raised_amount_usd >= 5000000)
                    


#### Top 1 Country -- USA
#### Dataframe for USA with FT  as venture and 5-15 million USD.

top1_d1<- subset(master_frame_with_primary_sector,country_code==top9$Country_Code[1])


#The total number (or count) of investments for each main sector in a separate column
top1_d2<- top1_d1 %>% group_by(main_sector) %>% summarise(total_number=n()) %>% arrange(desc(total_number))
top1_d2 

# The total amount invested in each main sector in a separate column

top1_d3<- top1_d1 %>% group_by(main_sector) %>% summarise(total_amount = sum(raised_amount_usd)) %>% arrange(desc(total_amount))

top1_d3


# Total number of investments
top1_count_investments <- top1_d2 %>% summarise(sum(total_number))
top1_count_investments

# Total amount of investments
top1_amount_investments <- top1_d3 %>% summarise(sum(total_amount))
top1_amount_investments


# Top 2 Country --- Other Countries
# Dataframe for Other Countries with FT  as venture and 5-15 million USD.

master_frame$country_code[which(master_frame$country_code=="")]="Other Countries"

top2_d1<- subset(master_frame_with_primary_sector,country_code=="")


#The total number (or count) of investments for each main sector in a separate column
top2_d2<- top2_d1 %>% group_by(main_sector) %>% summarise(total_number=n()) %>% arrange(desc(total_number))
top2_d2 

# The total amount invested in each main sector in a separate column

top2_d3<- top2_d1 %>% group_by(main_sector) %>% summarise(total_amount = sum(raised_amount_usd)) %>% arrange(desc(total_amount))

top2_d3


# Total number of investments
top2_count_investments <- top2_d2 %>% summarise(sum(total_number))
top2_count_investments

# Total amount of investments
top2_amount_investments <- top2_d3 %>% summarise(sum(total_amount))
top2_amount_investments








