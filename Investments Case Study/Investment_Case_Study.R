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

##INSTALL AND LOAD PACKAGES
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("countrycode")
library(tidyr)
library(dplyr)
library(stringr)
library(countrycode) 


######## CHECK POINT 1 ###################################################################################################

######## DATA CLEANING 1  ##

# Load The COMPANIES DATA txt file into DataFrame ##
companies <- read.delim("companies.txt",sep="\t",stringsAsFactors = FALSE)



## Load the Rounds Data file into rounds Dataframe ##
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = FALSE)



## Understand the Data Set

## 1. Number of unique companies present in rounds2. ##
## Convert all entries under company_permalink column to lower case to find unique enteries

rounds2$company_permalink <- tolower(rounds2$company_permalink)
num_unique_company_permalink <- length(unique(rounds2$company_permalink))

## Display the result
num_unique_company_permalink 




## 2.Number of unique companies present in companies ##
## Convert all enteries under permalink column to lower case to find unique enteries

companies$permalink <- tolower(companies$permalink)
num_unique_permalink <- length(unique(companies$permalink))

## Display the result
num_unique_permalink



## 3. In the companies data frame, which column can be used as the unique key for each company?##
## Write the name of the column.

#ANS:
## We have 66368 observation in Companies Data frame,As per answer 2 we have found that we have 66368 unique entries for Permalink column.
## Hence Permalink is a unique key in companies data frame.


## 4.Are there any companies in the rounds2 file which are not present in companies  ##

differenceInSets <- (!rounds2$company_permalink %in% companies$permalink)

##Display the result.
length(which(differenceInSets=='TRUE'))




## 5.Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.
##   How many observations are present in master_frame ? 

master_frame <- merge(x=rounds2,y=companies,by.x=c("company_permalink"),by.y = c("permalink"))


## Find the no of observations ..#114949 obs of 15 variables  ##
str(master_frame)



###CHECK POINT 1 ENDS###############################################################################################################################################


######CHECK POINT 2 ###################################################################################################################################################

###### Average Values of Investments for Each of these Funding Types##
## Since we have to calculate average funding amount of different type,we can create a function instead of repeating same code.

average_funding_amount<-function(funding_type)
{
  avg_funding_amount <- master_frame %>% filter(funding_round_type==funding_type) %>%
                        summarise(Average_Funding_Amount=mean(raised_amount_usd,na.rm=T))
}





##1.Average funding amount of venture type##

avg_venture_type_amnt <- average_funding_amount("venture")
avg_venture_type_amnt



##2.Average funding amount of angel type##

avg_angel_type_amnt <- average_funding_amount("angel")
avg_angel_type_amnt



##3.Average funding amount of seed type##

avg_seed_type_amnt <- average_funding_amount("seed")
avg_seed_type_amnt



##4.Average funding amount of Private Equity##

avg_private_equity_amnt <- average_funding_amount("private_equity")
avg_private_equity_amnt


##5. Venture Type is most suitable since the average funding amount is in between expected 5 to 15 millions USD 
master_frame %>% group_by(funding_round_type)%>% 
summarize(Averagefunding=mean(raised_amount_usd,na.rm=T)) %>%
filter(Averagefunding >= 5000000 & Averagefunding <= 15000000)
            

######CHECK POINT 2 ENDS###############################################################################################################################################


######CHECK POINT 3 ###################################################################################################################################################

##Country Analysis##

##Step 1 -- To find the top 9 countries which have recieved highest total funding (across ALL sectors for the chosen investment type)####
## In our case chosen investment type is Venture Type##

##1. Filter the data for venture type investment ##
##2. group by country code and summarise to find the sum of funding for each country ##
##3. Order the country according to Total_Funding##

country_total_funds <- master_frame %>% filter(funding_round_type=="venture" & str_length(country_code)>0) %>%
                       group_by(country_code)  %>%
                       summarise(Total_Funding=sum(raised_amount_usd,na.rm = T)) %>%
                       arrange(desc(Total_Funding))

## Extracting top 9 countries ####
top9 <-  head(country_total_funds,9)
top9
colnames(top9)<-c("CountryCode","Total_Funding")

## Converting country code to country name
## Comparing Country Names obtained with the pdf file provided to us in Upgrad Module.
mycode <- top9$CountryCode 
countrycode(mycode, origin="iso3c", destination="country.name") 

## Hence top 3 english speaking countries are as below
## United States (USA)
## United Kingdom (GBR)
## India (IND)
top3 <- c(top9[1,1],top9[3,1],top9[4,1])
top3

######CHECK POINT 3 ENDS ###############################################################################################################################################

######CHECK POINT 4 ###################################################################################################################################################

## SECTOR ANALYSIS##

##1.Extract the primary sector of each category list from the category_list column##
mf1 <- master_frame
mf1 <- separate(mf1,category_list,into=c("primary_sector"),sep="\\|",remove = FALSE)

##2.Map each of the primary sector with main sector ##
##Read mapping file

mapping <- read.csv("mapping.csv")


## Convert wide format into long format
## Make key value pair for category list with main sector.
## After conversion only have key value mappings.Ignore others.

mapping_1 <- gather(mapping,main_sector,val,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping_1 <- mapping_1[!(mapping_1$val==0),]
mapping_1 <- mapping_1[,-3]
mapping_1 <- mapping_1[!(mapping_1$main_sector=="Blanks"),]

## Converting primary sectors and category list to lower case before merging

mf1$primary_sector<-tolower(mf1$primary_sector)
mapping_1$category_list<-tolower(mapping_1$category_list)

## merged data frame with each primary sector mapped to its main sector

master_frame_with_primary_sector <- merge(mf1,mapping_1,by.x=c('primary_sector'),by.y=c('category_list'))

View(master_frame_with_primary_sector)

######CHECK POINT 4 ENDS###############################################################################################################################################



######CHECK POINT 5 ###################################################################################################################################################

##Sector Analysis 2##


## Create three separate data frames D1, D2 and D3 for each of the three countries 
## containing the observations of funding type FT falling within the 5-15 million USD range. 


## Filter DataFrame for  FT as Venture and 5-15 million USD.
## The case study statement clearly states sector analysis has to be in one of eight main sectors.
## Hence Remove Main Sectors where we have NA.

master_frame_with_primary_sector <- subset(master_frame_with_primary_sector,funding_round_type == "venture" 
                                           & !is.na(main_sector)
                                           & raised_amount_usd <= 15000000 
                                           & raised_amount_usd >= 5000000)



###### Top 1 Country -- United States (USA) #########################################################################################################################
## Dataframe for USA with FT  as venture and 5-15 million USD.

top1_d1<- subset(master_frame_with_primary_sector,country_code==top3[1])


#The total number (or count) of investments for each main sector in a separate column

top1_d2<- top1_d1 %>% group_by(main_sector) %>% 
          summarise(total_number=n()) %>% 
          arrange(desc(total_number))


top1_d2 

## The total amount invested in each main sector in a separate column

top1_d3<- top1_d1 %>% group_by(main_sector) %>% 
          summarise(total_amount = sum(raised_amount_usd)) %>% 
          arrange(desc(total_amount))

top1_d3


## 1. Total number of investments
top1_count_investments <- top1_d2 %>% summarise(sum(total_number))
top1_count_investments

## 2. Total amount of investments
top1_amount_investments <- top1_d3 %>% summarise(sum(total_amount))
top1_amount_investments

## 3.Top Sector name (no. of investment-wise)
top1_d2$main_sector[1]

## 4.Secpnd Sector name (no. of investment-wise)
top1_d2$main_sector[2]


## 5.Third Sector name (no. of investment-wise)
top1_d2$main_sector[3]

## 6.Number of investments in top sector (3)
top1_d2$total_number[1]

## 7.Number of investments in second sector (4)
top1_d2$total_number[2]

## 8.Number of investments in second sector (5)
top1_d2$total_number[3]


## 9.Which company received the highest investment in top Sector
top_cmpny_sector_count_wise <- top1_d1 %>% filter(main_sector==top1_d2$main_sector[1]) %>% 
                               group_by(company_permalink,name) %>% 
                               summarise(total_amount=sum(raised_amount_usd)) %>% 
                               arrange(desc(total_amount))

top_cmpny_sector_count_wise[1,2] 


#10.which company received the highest investment in 2nd top Sector
second_top_cmpny_sector_count_wise <- top1_d1 %>% filter(main_sector==top1_d2$main_sector[2]) %>% 
                                      group_by(company_permalink,name) %>% 
                                      summarise(total_amount=sum(raised_amount_usd)) %>% 
                                      arrange(desc(total_amount))

second_top_cmpny_sector_count_wise[1,2]

  
###### Top 1 Country -- United States (USA) Done ######################################################################################################################


###### Top 2 Country -- United Kingdom (GBR) ##########################################################################################################################
## Dataframe for Other Countries with FT  as venture and 5-15 million USD.

top2_d1<- subset(master_frame_with_primary_sector,country_code==top3[2])


##The total number (or count) of investments for each main sector in a separate column
top2_d2<- top2_d1 %>% group_by(main_sector) %>% 
	    	  summarise(total_number=n()) %>% 
		      arrange(desc(total_number))
  
top2_d2 

## The total amount invested in each main sector in a separate column

top2_d3<- top2_d1 %>% group_by(main_sector) %>% 
	        summarise(total_amount = sum(raised_amount_usd)) %>% 	
		      arrange(desc(total_amount))

top2_d3


## 1.Total number of investments
top2_count_investments <- top2_d2 %>% summarise(sum(total_number))
top2_count_investments

## 2.Total amount of investments
top2_amount_investments <- top2_d3 %>% summarise(sum(total_amount))
top2_amount_investments


## 3.Top Sector name (no. of investment-wise)
top2_d2$main_sector[1]

## 4.Secpnd Sector name (no. of investment-wise)
top2_d2$main_sector[2]


## 5.Third Sector name (no. of investment-wise)
top2_d2$main_sector[3]

## 6.Number of investments in top sector (3)
top2_d2$total_number[1]

## 7.Number of investments in second sector (4)
top2_d2$total_number[2]

## 8.Number of investments in second sector (5)
top2_d2$total_number[3]


##9. Which company received the highest investment in top Sector
top_cmpny_sector_count_wise <- top2_d1 %>% filter(main_sector==top2_d2$main_sector[1]) %>% 
                               group_by(company_permalink,name) %>% 
                               summarise(total_amount=sum(raised_amount_usd)) %>% 
                               arrange(desc(total_amount))

top_cmpny_sector_count_wise[1,2] 


#10.which company received the highest investment in 2nd top Sector
second_top_cmpny_sector_count_wise <- top2_d1 %>% filter(main_sector==top2_d2$main_sector[2]) %>% 
                                      group_by(company_permalink,name) %>% 
                                      summarise(total_amount=sum(raised_amount_usd)) %>% 
                                      arrange(desc(total_amount))

second_top_cmpny_sector_count_wise[1,2]


###### Top 2 Country -- United Kingdom (GBR) Done #####################################################################################################################



###### Top 3 Country -- India (IND)###################################################################################################################################
## Dataframe for Other Countries with FT  as venture and 5-15 million USD.
top3_d1<- subset(master_frame_with_primary_sector,country_code==top3[3])


##The total number (or count) of investments for each main sector in a separate column
top3_d2<- top3_d1 %>% group_by(main_sector) %>% 
          summarise(total_number=n()) %>% 
          arrange(desc(total_number))
top3_d2 

##The total amount invested in each main sector in a separate column
top3_d3<- top3_d1 %>% group_by(main_sector) %>% 
          summarise(total_amount = sum(raised_amount_usd)) %>% 
          arrange(desc(total_amount))

top3_d3


##1. Total number of investments
top3_count_investments <- top3_d2 %>% summarise(sum(total_number))
top3_count_investments

##2.Total amount of investments
top3_amount_investments <- top3_d3 %>% summarise(sum(total_amount))
top3_amount_investments


## 3.Top Sector name (no. of investment-wise)
top3_d2$main_sector[1]

## 4.Secpnd Sector name (no. of investment-wise)
top3_d2$main_sector[2]


## 5.Third Sector name (no. of investment-wise)
top3_d2$main_sector[3]

## 6.Number of investments in top sector (3)
top3_d2$total_number[1]

## 7.Number of investments in second sector (4)
top3_d2$total_number[2]

## 8.Number of investments in second sector (5)
top3_d2$total_number[3]



##9. Which company received the highest investment in top Sector
top_cmpny_sector_count_wise <- top3_d1 %>% filter(main_sector==top3_d2$main_sector[1]) %>% 
                               group_by(company_permalink,name) %>% 
                               summarise(total_amount=sum(raised_amount_usd)) %>% 
                               arrange(desc(total_amount))

top_cmpny_sector_count_wise[1,2] 


#10.which company received the highest investment in 2nd top Sector
second_top_cmpny_sector_count_wise <- top3_d1 %>% filter(main_sector==top3_d2$main_sector[2]) %>% 
                                      group_by(company_permalink,name) %>% 
                                      summarise(total_amount=sum(raised_amount_usd)) %>% 
                                      arrange(desc(total_amount))

second_top_cmpny_sector_count_wise[1,2]


######CHECK POINT 5 ENDS###############################################################################################################################################

