  ##########################################################################################################################
  #################################                                #########################################################
  #################################      CAPSTONE BFSI PROJECT     #########################################################
  #################################                                #########################################################
  ##### TEAM MEMBERS #######################################################################################################
  ##### BHARATH KUMAR B.S      #############################################################################################
  ##### MUKUL TAMTA            #############################################################################################
  ##### SURYA PRAKASH TRIPATHI #############################################################################################
  ##### VARUN  AHALAWAT        #############################################################################################
  ##########################################################################################################################
  ##########################################################################################################################
  
  # Importing Libraries
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(DT)
  library(tidyr)
  library(corrplot)
  library(leaflet)
  library(lubridate)
  library(scales)
  library(Hmisc)
  library(gridExtra)
  library(Information)
  library(ROCR)
  library(randomForest)
  library(MASS) 
  library(Rcpp) 
  library(car)
  library(e1071) 
  library(stringi) 
  library(glue)
  library(caret)
  library(cowplot) 
  library(caTools) 
  library(GGally) 
  library(dimRed)
  library(reshape2)
  library(corrplot)
  library(Hmisc)
  library(pROC)
  library(RColorBrewer)
  
  
  options("scipen"=100, "digits"=4)
  
  ######################################################################################################################
  
  ########################################################################################################################
  
  ######## IMPORTING DATASETS ########################################################################################
  
  ######################################################################################################################
  
  
  #Read the data files and convert empty values in the dataset to NA
  
  credit_data <- read.csv("Credit Bureau data.csv",na.strings = c("", "NA"))
  demographic_data <- read.csv("Demographic data.csv",na.strings = c("", "NA"))
  
  #Count the rows in the data set
  nrow(demographic_data)
  # There are 71295 records
  nrow(credit_data)
  # There are 71295 records
  
  # COunt the number of unique records in Application ID
  length(unique(demographic_data$Application.ID))
  #71292 unique Application ID
  
  length(unique(credit_data$Application.ID))
  #71292 unique Application ID
  
  ## We have 3 duplicate Application ID's in both the datasets. So we have removed them
  
  demographic_data <- demographic_data[!duplicated(demographic_data$Application.ID),]
  nrow(demographic_data)
  # There are 71292 records
  
  credit_data <- credit_data[!duplicated(credit_data$Application.ID),]
  nrow(credit_data)
  # There are 71292 records
  
  nrow(demographic_data)== length(unique(demographic_data$Application.ID))
  nrow(credit_data)== length(unique(credit_data$Application.ID))
  ## Now there are no Duplicates in Application Id. We will consider this a primary key.
  
  # count the NA values in demographic data
  sum(is.na(demographic_data$Performance.Tag))
  
  # There are 1425 NA values for Performance Tag in demographic data
  
  # count the NA values in credit_data
  sum(is.na(credit_data$Performance.Tag))
  
  # There are 1425 NA values for Performance Tag in credit_data
  
  # Percentage of default in both the datasets
  
  length(which(demographic_data$Performance.Tag==1))/nrow(demographic_data)
  #0.04134
  
  length(which(credit_data$Performance.Tag==1))/nrow(credit_data)
  #0.04134
  
  
  #Summarising NA values in demographic data
  missing_values <- demographic_data %>%
          summarise_all(funs(sum(is.na(.))/n()))
  
  # Calculating missing % in demographic data    
  missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')
  
  #Plotting missing values   
  missing_values %>%
          ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
          geom_bar(stat = 'identity',fill='red') +
          coord_flip()
  
  
  ######################################################################################################################
  
  ########################################################################################################################
  
  ########################        Data Cleaning and Transformation For Dempgraphic Data       #######################
  
  ########################################################################################################################
  
  # We are not performing here outlier treatment.Outlier treatment and NA values treatment we will do by using WOE
  
  # 1. Age
  quantile(demographic_data$Age, seq(0, 1, 0.001))
  nrow(demographic_data)
  
  # There is negative age and also have records for which age is less than 18.Hence replacing records for age less than 18 to 18
  # and removing negative values of age from the dataset.
  
  nrow(filter(demographic_data,demographic_data$Age<0))
  
  # As suggested by mentor if we have any wrong data like negative age values then that we will remove upfront and will not 
  # treat those records.
  
  demographic_data<- demographic_data[-which((demographic_data$Age<0)),]
  nrow(demographic_data)
  
  # Replacing Age between 0-18 with 18
  
  demographic_data$Age[which(demographic_data$Age <= 18)] <- 18
  
  summary(demographic_data$Age)
  nrow(demographic_data)
  
  # Checking if there is any negative value of age is present
  
  nrow(filter(demographic_data,demographic_data$Age<0))
  
  quantile(demographic_data$Age, seq(0, 1, 0.01))
  
  # There is no negative row is present in the age column of the dataset
  
  str(demographic_data)
  
  
  #2.Gender
  
  levels(demographic_data$Gender)
  summary(demographic_data$Gender)
  # We have two levels F and M and 2 NA values
  
  # 3.Marital.Status..at.the.time.of.application
  levels(demographic_data$Marital.Status..at.the.time.of.application)
  summary(demographic_data$Marital.Status..at.the.time.of.application)
  # We have two levels Married and Single and 6 NA values
  
  # 4.No.of.dependents
  
  boxplot(demographic_data$No.of.dependents)
  
  nrow(filter(demographic_data,demographic_data$No.of.dependents<0))
  
  summary(demographic_data$No.of.dependents)
  
  quantile(demographic_data$No.of.dependents, seq(0, 1, 0.01),na.rm = T)
  
  #There are no negative values and 3 NA values are present.
  
  # There are no outliers present.
  
  #5. Income
  
  quantile(demographic_data$Income, seq(0, 1, 0.01))
  
  nrow(filter(demographic_data,demographic_data$Income<0))
  
  # There are negative income values for 81 records.
  # As income cant be negative hence removing these values as suggested by Mentor
  
  # Removing negative income values from the dataset as suggested by Mentor
  demographic_data<- demographic_data[-which((demographic_data$Income<0)),]
  nrow(demographic_data)
  
  # Now there are total 71210 reccords.
  # Checking if there is any negative income values still present
  
  nrow(filter(demographic_data,demographic_data$Income<0))
  # There is no negative value of income
  
  quantile(demographic_data$Income, seq(0, 1, 0.01))
  
  boxplot(demographic_data$Income)
  # Boxplot and quantile both are showing that there is no outlier in income variable.
  
  str(demographic_data)
  
  #6.Education
  
  levels(demographic_data$Education)
  summary(demographic_data$Education)
  nrow(demographic_data)
  
  #There are 5 levels  "Bachelor","Masters","Others","Phd" and "Professional" and 119 NA values
  
  summary(demographic_data$Education)
  
  # 7.Profession
  
  summary(demographic_data$Profession)
  levels(demographic_data$Profession)
  
  #There are 3 levels : "SAL","SE" and "SE_PROF" and 14 NA values
  
  #8.Type.of.residence
  summary(demographic_data$Type.of.residence)
  levels(demographic_data$Type.of.residence)
  
  #There are 5 levels : "Company provided","Living with Parents","Others","Owned" and "Rented"  and 8 NA values
  
  #9. No.of.months.in.current.residence
  
  nrow(filter(demographic_data,demographic_data$No.of.months.in.current.residence<0))
  summary(demographic_data$No.of.months.in.current.residence)
  
  # There is no negative value in the No.of.months.in.current.residence variable
  
  quantile(demographic_data$No.of.months.in.current.residence, seq(0, 1, 0.01))
  
  boxplot(demographic_data$No.of.months.in.current.residence)
  
  #10. No.of.months.in.current.company
  nrow(filter(demographic_data,demographic_data$No.of.months.in.current.company<0))
  summary(demographic_data$No.of.months.in.current.company)
  
  # There is no negative value in the No.of.months.in.current.company variable
  
  quantile(demographic_data$No.of.months.in.current.company, seq(0, 1, 0.01))
  
  #Now checking outlier with boxplot
  
  boxplot(demographic_data$No.of.months.in.current.company)
  
  # There are outlier in the No.of.months.in.current.company variable which we will be treating using WOE transformation.
  
  str(demographic_data)
  
  sum(is.na(demographic_data))
  
  # There are 1577 NA values in the demographic_data dataset
  
  ###########################################################################################
  
  # Creating dataframe of the Rejected population i.e. the records having NA in Performance Tag in demographic dataset
  demographic_rejected <- demographic_data[which(is.na(demographic_data$Performance.Tag)),]
  
  # Let's remove the records which have NA values in target variable from demographic dataset.
  # This is confirmed by TA in https://learn.upgrad.com/v/course/163/question/124008
  
  demographic_data_V1<- demographic_data[-which(is.na(demographic_data$Performance.Tag)),]
  nrow(demographic_data_V1)
  # Now number of records in demographic_data_V1 is 69785 after removing NA values in performance tag
  
  colnames(demographic_data_V1)
  
  # Let's check NA values of each column again to verify if there is any missing values still present
  demographic_data_V1 %>%
    summarise_all(funs(sum(is.na(.))))
  
  # Now We have NA values in follwoing features :Gender,Marital.Status..at.the.time.of.application.,No.of.dependents,Education,Profession and Type.of.residence
  
  ######################################################################################################################
  
  ########################################################################################################################
  ########################################   EDA of demographic data   ###########################################################
  #################################################################################################################################
  
  #Storing demographic_data_V1 in another dataframe for bivariate analysis and this DF will be used only in EDA
  
  demographic_data_V2<-demographic_data_V1
  
  # Bivariate Analysis
  
  #1.Age vs Performance tag
  
  summary(demographic_data_V2$Age)
  
  str(demographic_data_V2)
  demographic_data_V2$Age_bin<-ifelse(demographic_data_V2$Age>=18 & demographic_data_V2$Age<=28 ,"18-28",
                                      ifelse(demographic_data_V2$Age>28 & demographic_data_V2$Age<=38,"28-38",
                                             ifelse(demographic_data_V2$Age>38 & demographic_data_V2$Age<=48 ,"38-48",
                                                    ifelse(demographic_data_V2$Age>48 & demographic_data_V2$Age<=58,"48-58",
                                                           ifelse(demographic_data_V2$Age>58 & demographic_data_V2$Age<=68,"58-68","68+")))))
  
  demographic_data_V2$Age_bin<-factor(demographic_data_V2$Age_bin,levels=c("18-28","28-38","38-48","48-58","58-68","68+"))
  
  ggplot(demographic_data_V2,aes(x=demographic_data_V2$Age_bin,fill=factor(demographic_data_V2$Performance.Tag)))+
          geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))
  
  ggplot(demographic_data_V2,aes(x=demographic_data_V2$Age,fill=factor(demographic_data_V2$Performance.Tag)))+
          geom_bar() +ggtitle("Age Vs Performance Tag") +xlab("Age") + ylab("Count") + labs(fill = "Performance tag")
  
  #2.Gender vs Performance tag
  
  test<-filter(demographic_data_V2,demographic_data_V2$Gender %in% c('F','M'))
  
  #Excluding NA values and taking subset of data which does not have NA values
  
  ggplot(test,aes(x=test$Gender,fill=factor(test$Performance.Tag)))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
          ggtitle("Gender Vs Performance Tag") +xlab("Gender") + ylab("Count") + labs(fill = "Performance tag")
  str(demographic_data_V1)
  
  #3. Marital.Status..at.the.time.of.application. vs performance tag
  
  str(demographic_data_V2$Marital.Status..at.the.time.of.application.)
  
  test2<-filter(demographic_data_V2,demographic_data_V2$Marital.Status..at.the.time.of.application. %in% c('Married','Single'))
  
  
  #Excluding NA values and taking subset of data which does not have NA values
  
  ggplot(test2,aes(x=test2$Marital.Status..at.the.time.of.application.,fill=factor(test2$Performance.Tag)))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
          ggtitle("Age Vs Performance Tag") +xlab("") + ylab("Count") + labs(fill = "Performance tag")
  
  #4.No.of.dependents vs performance tag
  
  subset<-as.data.frame(demographic_data_V2[-which(is.na(demographic_data_V2$No.of.dependents)),])
  #Excluding NA values
  
  ggplot(subset,aes(x=subset$No.of.dependents,fill=factor(subset$Performance.Tag)))+
          geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
          ggtitle("No.of.dependents Vs Performance Tag") +xlab("No.of.dependents") + ylab("Count") + labs(fill = "Performance tag")
  
  #ggplot(demographic_data_V1,aes(x=demographic_data_V1$No.of.dependents,fill=factor(demographic_data_V1$Performance.Tag)))+
  #geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))
  
  
  str(demographic_data_V2)
  
  #5.income vs Performance tag
  summary(demographic_data_V2$Income)
  head(demographic_data_V2$Income)
  
  demographic_data_V2$Income_bin<-ifelse(demographic_data_V2$Income>=0 & demographic_data_V2$Income<=10 ,"0-10K",
                                         ifelse(demographic_data_V2$Income>10 & demographic_data_V2$Income<=20,"10K-20K",
                                                ifelse(demographic_data_V2$Income>20 & demographic_data_V2$Income<=30 ,"20K-30K",
                                                       ifelse(demographic_data_V2$Income>30 & demographic_data_V2$Income<=40,"30K-40K",
                                                              ifelse(demographic_data_V2$Income>40 & demographic_data_V2$Income<=50, "40K-50K",
                                                                     ifelse(demographic_data_V2$Income>50 & demographic_data_V2$Income<=60,"50K-60K","60K+"))))))
  
  demographic_data_V2$Income_bin<-factor(demographic_data_V2$Income_bin,levels=c("0-10K","10K-20K","20K-30K","30K-40K","40K-50K","50K-60K","60K+"))
  
  
  ggplot(demographic_data_V2,aes(x=demographic_data_V2$Income_bin,fill=factor(demographic_data_V2$Performance.Tag)))+
          geom_bar(position = "fill")+
          ggtitle("Income_bin Vs Performance Tag") +xlab("Income_bin") + ylab("Count") + labs(fill = "Performance tag")
  
  # default is decreasing when Income is increasing
  
  #6. Education vs Performance tag
  
  levels(demographic_data_V2$Education)
  test4<-filter(demographic_data_V2,demographic_data_V2$Education %in% c('Bachelor','Masters','Others','Phd','Professional'))
  
  ggplot(test4,aes(x=test4$Education,fill=factor(test4$Performance.Tag)))+
          geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))+
          ggtitle("Education Vs Performance Tag") +xlab("Education") + ylab("Count") + labs(fill = "Performance tag")
  
  #7. Profession vs Performance tag
  levels(demographic_data_V2$Profession)
  test9<-filter(demographic_data_V2,demographic_data_V2$Profession %in% c('SAL','SE','SE_PROF'))
  
  ggplot(test9,aes(x=test9$Profession,fill=factor(test9$Performance.Tag)))+
          geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))+
          ggtitle("Profession Vs Performance Tag") +xlab("Profession") + ylab("Count") + labs(fill = "Performance tag")
  
  str(demographic_data_V2)
  
  
  #8. Type.of.residence vs Performance tag
  
  levels(demographic_data_V2$Type.of.residence)
  test6<-filter(demographic_data_V2,demographic_data_V2$Type.of.residence %in% c('Company provided','Living with Parents','Others','Owned','Rented'))
  
  ggplot(test6,aes(x=test6$Type.of.residence,fill=factor(test6$Performance.Tag)))+
          geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))+
          ggtitle("Type.of.residence Vs Performance Tag") +xlab("Type.of.residence") + ylab("Count") + labs(fill = "Performance tag")
  
  # Records which have Type.of.residence is others have less chances of default as compare to following Type.of.residence: 'Company provided','Living with Parents','Others','Owned','Rented'
  
  
  #9. No.of.months.in.current.residence vs performance tag
  summary(demographic_data_V2$No.of.months.in.current.residence)
  demographic_data_V2$No.of.months.in.current.residence_bin<-ifelse(demographic_data_V2$No.of.months.in.current.residence>=0 & demographic_data_V2$No.of.months.in.current.residence<=21 ,"0-21",
                                                                    ifelse(demographic_data_V2$No.of.months.in.current.residence>21 & demographic_data_V2$No.of.months.in.current.residence<=42,"21-42",
                                                                           ifelse(demographic_data_V2$No.of.months.in.current.residence>42 & demographic_data_V2$No.of.months.in.current.residence<=63 ,"42-63",
                                                                                  ifelse(demographic_data_V2$No.of.months.in.current.residence>63 & demographic_data_V2$No.of.months.in.current.residence<=84,"63-84",
                                                                                         ifelse(demographic_data_V2$No.of.months.in.current.residence>84 & demographic_data_V2$No.of.months.in.current.residence<=105, "84-10K",
                                                                                                ifelse(demographic_data_V2$No.of.months.in.current.residence>105 & demographic_data_V2$No.of.months.in.current.residence<=126,"105-126","126+"))))))
  
  demographic_data_V2$No.of.months.in.current.residence_bin<-factor(demographic_data_V2$No.of.months.in.current.residence_bin,levels=c("0-21","21-42","42-63","63-84","84-105","105-126","126+"))
  
  
  ggplot(demographic_data_V2,aes(x=demographic_data_V2$No.of.months.in.current.residence_bin,fill=factor(demographic_data_V2$Performance.Tag)))+
          geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))+
          ggtitle("No.of.months.in.current.residence_bin Vs Performance Tag") +xlab("No.of.months.in.current.residence_bin") + ylab("Count") + labs(fill = "Performance tag")
  
  #10.No.of.months.in.current.company vs performance tag
  summary(demographic_data_V2$No.of.months.in.current.company)
  
  demographic_data_V2$No.of.months.in.current.company_bin<-ifelse(demographic_data_V2$No.of.months.in.current.company>=0 & demographic_data_V2$No.of.months.in.current.company<=15 ,"0-15",
                                                                  ifelse(demographic_data_V2$No.of.months.in.current.company>15 & demographic_data_V2$No.of.months.in.current.company<=30,"15-30",
                                                                         ifelse(demographic_data_V2$No.of.months.in.current.company>30 & demographic_data_V2$No.of.months.in.current.company<=45 ,"30-45",
                                                                                ifelse(demographic_data_V2$No.of.months.in.current.company>45 & demographic_data_V2$No.of.months.in.current.company<=60,"45-60",
                                                                                       ifelse(demographic_data_V2$No.of.months.in.current.company>60 & demographic_data_V2$No.of.months.in.current.company<=75, "60-75",
                                                                                              ifelse(demographic_data_V2$No.of.months.in.current.company>75 & demographic_data_V2$No.of.months.in.current.company<=90,"75-90","90+"))))))
  
  demographic_data_V2$No.of.months.in.current.company_bin<-factor(demographic_data_V2$No.of.months.in.current.company_bin,levels=c("0-15","15-30","30-45","45-60","60-75","75-90","90+"))
  
  
  
  ggplot(demographic_data_V2,aes(x=demographic_data_V2$No.of.months.in.current.company_bin,fill=factor(demographic_data_V2$Performance.Tag)))+
          geom_bar(position="fill")+geom_text(aes(label=..count..),stat="count",position=position_fill(0.5))+
          ggtitle("No.of.months.in.current.company_bin Vs Performance Tag") +xlab("No.of.months.in.current.company_bin") + ylab("Count") + labs(fill = "Performance tag")
  
  
  # Creating another dataset by copying demographic_data_V1 so that it can be used for merging it with Credit bureau dataset
  
  demographic_data_V3 <- demographic_data_V1
  
  ##################################################################################################################################################################################################
  ##################################################################################################################################################################################################
  ############### Calculating IV and WOE values for only Demographic dataset for Model Building only for Demographic dataset
  ##################################################################################################################################################################################################
  
  #IV and WOE calculation
  
  IV <- Information::create_infotables(data = demographic_data_V1[,!colnames(demographic_data_V1) %in%("Application.ID")],
                                       y = "Performance.Tag",
                                       parallel = FALSE)
  
  IV_table<-as.data.frame(IV$Summary)
  
  IV_table
  
  #Function to calculate WOE of variables
  
  for(i in 1:length(IV$Summary$Variable))
  {
    
    column_name <- IV$Summary$Variable[i]
    x_labels <- as.list(IV$Tables[[column_name]][,1])
    IV$Tables[[column_name]][,1][1]
    new_column_name <- paste(column_name,"_woe",  sep = "")
    demographic_data_V1[, new_column_name] <- 0
    
    for(j in length(x_labels):1)
    {
      
      if (is.numeric(demographic_data_V1[,column_name]) & !is.na(IV$Tables[[column_name]][,1][j])& (IV$Table[[column_name]][,1][j] != "NA")) 
      {
        aIndexLableSplitPart_lower <- unlist(strsplit(IV$Tables[[column_name]][, 1][j], ","))[[1]]
        actuallowerPart <- substr(aIndexLableSplitPart_lower, 2,nchar(aIndexLableSplitPart_lower))
        bin_lower_limit <- as.numeric(actuallowerPart)
        bin_lower_limit
        aIndexLableSplitPart_upper <- unlist(strsplit(IV$Tables[[column_name]][, 1][j], ","))[[2]]
        actualUpperPart <- substr(aIndexLableSplitPart_upper, 0, nchar(aIndexLableSplitPart_upper) - 1)
        bin_upper_limit <- as.numeric(actualUpperPart)
        bin_upper_limit
        
        demographic_data_V1[,new_column_name] <- 
          ifelse ((demographic_data_V1[, which(colnames(demographic_data_V1) == column_name)] >= bin_lower_limit)
                  &
                    (demographic_data_V1[, which(colnames(demographic_data_V1) == column_name)] <= bin_upper_limit),
                  IV$Tables[[column_name]][, 4][j],
                  demographic_data_V1[, new_column_name])
      } else
      {
        
        level_name <- x_labels[[j]]
        woe <- IV$Tables[[column_name]][,4][j]
        
        if(is.na(level_name) | level_name == "NA")
        {
          demographic_data_V1[which(is.na(demographic_data_V1[,column_name])),new_column_name]<- woe
          
        }else
        {
          demographic_data_V1[which(demographic_data_V1[,column_name]==level_name),new_column_name]<- woe
        }
      }
    }
  }
  
  str(demographic_data_V1)
  
  # Saving only WOE columns of demographic_data_V1 in other Data frame for model building
  
  demographic_data_V1_final<- demographic_data_V1[,!colnames(demographic_data_V1) %in% c("Age","Gender","Marital.Status..at.the.time.of.application.","No.of.dependents","Income","Education","Profession","Type.of.residence","No.of.months.in.current.residence","No.of.months.in.current.company")]
  
  str(demographic_data_V1_final)
  
  nrow(demographic_data_V1_final)
  
  # Removing Application ID from final dataset
  demographic_data_V1_final_woe<- demographic_data_V1_final[,!colnames(demographic_data_V1_final) %in% c("Application.ID")]
  str(demographic_data_V1_final_woe)
  
  ### CORRELATION FOR DEMOGRAPHIC DATA SET
  cor_df<- demographic_data_V1_final_woe[,!colnames(demographic_data_V1_final_woe) %in% c("Performance.Tag")]
  
  corr_index<- cor(cor_df) 
  
  
  
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(cor_df)
  str(cor_df)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(corr_index, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .6,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90, # Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = 0.01, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = FALSE,tl.cex = 0.8)
  
  ## THERE IS NO STRONG CORRELATION BETWEEN ANY OF THE VARIABLES.
  
  ##################################################################################################################################################################################################
  ##################################################################################################################################################################################################
  ############################# Model building of Demographic data #####################################
  ##################################################################################################################################################################################################
  
  ######## Logistic Regression Model ##################################################################
  
  set.seed(100)  
  indices = sample.split(demographic_data_V1_final_woe$Performance.Tag, SplitRatio = 0.7)  
  train = demographic_data_V1_final_woe[indices,]  
  test = demographic_data_V1_final_woe[!(indices),]  
  
  
  #Initial model 
  
  model_1 = glm(Performance.Tag ~ ., data = train, family = "binomial")
  sort(vif(model_1))
  summary(model_1)
  
  # The maximumn VIF is 1.021 , so this clearly indicates that there exist no collinearilty in the dataset
  
  # Stepwise selection 
  model_2<- stepAIC(model_1, direction="both")
  sort(vif(model_2))
  summary(model_2)
  
  #
  model_3<- glm(Performance.Tag ~ No.of.months.in.current.residence_woe + Income_woe + 
                  No.of.months.in.current.company_woe + Age_woe + No.of.dependents_woe + 
                  Profession_woe + Education_woe + Gender_woe, data = train, family = "binomial") 
  
  sort(vif(model_3))
  summary(model_3)
  
  # As Vif of all variables are between 1-1.02 hence removing the value based on p value
  # Removing Profession_woe as it is more insignificant variable compare to others
  
  model_4<- glm(Performance.Tag ~ No.of.months.in.current.residence_woe + Income_woe + 
                  No.of.months.in.current.company_woe + Age_woe + No.of.dependents_woe + 
                  Education_woe + Gender_woe, data = train, family = "binomial")
  
  sort(vif(model_4))
  summary(model_4)
  # Removing No.of.dependents_woe variable
  
  model_5<- glm(Performance.Tag ~ No.of.months.in.current.residence_woe + Income_woe + 
                  No.of.months.in.current.company_woe + Age_woe + 
                  Education_woe + Gender_woe, data = train, family = "binomial")
  sort(vif(model_5))
  summary(model_5)
  
  # Removing Gender_woe variable
  
  model_6<- glm(Performance.Tag ~ No.of.months.in.current.residence_woe + Income_woe + 
                  No.of.months.in.current.company_woe + Age_woe + 
                  Education_woe , data = train, family = "binomial")
  sort(vif(model_6))
  summary(model_6)
  
  # Removing Age_woe variable
  
  model_7<- glm(Performance.Tag ~ No.of.months.in.current.residence_woe + Income_woe + 
                  No.of.months.in.current.company_woe + 
                  Education_woe , data = train, family = "binomial")
  sort(vif(model_7))
  summary(model_7)
  
  # Removing Education_woe variable
  
  model_8<- glm(Performance.Tag ~ No.of.months.in.current.residence_woe + Income_woe + 
                  No.of.months.in.current.company_woe, data = train, family = "binomial")
  sort(vif(model_8))
  summary(model_8)
  
  
  # All are significant variable after building model_8 and also there are no high VIF values hence treating this as final model
  
  final_model<- model_8
  
  #######################################################################
  
  ### Model Evaluation
  
  ### Test Data ####
  
  # predicted probabilities of Credit default for test data
  
  test_pred = predict(final_model, type = "response", newdata = test[,-1])
  
  
  # Let's see the summary 
  
  summary(test_pred)
  
  # Let's use the probability cutoff of 10%.
  
  test_pred_default <- factor(ifelse(test_pred >= 0.1, "Yes", "No"))
  test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
  
  table(test_actual_default,test_pred_default)
  
  summary(test_pred_default)
  # We are not finding any Yes for test_pred_default variable with cutoff 10%
  
  # Let's use the probability cutoff of 5%
  test_pred_default <- factor(ifelse(test_pred >= 0.05, "Yes", "No"))
  test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
  
  table(test_actual_default,test_pred_default)
  
  
  test_conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
  test_conf
  #######################################################################
  
  #########################################################################################
  # Sensitivity is very low. So let's choose a different cutoff value
  
  # Let's find out the optimal probalility cutoff 
  # First let's create a function to find the accuracy, sensitivity and specificity for a given cutoff
  
  perform_fn <- function(cutoff) 
  {
    predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  # Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.
  
  # Summary of test probability
  
  summary(test_pred)
  
  s = seq(.01,.80,length=100)
  
  OUT = matrix(0,100,3)
  
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i])
  } 
  
  
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=9),seq(0,1,length=9),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.0625)]
  
  # Let's choose a cutoff value of 0.0415 for final model as for this all the 3 attributes are almost same
  # best value 0.0415
  test_cutoff_default <- factor(ifelse(test_pred >=0.0415, "Yes", "No"))
  
  conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")
  
  acc <- conf_final$overall[1]
  
  sens <- conf_final$byClass[1]
  
  spec <- conf_final$byClass[2]
  
  acc
  #0.5715
  
  sens
  #0.5742
  
  spec
  #0.5714
  
  ### KS -statistic - Test Data ######
  
  test_cutoff_default<- ifelse(test_cutoff_default=="Yes",1,0)
  test_actual_default <- ifelse(test_actual_default=="Yes",1,0)
  
  
  #on testing  data
  pred_object_test<- prediction(test_cutoff_default, test_actual_default)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  max(ks_table_test)
  #0.1455
  
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
  
  default_decile = lift(test_actual_default, test_pred, groups = 10)
  default_decile
  
  
  plot(default_decile$bucket,default_decile$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")
  plot(default_decile$bucket,default_decile$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
  
  
  
  ##### ROC on logistics model
  
  
  demographic_data_ROC <- demographic_data_V1_final_woe
  prob<- predict(final_model, newdata = demographic_data_ROC[, -c(1)], type = "response")
  demographic_data_ROC$prob<- prob
  roc_plot <- roc(Performance.Tag ~ prob, data = demographic_data_ROC)
  plot(roc_plot)
  roc_plot
  ##Area under the curve: 0.602

  
  
  
  ####################################################################################################
  ##---- Model Building :- Random forest
  
  demographic_data_rf <- demographic_data_V1_final_woe
  
  set.seed(100)
  
  demographic_data_rf$Performance.Tag <- as.factor(ifelse(demographic_data_rf$Performance.Tag==1,"yes","no"))
  split_indices <- sample.split(demographic_data_rf$Performance.Tag, SplitRatio = 0.70)
  
  demographic_train_rf <- demographic_data_rf[split_indices, ]
  
  demographic_test_rf <- demographic_data_rf[!split_indices, ]
  
  #---------------------------------------------------------    
  
  # Building the model 
  
  model_demographic_rf <- randomForest(Performance.Tag ~., data = demographic_train_rf, proximity = F, do.trace = T, mtry = 5)
  
  # Predict response for test data
  
  rf_pred_demographic <- predict(model_demographic_rf, demographic_test_rf[, -c(1)], type = "prob")
  
  
  #---------------------------------------------------------    
  
  # Cutoff for randomforest to assign yes or no
  
  perform_fn_rf_demographic <- function(cutoff) 
  {
    predicted_Performance.Tag <- as.factor(ifelse(rf_pred_demographic[, 2] >= cutoff, "yes", "no"))
    conf <- confusionMatrix(predicted_Performance.Tag, demographic_test_rf$Performance.Tag, positive = "yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
    colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
    return(OUT_rf)
  }
  
  #---------------------------------------------------------    
  
  # creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
  s = seq(.01,.99,length=100)
  
  OUT_rf = matrix(0,100,3)
  
  # calculate the sens, spec and acc for different cutoff values
  
  for(i in 1:100)
  {
    OUT_rf[i,] = perform_fn_rf_demographic(s[i])
  } 
  
  #---------------------------------------------------------    
  
  # plotting cutoffs
  
  plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
  lines(s,OUT_rf[,3],col=4,lwd=2)
  box()
  
  
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.125)]
  
  predicted_Performance.Tag_rf <- factor(ifelse(rf_pred_demographic[, 2] >= 0.0415, "yes", "no"))
  
  conf_forest <- confusionMatrix(predicted_Performance.Tag_rf, demographic_test_rf[,c("Performance.Tag")], positive = "yes")
  
  conf_forest
  
  # Sensitivity
  conf_forest$byClass[1]
  #0.4666
  
  # Specificity 
  conf_forest$byClass[2]
  #0.6044
  
  # Accuracy 
  conf_forest$overall[1]
  #0.5986
  
  
  # Final RF important variables
  importance_demographic <-model_demographic_rf$importance
  
  importance_demographic <- data.frame(importance_demographic)
  
  importance_demographic
  
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  ############################################################## Credit Bureau data   #########################################################################
  ############################################################################################################################################################
  
  colnames(credit_data)
  str(credit_data)
  
  missing_values_c <- credit_data %>%
    summarise_all(funs(sum(is.na(.))/n()))
  
  missing_values_c <- gather(missing_values_c,key='feature',value = 'missing_percentage')
  
  missing_values_c %>%
    ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
    geom_bar(stat = 'identity',fill='red') +
    coord_flip()
  
  
  
  ####################################################################################################################################################
  ####################################################################################################################################################
  ########################        Data Cleaning and Transformation of Credit Bureau data      #######################
  #####################################################################################################################################################
  
  #Converting columns into factor which are categorical in nature
  
  credit_data$Presence.of.open.auto.loan<-as.factor(credit_data$Presence.of.open.auto.loan)
  
  credit_data$Presence.of.open.home.loan<-as.factor(credit_data$Presence.of.open.home.loan)
  
  str(credit_data)
  
  # 1. No.of.times.90.DPD.or.worse.in.last.6.months
  
  nrow(credit_data)
  
  quantile(credit_data$No.of.times.90.DPD.or.worse.in.last.6.months, seq(0, 1, 0.01))
  
  boxplot(credit_data$No.of.times.90.DPD.or.worse.in.last.6.months)
  
  summary(credit_data$No.of.times.90.DPD.or.worse.in.last.6.months)
  
  nrow(filter(credit_data,credit_data$No.of.times.90.DPD.or.worse.in.last.6.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  
  #2.No.of.times.60.DPD.or.worse.in.last.6.months
  
  quantile(credit_data$No.of.times.60.DPD.or.worse.in.last.6.months, seq(0, 1, 0.01))
  
  boxplot(credit_data$No.of.times.60.DPD.or.worse.in.last.6.months)
  
  summary(credit_data$No.of.times.60.DPD.or.worse.in.last.6.months)
  
  nrow(filter(credit_data,credit_data$No.of.times.60.DPD.or.worse.in.last.6.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  
  # 3.No.of.times.30.DPD.or.worse.in.last.6.months
  
  quantile(credit_data$No.of.times.30.DPD.or.worse.in.last.6.months, seq(0, 1, 0.01))
  
  boxplot(credit_data$No.of.times.30.DPD.or.worse.in.last.6.months)
  
  summary(credit_data$No.of.times.30.DPD.or.worse.in.last.6.months)
  
  nrow(filter(credit_data,credit_data$No.of.times.30.DPD.or.worse.in.last.6.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  # 4.No.of.times.90.DPD.or.worse.in.last.12.months
  
  quantile(credit_data$No.of.times.90.DPD.or.worse.in.last.12.months, seq(0, 1, 0.01))
  
  boxplot(credit_data$No.of.times.90.DPD.or.worse.in.last.12.months)
  
  summary(credit_data$No.of.times.90.DPD.or.worse.in.last.12.months)
  
  nrow(filter(credit_data,credit_data$No.of.times.90.DPD.or.worse.in.last.12.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  # 5.No.of.times.60.DPD.or.worse.in.last.12.months
  
  quantile(credit_data$No.of.times.60.DPD.or.worse.in.last.12.months, seq(0, 1, 0.01))
  
  boxplot(credit_data$No.of.times.60.DPD.or.worse.in.last.12.months)
  
  summary(credit_data$No.of.times.60.DPD.or.worse.in.last.12.months)
  
  nrow(filter(credit_data,credit_data$No.of.times.60.DPD.or.worse.in.last.12.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  # 6.No.of.times.30.DPD.or.worse.in.last.12.months
  quantile(credit_data$No.of.times.30.DPD.or.worse.in.last.12.months, seq(0, 1, 0.01))
  
  boxplot(credit_data$No.of.times.30.DPD.or.worse.in.last.12.months)
  
  summary(credit_data$No.of.times.30.DPD.or.worse.in.last.12.months)
  
  nrow(filter(credit_data,credit_data$No.of.times.30.DPD.or.worse.in.last.12.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  
  # 7.Avgas.CC.Utilization.in.last.12.months
  
  quantile(credit_data$Avgas.CC.Utilization.in.last.12.months, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$Avgas.CC.Utilization.in.last.12.months)
  
  summary(credit_data$Avgas.CC.Utilization.in.last.12.months)
  
  nrow(filter(credit_data,credit_data$Avgas.CC.Utilization.in.last.12.months<0))
  
  # There are no unexpected values in this variable,outliers and NA vaues are there but that we will handle using WOE
  # There are 1058 NA values.
  
  # 8.No.of.trades.opened.in.last.6.months
  
  quantile(credit_data$No.of.trades.opened.in.last.6.months, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$No.of.trades.opened.in.last.6.months)
  
  summary(credit_data$No.of.trades.opened.in.last.6.months)
  
  nrow(filter(credit_data,credit_data$No.of.trades.opened.in.last.6.months<0))
  
  # There are no unexpected values in this variable,outliers and NA vaues are there but that we will handle using WOE
  # There is 1 NA value.
  
  # 9.No.of.trades.opened.in.last.12.months
  
  quantile(credit_data$No.of.trades.opened.in.last.12.months, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$No.of.trades.opened.in.last.12.months)
  
  summary(credit_data$No.of.trades.opened.in.last.12.months)
  
  nrow(filter(credit_data,credit_data$No.of.trades.opened.in.last.12.months<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  # 10.No.of.PL.trades.opened.in.last.6.months
  
  quantile(credit_data$No.of.PL.trades.opened.in.last.6.months, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$No.of.PL.trades.opened.in.last.6.months)
  
  summary(credit_data$No.of.PL.trades.opened.in.last.6.months)
  
  nrow(filter(credit_data,credit_data$No.of.PL.trades.opened.in.last.6.months<0))
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  # 11.No.of.PL.trades.opened.in.last.12.months
  
  quantile(credit_data$No.of.PL.trades.opened.in.last.12.months, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$No.of.PL.trades.opened.in.last.12.months)
  
  summary(credit_data$No.of.PL.trades.opened.in.last.12.months)
  
  nrow(filter(credit_data,credit_data$No.of.PL.trades.opened.in.last.12.months<0))
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  
  # 12.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
  
  quantile(credit_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
  
  summary(credit_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
  
  nrow(filter(credit_data,credit_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  # 13.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
  
  quantile(credit_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
  
  summary(credit_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
  
  nrow(filter(credit_data,credit_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<0))
  
  # There are no unexpected values in this variable,outliers are there but that we will handle using WOE
  
  
  # 14.Presence.of.open.home.loan
  
  levels(credit_data$Presence.of.open.home.loan)
  
  summary(credit_data$Presence.of.open.home.loan)
  
  # There are no unexpected values in this variable and only two levels are present i.e.0 and 1 and
  # 272 NA values are present in this variable.
  
  
  # 15. Outstanding.Balance
  
  quantile(credit_data$Outstanding.Balance, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$Outstanding.Balance)
  summary(credit_data$Outstanding.Balance)
  
  nrow(filter(credit_data,credit_data$Outstanding.Balance<0))
  
  # Boxplot and quantile both shows it does not have any outlier and no unexpected values are present however there are 272 NA values are present.
  
  
  #16. Total.No.of.Trades
  
  quantile(credit_data$Total.No.of.Trades, seq(0, 1, 0.01),na.rm = T)
  
  boxplot(credit_data$Total.No.of.Trades)
  summary(credit_data$Total.No.of.Trades)
  nrow(filter(credit_data,credit_data$Total.No.of.Trades<0))
  
  # There are no unexpected values in this variable.Outliers are present and that we weill handle using WOE
  
  
  # 17.Presence.of.open.auto.loan
  
  levels(credit_data$Presence.of.open.auto.loan)
  
  summary(credit_data$Presence.of.open.auto.loan)
  
  # There are no unexpected values in this variable and only two levels are present i.e. 0 and 1
  
  # 18.Performance.Tag
  
  summary(credit_data$Performance.Tag)
  
  # There are 1425 NA values
  # There are no unexpected values in this variable and only two levels are present i.e. 0 and 1
  
  # Creating dataframe of the Rejected population i.e. the records having NA in Performance Tag in credit bureau dataset
  credit_rejected <- credit_data[which(is.na(credit_data$Performance.Tag)),]
  
  #Let's remove the records which have NA values in target variable.
  # This is confirmed by TA and mentor #https://learn.upgrad.com/v/course/163/question/124008
  
  sum(is.na(credit_data$Performance.Tag))
  # We have 1425 rows which have Performance tag with NA Values
  
  credit_data_V1<- credit_data[-which(is.na(credit_data$Performance.Tag)),]
  nrow(credit_data_V1)
  
  length(unique(credit_data_V1$Application.ID))
  
  #There are 69867 unique records in application id column which is same as number of record of credit_data_V1 hence we can
  #say Application.ID feature is here working as primary key (unique identifier)
  
  colnames(credit_data_V1)
  
  # Let's check NA values of each column
  credit_data_V1 %>%
         summarise_all(funs(sum(is.na(.))))
  
  # Now We have NA values in following columns : Avgas.CC.Utilization.in.last.12.months, No.of.trades.opened.in.last.6.months,Presence.of.open.home.loan, Outstanding.Balance
  
  str(credit_data_V1)
  
  
  ####################################################################################################################################################
  ####################################################################################################################################################
  ##########################################################   EDA of credit bureau data   ###########################################################
  ####################################################################################################################################################
  
  
  # Function for distribution of categorical variables (plotting bar charts)
  
  univariate_categorical <- function(dataset,var,var_name){
    
    dataset %>% ggplot(aes(x = as.factor(var))) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
      scale_y_continuous(labels = percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = var_name, y = "Percent", x = var_name)+theme(
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.y=element_blank()
      ) 
  }
  
  
  univariate_categorical(credit_data_V1,credit_data_V1$Presence.of.open.auto.loan ,"Avg cc")
  
  
  categorical_bivariate <- function(dataset, var, var_name){
    plot_bi = ggplot(dataset, aes(x=var,fill=factor(credit_data_V1$Performance.Tag)))+geom_bar()
    return(plot_bi)
  }
  
  categorical_bivariate(credit_data_V1,credit_data_V1$Presence.of.open.auto.loan,"Open auto loan")
  
  #1.No.of.times.90.DPD.or.worse.in.last.6.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.times.90.DPD.or.worse.in.last.6.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.times.90.DPD.or.worse.in.last.6.months Vs Performance Tag") +xlab("No.of.times.90.DPD.or.worse.in.last.6.months") + ylab("Count") + labs(fill = "Performance tag")
  #ggplot(credit_data_V1, aes(x=as.numeric(credit_data_V1$No.of.times.90.DPD.or.worse.in.last.6.months),y=credit_data_V1$Performance.Tag)) +geom_boxplot()
  
  #2.No.of.times.60.DPD.or.worse.in.last.6.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.times.60.DPD.or.worse.in.last.6.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.times.60.DPD.or.worse.in.last.6.months Vs Performance Tag") +xlab("No.of.times.60.DPD.or.worse.in.last.6.months") + ylab("Count") + labs(fill = "Performance tag")
  #3.No.of.times.30.DPD.or.worse.in.last.6.months
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.times.30.DPD.or.worse.in.last.6.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.times.30.DPD.or.worse.in.last.6.months Vs Performance Tag") +xlab("No.of.times.30.DPD.or.worse.in.last.6.months") + ylab("Count") + labs(fill = "Performance tag")
  #4.No.of.times.90.DPD.or.worse.in.last.12.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.times.90.DPD.or.worse.in.last.12.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.times.90.DPD.or.worse.in.last.12.months Vs Performance Tag") +xlab("No.of.times.90.DPD.or.worse.in.last.12.months") + ylab("Count") + labs(fill = "Performance tag")
  #5 No.of.times.60.DPD.or.worse.in.last.12.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.times.60.DPD.or.worse.in.last.12.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.times.60.DPD.or.worse.in.last.12.months Vs Performance Tag") +xlab("No.of.times.60.DPD.or.worse.in.last.12.months") + ylab("Count") + labs(fill = "Performance tag")
  #6 No.of.times.30.DPD.or.worse.in.last.12.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.times.30.DPD.or.worse.in.last.12.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.times.30.DPD.or.worse.in.last.12.months Vs Performance Tag") +xlab("No.of.times.30.DPD.or.worse.in.last.12.months") + ylab("Count") + labs(fill = "Performance tag")
  #7 Avgas.CC.Utilization.in.last.12.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$Avgas.CC.Utilization.in.last.12.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("Avgas.CC.Utilization.in.last.12.months Vs Performance Tag") +xlab("Avgas.CC.Utilization.in.last.12.months") + ylab("Count") + labs(fill = "Performance tag")
  #8. No.of.trades.opened.in.last.6.months  vs performance tag
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.trades.opened.in.last.6.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.trades.opened.in.last.6.months Vs Performance Tag") +xlab("No.of.trades.opened.in.last.6.months") + ylab("Count") + labs(fill = "Performance tag")
  #9. No.of.trades.opened.in.last.12.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.trades.opened.in.last.12.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.trades.opened.in.last.12.months Vs Performance Tag") +xlab("No.of.trades.opened.in.last.12.months") + ylab("Count") + labs(fill = "Performance tag")
  #10.No.of.PL.trades.opened.in.last.6.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.PL.trades.opened.in.last.6.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.PL.trades.opened.in.last.6.months Vs Performance Tag") +xlab("No.of.PL.trades.opened.in.last.6.months") + ylab("Count") + labs(fill = "Performance tag")
  
  #11. No.of.PL.trades.opened.in.last.12.months vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.PL.trades.opened.in.last.12.months,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.PL.trades.opened.in.last.12.months Vs Performance Tag") +xlab("No.of.PL.trades.opened.in.last.12.months") + ylab("Count") + labs(fill = "Performance tag")
  
  #12.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. vs Performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. Vs Performance Tag") +xlab("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.") + ylab("Count") + labs(fill = "Performance tag")
  
  
  #13. No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. Vs Performance Tag") +xlab("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") + ylab("Count") + labs(fill = "Performance tag")
  
  #14  Presence.of.open.home.loan  vs performance tag
  ggplot(credit_data_V1, aes(x=credit_data_V1$Presence.of.open.home.loan,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("Presence.of.open.home.loan Vs Performance Tag") +xlab("Presence.of.open.home.loan") + ylab("Count") + labs(fill = "Performance tag")
  
  #15  Outstanding.Balance
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$Outstanding.Balance,fill = factor(credit_data_V1$Performance.Tag))) +geom_histogram()+
    ggtitle("Outstanding.Balance Vs Performance Tag") +xlab("Outstanding.Balance") + ylab("Count") + labs(fill = "Performance tag")
  
  #16 Total.No.of.Trades vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$Total.No.of.Trades,fill = factor(credit_data_V1$Performance.Tag))) +geom_histogram()+
    ggtitle("Total.No.of.Trades Vs Performance Tag") +xlab("Total.No.of.Trades") + ylab("Count") + labs(fill = "Performance tag")
  
  #17 Presence.of.open.auto.loan vs performance tag
  
  ggplot(credit_data_V1, aes(x=credit_data_V1$Presence.of.open.auto.loan,fill = factor(credit_data_V1$Performance.Tag))) +geom_bar()+
    ggtitle("Presence.of.open.auto.loan Vs Performance Tag") +xlab("Presence.of.open.auto.loan") + ylab("Count") + labs(fill = "Performance tag")
  
  ####################################################################################################################################################
  ####################################################################################################################################################
  ############################# MERGING DEMOGRAPHIC & CREDIT BUREAU DATASETS #############################
  ###################################################################################################################################################
  
  nrow(demographic_data_V3)
  length(unique(demographic_data_V3$Application.ID))
  
  nrow(credit_data_V1)
  length(unique(credit_data_V1$Application.ID))
  
  merge_dataset <- merge(x=demographic_data_V3,y=credit_data_V1,by.x=c("Application.ID"),by.y = c("Application.ID"))
  nrow(merge_dataset)
  
  # Checking the value of Performance Tag of both the datasets in the merge dataset
  sum(merge_dataset$Performance.Tag.x!=merge_dataset$Performance.Tag.y)
  # The value of performance tag from both the data set is same.We are dropping one of the performance Tag from the dataset.
  
  merge_dataset_V1<- merge_dataset[,!colnames(merge_dataset) %in% c('Performance.Tag.y')]
  
  str(merge_dataset_V1)
  
  colnames(merge_dataset_V1)[colnames(merge_dataset_V1)=="Performance.Tag.x"] <- "Performance.Tag"
  
  ###################################################################################################################################################
  ###################################################################################################################################################
  #################### IV CALCULATION FOR ENTIRE DATA SET ##########################################################################################
  ###################################################################################################################################################
  
  # IV & WOE calculation on the entire Merged Dataset
  
  IV_Merge <- Information::create_infotables(data = merge_dataset_V1[,-1],y = "Performance.Tag",parallel = FALSE)
  
  
  IV_Merge_table <- as.data.frame(IV_Merge$Summary)
  
  IV_Merge_table
  #Function to calculate WOE of variables
  
  for(i in 1:length(IV_Merge$Summary$Variable))
  {
    
    column_name <- IV_Merge$Summary$Variable[i]
    x_labels <- as.list(IV_Merge$Tables[[column_name]][,1])
    IV_Merge$Tables[[column_name]][,1][1]
    new_column_name <- paste(column_name,"_woe",  sep = "")
    merge_dataset_V1[, new_column_name] <- 0
    
    for(j in length(x_labels):1)
    {
      
      if (is.numeric(merge_dataset_V1[,column_name]) & !is.na(IV_Merge$Tables[[column_name]][,1][j])& (IV_Merge$Table[[column_name]][,1][j] != "NA")) 
      {
        aIndexLableSplitPart_lower <- unlist(strsplit(IV_Merge$Tables[[column_name]][, 1][j], ","))[[1]]
        actuallowerPart <- substr(aIndexLableSplitPart_lower, 2,nchar(aIndexLableSplitPart_lower))
        bin_lower_limit <- as.numeric(actuallowerPart)
        bin_lower_limit
        aIndexLableSplitPart_upper <- unlist(strsplit(IV_Merge$Tables[[column_name]][, 1][j], ","))[[2]]
        actualUpperPart <- substr(aIndexLableSplitPart_upper, 0, nchar(aIndexLableSplitPart_upper) - 1)
        bin_upper_limit <- as.numeric(actualUpperPart)
        bin_upper_limit
        
        merge_dataset_V1[,new_column_name] <- 
          ifelse ((merge_dataset_V1[, which(colnames(merge_dataset_V1) == column_name)] >= bin_lower_limit)
                  &
                    (merge_dataset_V1[, which(colnames(merge_dataset_V1) == column_name)] <= bin_upper_limit),
                  IV_Merge$Tables[[column_name]][, 4][j],
                  merge_dataset_V1[, new_column_name])
      } else
      {
        
        level_name <- x_labels[[j]]
        woe <- IV_Merge$Tables[[column_name]][,4][j]
        
        if(is.na(level_name) | level_name == "NA")
        {
          merge_dataset_V1[which(is.na(merge_dataset_V1[,column_name])),new_column_name]<- woe
          
        }else
        {
          merge_dataset_V1[which(merge_dataset_V1[,column_name]==level_name),new_column_name]<- woe
        }
      }
    }
  }
  
  str(merge_dataset_V1)
  
  # Saving only WOE columns of merge_data_V1 in other Data frame for model building
  
  merge_dataset_V1_final<- merge_dataset_V1[,!colnames(merge_dataset_V1) %in% c("Age",
                                                                                "Gender",
                                                                                "Marital.Status..at.the.time.of.application.",
                                                                                "No.of.dependents",
                                                                                "Income",
                                                                                "Education",
                                                                                "Profession",
                                                                                "Type.of.residence",
                                                                                "No.of.months.in.current.residence",
                                                                                "No.of.months.in.current.company",
                                                                                "No.of.times.90.DPD.or.worse.in.last.6.months",
                                                                                "No.of.times.60.DPD.or.worse.in.last.6.months",
                                                                                "No.of.times.30.DPD.or.worse.in.last.6.months",
                                                                                "No.of.times.90.DPD.or.worse.in.last.12.months",
                                                                                "No.of.times.60.DPD.or.worse.in.last.12.months",
                                                                                "No.of.times.30.DPD.or.worse.in.last.12.months",
                                                                                "Avgas.CC.Utilization.in.last.12.months",
                                                                                "No.of.trades.opened.in.last.6.months",
                                                                                "No.of.trades.opened.in.last.12.months",
                                                                                "No.of.PL.trades.opened.in.last.6.months",
                                                                                "No.of.PL.trades.opened.in.last.12.months",
                                                                                "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                                                                                "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                                                                                "Presence.of.open.home.loan",
                                                                                "Outstanding.Balance",
                                                                                "Total.No.of.Trades",
                                                                                "Presence.of.open.auto.loan")]
  
  
  str(merge_dataset_V1_final)
  
  nrow(merge_dataset_V1_final)
  
  
  ## CORRELATION MATRIX ###
  
  cor_df<- merge_dataset_V1_final[,!colnames(merge_dataset_V1_final) %in% c("Performance.Tag","Application.ID")]
  
  corr_index<- cor(cor_df) 
  
  
  
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(cor_df)
  str(cor_df)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(corr_index, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .5,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 75, # Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = 0.01, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = FALSE,tl.cex = 0.7)
  
  
  
  ##################################################################################################################################################
  ##################################################################################################################################################
  ############################### MODEL BUILDING FOR ENTIRE DATA SET ###############################################################################
  ##################################################################################################################################################
  
  ######## Logistic Regression Model ##################################################################
  
  set.seed(100)  
  indices = sample.split(merge_dataset_V1_final$Performance.Tag, SplitRatio = 0.7) 
  
  ## REMOVING APPLICATION ID BEFORE CREATING TRAIN AND TEST DATA
  train_mergeData = merge_dataset_V1_final[indices,-1]  
  test_mergeData  = merge_dataset_V1_final[!(indices),-1]  
  
  
  #Initial model 
  
  model_1 = glm(Performance.Tag ~ ., data = train_mergeData, family = "binomial")
  sort(vif(model_1))
  summary(model_1)
  
  # Stepwise AIC selection
  
  model_2<- stepAIC(model_1, direction="both")
  sort(vif(model_2))
  summary(model_2)
  
  
  model_3 <- glm(Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months_woe +  No.of.trades.opened.in.last.12.months_woe +  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                   Outstanding.Balance_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                   Income_woe + No.of.months.in.current.company_woe + Age_woe + 
                   No.of.dependents_woe + Profession_woe + Type.of.residence_woe, data = train_mergeData, family = "binomial")
  sort(vif(model_3))
  summary(model_3)
  
  # VIF is less than 4 for all variables. So we will remove variables on the basis of p-value.
  
  # Removing 'Income_woe' variable
  model_4<- glm(Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months_woe +  No.of.trades.opened.in.last.12.months_woe +  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                  Outstanding.Balance_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                  No.of.months.in.current.company_woe + Age_woe + 
                  No.of.dependents_woe + Profession_woe + Type.of.residence_woe, data = train_mergeData, family = "binomial")
  
  sort(vif(model_4))
  summary(model_4)
  
  #Removing 'Type.of.residence_woe' variable
  
  model_5 <- glm(Performance.Tag ~ No.of.months.in.current.company_woe + 
                   Age_woe + No.of.dependents_woe + Profession_woe +
                   Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                   Outstanding.Balance_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe, data = train_mergeData, family = "binomial")
  sort(vif(model_5))
  summary(model_5)
  
  
  # Removing 'Profession_woe' variable
  model_6 <- glm(Performance.Tag ~ No.of.months.in.current.company_woe + 
                   Age_woe + No.of.dependents_woe +
                   Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                   Outstanding.Balance_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe, data = train_mergeData, family = "binomial")
  sort(vif(model_6))
  summary(model_6)
  
  
  # Removing 'No.of.months.in.current.company_woe' variable
  
  model_7 <- glm(Performance.Tag ~ Age_woe + No.of.dependents_woe +
                   Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                   Outstanding.Balance_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe, data = train_mergeData, family = "binomial")
  
  sort(vif(model_7))
  summary(model_7)
  
  
  # Removing 'No.of.dependents_woe' variable
  
  model_8 <- glm(Performance.Tag ~ Age_woe +
                   Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                   Outstanding.Balance_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe, data = train_mergeData, family = "binomial")
  sort(vif(model_8))
  summary(model_8)
  
  
  # Removing 'Outstanding.Balance_woe' variable
  
  model_9 <- glm(Performance.Tag ~ Age_woe +
                   Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                   No.of.times.30.DPD.or.worse.in.last.6.months_woe, data = train_mergeData, family = "binomial")
  sort(vif(model_9))
  summary(model_9)
  
  # Removing 'Age_woe' variable
  
  model_10 <- glm(Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.times.30.DPD.or.worse.in.last.6.months_woe, data = train_mergeData, family = "binomial")
  sort(vif(model_10))
  summary(model_10)
  
  
  final_merge_model<- model_10
  
  
  #######################################################################
  
  ### Model Evaluation
  
  ### Test_mergeData Data ####
  
  # predicted probabilities of Credit default for test_mergeData data
  
  test_mergeData_pred = predict(final_merge_model, type = "response", newdata = test_mergeData[,-1])
  
  
  # Let's see the summary 
  
  summary(test_mergeData_pred)
  
  # Let's use the probability cutoff of 10%.
  
  test_mergeData_pred_default <- factor(ifelse(test_mergeData_pred >= 0.1, "Yes", "No"))
  test_mergeData_actual_default <- factor(ifelse(test_mergeData$Performance.Tag==1,"Yes","No"))
  
  table(test_mergeData_actual_default,test_mergeData_pred_default)
  
  summary(test_mergeData_pred_default)
  # We are not finding any Yes for test_mergeData_pred_default variable with cutoff 10%
  
  # Let's use the probability cutoff of 5%
  test_mergeData_pred_default <- factor(ifelse(test_mergeData_pred >= 0.05, "Yes", "No"))
  test_mergeData_actual_default <- factor(ifelse(test_mergeData$Performance.Tag==1,"Yes","No"))
  
  table(test_mergeData_actual_default,test_mergeData_pred_default)
  
  
  test_mergeData_conf <- confusionMatrix(test_mergeData_pred_default, test_mergeData_actual_default, positive = "Yes")
  test_mergeData_conf
  #######################################################################
  
  #########################################################################################
  # Sensitivity is very low. So let's choose a different cutoff value
  
  # Let's find out the optimal probalility cutoff 
  # First let's create a function to find the accuracy, sensitivity and specificity for a given cutoff
  
  perform_fn <- function(cutoff) 
  {
    predicted_default <- factor(ifelse(test_mergeData_pred >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_default, test_mergeData_actual_default, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  # Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.
  
  # Summary of test_mergeData probability
  
  summary(test_mergeData_pred)
  
  s = seq(.01,.80,length=100)
  
  OUT = matrix(0,100,3)
  
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i])
  } 
  
  
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=9),seq(0,1,length=9),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.125)]
  
  # Let's choose a cutoff value of 0.047 for final model as for this all the 3 attributes are almost same
  # best value 0.048
  test_mergeData_cutoff_default <- factor(ifelse(test_mergeData_pred >=0.047, "Yes", "No"))
  
  conf_final <- confusionMatrix(test_mergeData_cutoff_default, test_mergeData_actual_default, positive = "Yes")
  
  acc <- conf_final$overall[1]
  
  sens <- conf_final$byClass[1]
  
  spec <- conf_final$byClass[2]
  
  acc
  # 0.6435
  sens
  #0.6036
  spec
  #0.6452
  
  ### KS -statistic - test_mergeData Data ######
  
  test_mergeData_cutoff_default<- ifelse(test_mergeData_cutoff_default=="Yes",1,0)
  test_mergeData_actual_default <- ifelse(test_mergeData_actual_default=="Yes",1,0)
  
  
  #on test_mergeDataing  data
  pred_object_test_mergeData<- prediction(test_mergeData_cutoff_default, test_mergeData_actual_default)
  
  performance_measures_test_mergeData<- performance(pred_object_test_mergeData, "tpr", "fpr")
  
  ks_table_test_mergeData <- attr(performance_measures_test_mergeData, "y.values")[[1]] - 
    (attr(performance_measures_test_mergeData, "x.values")[[1]])
  
  max(ks_table_test_mergeData)
  #0.2488
  
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
  
  default_decile = lift(test_mergeData_actual_default, test_mergeData_pred, groups = 10)
  default_decile
  
  plot(default_decile$bucket,default_decile$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")
  plot(default_decile$bucket,default_decile$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
  
  
  ##### ROC on logistics model
  
  
  merge_dataset_ROC <- merge_dataset_V1_final
  prob<- predict(final_merge_model, newdata = merge_dataset_ROC[, -c(2)], type = "response")
  merge_dataset_ROC$prob<- prob
  roc_plot <- roc(Performance.Tag ~ prob, data = merge_dataset_ROC)
  plot(roc_plot)
  roc_plot
 ## Area under the curve: 0.675
  
  
  ################################################################################################################################
  # ---- Model Building :- Random forest
  
  ## Store "merge_dataset_V1_final" to "merge_dataset_V1_rf" dataframe
  
  merge_dataset_V1_rf <- merge_dataset_V1_final[,-1]
  
  set.seed(100)
  
  # Spliting the merge_dataset_V1_rf data in 70:30 ratio
  
  merge_dataset_V1_rf$Performance.Tag <- as.factor(ifelse(merge_dataset_V1_rf$Performance.Tag==1,"yes","no"))
  split_indices <- sample.split(merge_dataset_V1_rf$Performance.Tag, SplitRatio = 0.70)
  
  merge_train_rf <- merge_dataset_V1_rf[split_indices, ]
  
  merge_test_rf <- merge_dataset_V1_rf[!split_indices, ]
  
  #---------------------------------------------------------    
  
  # Building the model 
  
  model_merge_rf <- randomForest(Performance.Tag ~., data = merge_train_rf, proximity = F, do.trace = T, mtry = 5)
  
  # Predict response for test data
  
  rf_pred <- predict(model_merge_rf, merge_test_rf[, -c(1)], type = "prob")
  
  
  #---------------------------------------------------------    
  
  # Cutoff for randomforest to assign yes or no
  
  perform_fn_rf <- function(cutoff) 
  {
    predicted_Performance.Tag <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
    conf <- confusionMatrix(predicted_Performance.Tag, merge_test_rf$Performance.Tag, positive = "yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
    colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
    return(OUT_rf)
  }
  
  #---------------------------------------------------------    
  
  # creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
  s = seq(.01,.99,length=100)
  
  OUT_rf = matrix(0,100,3)
  
  # calculate the sens, spec and acc for different cutoff values
  
  for(i in 1:100)
  {
    OUT_rf[i,] = perform_fn_rf(s[i])
  } 
  
  #---------------------------------------------------------    
  
  # plotting cutoffs
  
  plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
  lines(s,OUT_rf[,3],col=4,lwd=2)
  box()
  
  
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.25)]
  
  # The plot shows that cutoff value of around 22% optimises sensitivity and accuracy
  
  predicted_Performance.Tag_rf <- factor(ifelse(rf_pred[, 2] >= 0.04960, "yes", "no"))
  
  conf_forest <- confusionMatrix(predicted_Performance.Tag_rf, merge_test_rf[,c("Performance.Tag")], positive = "yes")
  
  conf_forest
  
  # Sensitivity
  conf_forest$byClass[1]
  #0.624
  
  # Specificity 
  conf_forest$byClass[2]
  #0.5805
  
  # Accuracy 
  conf_forest$overall[1]
  #0.5823
  
  # Final RF important variables
  importance <- model_merge_rf$importance 
  
  importance <- data.frame(importance)
  
  importance
  
  
  ###################################################################################################################################################
  
  ### SINCE WE HAVE BETTER ACCURACY AND SENSITIVITY IN LOGISTICS MODEL,
  ### WE HAVE CONSIDERED LOGISTICS REGERESSION MODEL AS THE FINAL MODEL FOR SCORE CARD PREDICTION 
  
  
  ###################################################################################################################################################
  ###################################################################################################################################################
  ############################ SCORE CARD BUILDING ##################################################################################################
  ###################################################################################################################################################
  
  ## Manual Scorecard development on the entire Merged dataset on the basis of logistics Regression Model
  
  pred <- predict(final_merge_model, newdata = merge_dataset_V1_final[, -c(2)], type = "response")
  summary(pred)
  
  pred_response <- factor(ifelse(pred >= 0.047, "Yes", "No"))
  
  merge_dataset_V1_final$Predicted_Performance.Tag <- pred_response
  merge_dataset_V1_final$Predicted_prob <- pred
  merge_dataset_V1_final$logit<- log(merge_dataset_V1_final$Predicted_prob/(1-merge_dataset_V1_final$Predicted_prob))
  
  points0 = 400
  odds0 = 10
  pdo = 20
  
  factor = pdo / log(2)
  offset = points0 - factor * log(odds0)
  merge_dataset_V1_final$Score = round(offset - factor * merge_dataset_V1_final$logit)
  
  summary(merge_dataset_V1_final$Score)
  
  #Minimum score is 398 and Max Score is 457
  
  Cutoff_Score = offset - factor * log(0.047/(1-0.047))
  Cutoff_Score
  # Cutoff_Score is 420.4. For all our calculation we are taking upper limit.
  ceiling(Cutoff_Score)
  # Rounded Cutoff_Score is at 421
  # Below the cutoff score the customer will not be given credit card hence taking cutoff of 421
  
  
  #######################################################################################################################################################
  
  # REJECTED POPULATION DATASET MERGING
  
  nrow(demographic_rejected)
  length(unique(demographic_rejected$Application.ID))
  
  nrow(credit_rejected)
  length(unique(credit_rejected$Application.ID))
  
  merge_rejected_dataset <- merge(x=demographic_rejected,y=credit_rejected,by.x=c("Application.ID"),by.y = c("Application.ID"))
  nrow(merge_rejected_dataset)
  
  str(merge_rejected_dataset)
  
  #Removing Application.ID,Performance.Tag.x and Performance.Tag.y
  
  merge_rejected_dataset_V1<- merge_rejected_dataset[,!colnames(merge_rejected_dataset) %in% c('Performance.Tag.x','Performance.Tag.y')]
  
  
  #Function to calculate WOE of variables for merge_rejected_dataset_V1
  
  for(i in 1:length(IV_Merge$Summary$Variable))
  {
    
    column_name <- IV_Merge$Summary$Variable[i]
    x_labels <- as.list(IV_Merge$Tables[[column_name]][,1])
    IV_Merge$Tables[[column_name]][,1][1]
    new_column_name <- paste(column_name,"_woe",  sep = "")
    merge_rejected_dataset_V1[, new_column_name] <- 0
    
    for(j in length(x_labels):1)
    {
      
      if (is.numeric(merge_rejected_dataset_V1[,column_name]) & !is.na(IV_Merge$Tables[[column_name]][,1][j])& (IV_Merge$Table[[column_name]][,1][j] != "NA")) 
      {
        aIndexLableSplitPart_lower <- unlist(strsplit(IV_Merge$Tables[[column_name]][, 1][j], ","))[[1]]
        actuallowerPart <- substr(aIndexLableSplitPart_lower, 2,nchar(aIndexLableSplitPart_lower))
        bin_lower_limit <- as.numeric(actuallowerPart)
        bin_lower_limit
        aIndexLableSplitPart_upper <- unlist(strsplit(IV_Merge$Tables[[column_name]][, 1][j], ","))[[2]]
        actualUpperPart <- substr(aIndexLableSplitPart_upper, 0, nchar(aIndexLableSplitPart_upper) - 1)
        bin_upper_limit <- as.numeric(actualUpperPart)
        bin_upper_limit
        
        merge_rejected_dataset_V1[,new_column_name] <- 
          ifelse ((merge_rejected_dataset_V1[, which(colnames(merge_rejected_dataset_V1) == column_name)] >= bin_lower_limit)
                  &
                    (merge_rejected_dataset_V1[, which(colnames(merge_rejected_dataset_V1) == column_name)] <= bin_upper_limit),
                  IV_Merge$Tables[[column_name]][, 4][j],
                  merge_rejected_dataset_V1[, new_column_name])
      } else
      {
        
        level_name <- x_labels[[j]]
        woe <- IV_Merge$Tables[[column_name]][,4][j]
        
        if(is.na(level_name) | level_name == "NA")
        {
          merge_rejected_dataset_V1[which(is.na(merge_rejected_dataset_V1[,column_name])),new_column_name]<- woe
        }else
        {
          merge_rejected_dataset_V1[which(merge_rejected_dataset_V1[,column_name]==level_name),new_column_name]<- woe
        }
      }
    }
  }
  
  # Saving only WOE columns
  
  merge_rejected_dataset_V1_final<- merge_rejected_dataset_V1[,!colnames(merge_rejected_dataset_V1) %in% c("Age",
                                                                                                           "Gender",
                                                                                                           "Marital.Status..at.the.time.of.application.",
                                                                                                           "No.of.dependents",
                                                                                                           "Income",
                                                                                                           "Education",
                                                                                                           "Profession",
                                                                                                           "Type.of.residence",
                                                                                                           "No.of.months.in.current.residence",
                                                                                                           "No.of.months.in.current.company",
                                                                                                           "No.of.times.90.DPD.or.worse.in.last.6.months",
                                                                                                           "No.of.times.60.DPD.or.worse.in.last.6.months",
                                                                                                           "No.of.times.30.DPD.or.worse.in.last.6.months",
                                                                                                           "No.of.times.90.DPD.or.worse.in.last.12.months",
                                                                                                           "No.of.times.60.DPD.or.worse.in.last.12.months",
                                                                                                           "No.of.times.30.DPD.or.worse.in.last.12.months",
                                                                                                           "Avgas.CC.Utilization.in.last.12.months",
                                                                                                           "No.of.trades.opened.in.last.6.months",
                                                                                                           "No.of.trades.opened.in.last.12.months",
                                                                                                           "No.of.PL.trades.opened.in.last.6.months",
                                                                                                           "No.of.PL.trades.opened.in.last.12.months",
                                                                                                           "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                                                                                                           "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                                                                                                           "Presence.of.open.home.loan",
                                                                                                           "Outstanding.Balance",
                                                                                                           "Total.No.of.Trades",
                                                                                                           "Presence.of.open.auto.loan")]
  
  str(merge_rejected_dataset_V1_final)
  
  ## ---------------------- Score card for the Rejected Dataset ----------------------- 
  
  pred_rejected <- predict(final_merge_model, newdata = merge_rejected_dataset_V1_final, type = "response")
  summary(pred_rejected)
  
  pred_response_rejected <- factor(ifelse(pred_rejected >= 0.047, "Yes", "No"))
  
  merge_rejected_dataset_V1_final$Predicted_Performance.Tag <- pred_response_rejected
  merge_rejected_dataset_V1_final$Predicted_prob <- pred_rejected
  merge_rejected_dataset_V1_final$logit<- log(merge_rejected_dataset_V1_final$Predicted_prob/(1-merge_rejected_dataset_V1_final$Predicted_prob))
  
  points0 = 400
  odds0 = 10
  pdo = 20
  
  factor = pdo / log(2)
  offset = points0 - factor * log(odds0)
  merge_rejected_dataset_V1_final$Score = offset - factor * merge_rejected_dataset_V1_final$logit
  
  summary(merge_rejected_dataset_V1_final$Score)
  # Min Score is 398 and Max Score is 426
  
  
  ### COMPARISION OF APPLICATION SCORECARD WITH REJECTED SCORECARD.
  
  length(which(merge_rejected_dataset_V1_final$Score >= 421))
  
  ## After comparison of Application Scorecard cutoff with Rejected Population Scorecard 
  ## We have found out that there are 7 applicants who have scores greater than cutoff score.
  
  ## Hence based on our model we can approve loan for these 7 applicants.
  ## We can also consider these 7 applicants for revenue loss.
  
  
  
  #######################################################################################################################################################
  #######################################################################################################################################################
  ################################################################ FINANCIAL ASSESMENT ##################################################################
  #######################################################################################################################################################
  
  
  ggplot(merge_dataset_V1_final,aes(x=merge_dataset_V1_final$Score,fill=as.factor(Performance.Tag)))+ geom_histogram()
  
  ggplot(merge_dataset_V1_final,aes(x=merge_dataset_V1_final$Predicted_prob,y=merge_dataset_V1_final$Score))+ geom_point()
  
  # Merging the below datasets so that all the variables and there corresponding woe values along with the Score,Predicted Prob etc is merged together
  
  financial_data <- merge(x=merge_dataset_V1_final,y=merge_dataset_V1,by.x=c("Application.ID"),by.y = c("Application.ID"))
  financial_data<- financial_data[,!colnames(financial_data) %in% c('Performance.Tag.y')]
  
  str(merge_dataset_V1)
  
  colnames(financial_data)[colnames(financial_data)=="Performance.Tag.x"] <- "Performance.Tag"
  
  financial_data_final <- (dplyr::select(financial_data,Application.ID,Outstanding.Balance,Performance.Tag,Predicted_prob,Score))
  financial_data_final$Score <- round(financial_data_final$Score)
  financial_data_final <- mutate(financial_data_final,Expected_Credit_Loss=Predicted_prob*Outstanding.Balance*0.75)
  
  # Merging rejected dataset original dataset to calculate outstanding balance
  Rejected_data_set_RL <- merge(x=merge_rejected_dataset_V1,y=merge_rejected_dataset_V1_final,by.x=c("Application.ID"),by.y = c("Application.ID"))
  nrow(Rejected_data_set_RL)
  
  ############################## Auto approval rate###################################
  
  ## WITHOUT MODEL
  
  nrow(merge_dataset_V1)/(nrow(merge_rejected_dataset)+nrow(merge_dataset_V1)) * 100
  ## 98% of applicants were approved without the model.
  
  
  ## WITH MODEL
  (length(which(financial_data_final$Score>=421)) + length(which(Rejected_data_set_RL$Score >= 421))) / (nrow(financial_data_final) + nrow(Rejected_data_set_RL)) * 100
  
  ## Auto Approval Rate  using the model is 62.31%
  
  
  #############################################CREDIT LOSS CALCULATION##########################################
  #### Expected credit loss = PD*EAD*LGD
  # This formula is taken from our risk analytics module.
  
  ## Weare assuming likelihood that we will recover 25% of the money from the customers.
  
  ## Hence LGD will be 1-0.25 = 0.75
  
  
  Total_Expected_Credit_Loss<- sum(filter(financial_data_final,Performance.Tag==1)$Expected_Credit_Loss,na.rm = TRUE)
  
  Total_Expected_Credit_Loss 
  # 159630034
  
  # We decided cutoff score 421 hence calculating the expected loss with score greater than 421
  
  financial_default_data <- filter(financial_data_final,Performance.Tag==1 & financial_data_final$Score>=421)
  
  Credit_loss_based_on_Scorecard<- sum(financial_default_data$Expected_Credit_Loss,na.rm=T) 
  Credit_loss_based_on_Scorecard
  # 34703380
  
  #Total gain by using score card
  ((Total_Expected_Credit_Loss-Credit_loss_based_on_Scorecard)/(Total_Expected_Credit_Loss))*100
  
  
  # Gain is 78.26% with scorecard. So we are able to reduce credit loss by 78.26% with score card approach.
  # Incremental benefit in terms of credit loss avoided is 78.26%, 
  
  
  #############################################REVENUE LOSS CALCULATION#####################################################
  
  #And the potential loss of revenue due to rejection of good customers. 
  
  
  Revenue_loss_temp<-filter(Rejected_data_set_RL,Rejected_data_set_RL$Score>=421)
  str(Revenue_loss_temp)
  Revenue_loss<- sum(Revenue_loss_temp$Outstanding.Balance,na.rm=T) 
  Revenue_loss
  # 4673367
  
  ########################################################################################
  
  ####### Summary of the inferences made from the analysis are as listed below #######
  
  # Approval rate without score card implementation was 98% which was the main driving factor in net credit loss.
  # With suggested optimal cut off score of 421, avg. 62.31% of applicants would be auto approved.Hence 37.69% applicants would be auto rejected.
  # From P & L point of view, we have implemented scorecard which in turns reduces net credit loss by 78.26%
  # With scorecard implementation we have reduced revenue loss to 4673367 Unit*.
  # *Unit here refers currency value which can be INR,USD etc.
  
  ###################################### END OF ASSIGNMENT ############################################