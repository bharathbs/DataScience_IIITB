##########################################################################################################################
#################################                                #########################################################
################################# GRAMENER CASE STUDY SOLUTION   #########################################################
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
install.packages("ggplot2")
install.packages("lubridate")
install.packages("gridExtra")
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(gridExtra)


#########################STEP 1 : READ FILE AND CHECK THE STRUCTURE OF DATA ############################################

loan_Data<-read.csv("loan.csv",stringsAsFactors = FALSE)

str(loan_Data)

head(loan_Data)

#######################STEP 2 : DATA CLEANING #########################################################################

####A. FIXING COLUMNS - DELETE UNNECESSARY COLUMNS

##A1.REMOVE COLUMNS WHICH HAVE ONLY NA's.WE WON'T BE ABLE TO DERIVE ANYTHING FROM THEM.
##  ALSO REMOVE COLUMNS WHICH HAVE SINGLE  VALUE FOR ALL ROWS

filter_cols <- apply(loan_Data, 2, function(x) !all(length(unique(x))==1| is.na(x) | x == 0))
loan<-loan_Data[,filter_cols]

##A2. DELETING IRREVALANT COLUMNS AS WE ARE NOT CONSIDERING THEM INTO OUR ANAYSIS.

loan<-loan[ , -which(names(loan) %in% c("installment","emp_title","url","desc","title","zip_code","sub_grade","total_rec_prncp","total_rec_int","last_pymnt_d","last_pymnt_amnt","last_credit_pull_d","total_pymnt_inv","total_pymnt","open_acc","total_acc"))]


####B. FIX MISSING VALUES (DELETE ROWS, COLUMNS)

##B1. DELETING COLUMNS WHICH HAVE MORE THAN 50% OF DATA AS N\A,NULL,0

loan<-loan[, -which(colMeans(is.na(loan)) > 0.5)]

loan<-loan[, sapply(loan, function(x) length(x[!( x == ""|x == 0)]) / length(x) >= 0.5)]


####C.STANDARDISE VALUES

##C1. STANDARDISE EMP_LENGTH COLUMN 
##  CONVERT < 1 YEARS TO 0 AND 10+YEARS TO 10.REMOVING YEARS FROM THE STRING.ALSO FIXING MISSING VALUES.


loan$emp_length <- gsub("years","year",loan$emp_length) 
loan$emp_length <- gsub("year","",loan$emp_length) 
loan$emp_length <- gsub("\\+","",loan$emp_length)
loan$emp_length <- gsub("< 1","0",loan$emp_length)

## As discussed in discussion forum.

loan$emp_length[loan$emp_length=='n/a']<-0
loan$emp_length <- as.numeric(loan$emp_length)


##C2. CONVERT DATES INTO R FORMAT.

loan$issue_d<-as.Date(paste("01-", loan$issue_d, sep = ""), format = "%d-%b-%y")
loan$earliest_cr_line<-as.Date(paste("01-", loan$earliest_cr_line, sep = ""), format = "%d-%b-%y")


##C3. CONVERT COLUMNS CONTAINING % TO NUMERIC

loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate))
loan$revol_util <- as.numeric(gsub("%", "", loan$revol_util))



##C4. REMOVE OUTLIERS
# CHECK AND REMOVE OUTLIERS FOR ANNUAL INCOME.WE ARE CONSIDERING VALUES ONLY TILL UPPER QUARTILES.

boxplot(loan$annual_inc)
quantile(loan_Data$annual_inc,  probs = c(1:100)/100)

## WE ARE ANALYSING OUTLIERS USING QUARTILES.AND WE ARE CHOOSING 99TH QUARTILE TO REMOVE OUTLIERS.

loan <- loan%>%subset(loan$annual_inc<=quantile(loan$annual_inc,.99))
boxplot(loan$annual_inc)


##C5. OVER PRECISION. ROUNDING FUNDED_AMNT

loan$funded_amnt_inv<-round(loan$funded_amnt_inv,2)

##C5 .CONVERT INTO FACTORS

loan$term<-factor(loan$term)

##########################################DATA CLEANING ENDS Here######################################################

##############################STEP 3: FILTERING DATA ##################################################################

## we ARE NOT CONSIDERING LOAN RECORDS WHICH HAVE LOAN STATUS AS CURRENT.

loan<-loan %>% subset(loan_status!="Current")

#CHECK DUPLICATE IN ID AND MEMBER ID COLUMN

#ID

dup <- data.frame(table(loan$id))

dup[dup$Freq > 1,]

#MEMBER_ID

dup1 <- data.frame(table(loan$member_id))

dup1[dup1$Freq > 1,]

## REMOVING ID AND MEMBER ID COLUMNS AS IT IS NOT REQUIRED

loan<-loan[ , -which(names(loan) %in% c("id","member_id"))]


##############################FILTERING DATA END HERE###################################################################

############################# STEP 4 - DERIVED VARIABLES ###############################################################

##4.1 FUNDING AMOUNT BIN

loan<-mutate(loan,funded_amnt_bin=ifelse(loan$funded_amnt>=500 & loan$funded_amnt<10000,"500-10000",
                                         ifelse(loan$funded_amnt>=10000 & loan$funded_amnt<20000,"10000-20000",
                                                ifelse(loan$funded_amnt>=20000 & loan$funded_amnt<=35000,"20000+","NA"))))

loan$funded_amnt_bin<-factor(loan$funded_amnt_bin,levels=c("500-10000","10000-20000","20000+"))

##4.2 INTEREST RATE BIN

loan<-mutate(loan,int_rate_bin=ifelse(loan$int_rate>=5 & loan$int_rate<10,"5-10",
                                      ifelse(loan$int_rate>=10 & loan$int_rate<15,"10-15",
                                             ifelse(loan$int_rate>=15 & loan$int_rate<20,"15-20",
                                                    ifelse(loan$int_rate>=20 & loan$int_rate<=25,"20-25","NA")))))


loan$int_rate_bin<-factor(loan$int_rate_bin,levels=c("5-10","10-15","15-20","20-25"))


## 4.3 DTI BIN

max(loan$dti) 
min(loan$dti)

loan<-mutate(loan,dti_bin = ifelse(loan$dti>=0 & loan$dti<5,"0-5",
                                   ifelse(loan$dti>=5 & loan$dti<10,"5-10",
                                          ifelse(loan$dti>=10 & loan$dti<15,"10-15",
                                                 ifelse(loan$dti>=15 & loan$dti<20,"15-20",
                                                        ifelse(loan$dti>=20 & loan$dti<=25,"20-25",
                                                               ifelse(loan$dti>=25 & loan$dti<=30,"25-30","NA")))))))


loan$dti_bin<-factor(loan$dti_bin,levels=c("0-5","5-10","10-15","15-20","20-25","25-30"))

## 4.4 ANNUAL_INC BIN

max(loan$annual_inc)
min(loan$annual_inc)

loan<-mutate(loan,annual_inc_bin = ifelse(loan$annual_inc>=0 & loan$annual_inc<20000,"0-20000",
                                          ifelse(loan$annual_inc>=20000 & loan$annual_inc<40000,"20000-40000",
                                                 ifelse(loan$annual_inc>=40000 & loan$annual_inc<60000,"40000-60000",
                                                        ifelse(loan$annual_inc>=60000 & loan$annual_inc<80000,"60000-80000",
                                                               ifelse(loan$annual_inc>=80000 & loan$annual_inc<=100000,"80000-100000",">100000"))))))

loan$annual_inc_bin<-factor(loan$annual_inc_bin,levels=c("0-20000","20000-40000","40000-60000","60000-80000","80000-100000",">100000"))


## 4.5 DERIVING CREDIT_LINE FROM ISSUE_D AND EARLIEST_CR_LINE

loan<- mutate(loan,loan_issue_year=year(loan$issue_d))
loan<- mutate(loan,ear_cr_line_year=year(loan$earliest_cr_line))
loan<-mutate(loan,length_of_credit_line=abs(loan$loan_issue_year - loan$ear_cr_line_year))

#CONVERTING CREDIT_LINE LENGTH IN NUMERIC

loan$length_of_credit_line<-as.numeric(loan$length_of_credit_line)


loan<-mutate(loan,length_of_credit_line_bin=ifelse(loan$length_of_credit_line>=0 & loan$length_of_credit_line<10,"0-10",
                                                   ifelse(loan$length_of_credit_line>=10 & loan$length_of_credit_line<20,"10-20",
                                                          ifelse(loan$length_of_credit_line>=20 & loan$length_of_credit_line<30,"20-30",
                                                                 ifelse(loan$length_of_credit_line>=30 & loan$length_of_credit_line<40,"30-40",
                                                                        ifelse(loan$length_of_credit_line>=40 & loan$length_of_credit_line<50,"40-50",
                                                                               ifelse(loan$length_of_credit_line>=50 & loan$length_of_credit_line<=60,"50-60","NA")))))))

loan$length_of_credit_line_bin<-factor(loan$length_of_credit_line_bin,levels=c("0-10","10-20","20-30","30-40","40-50","50-60"))

############################# DERIVED VARIABLES ENDS HERE ##############################################################

############################ STEP 5: UNIVARIATE ANALYSIS ###############################################################

### A. ORDERED UNIVARIATE ANALYSIS


##A1. LOAN STATUS

ggplot(loan, aes (x = loan$loan_status,fill=loan$loan_status)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle(" Loan Status distribution ") +
  xlab("Loan Status") + ylab("Count") + 
  labs(fill="Loan Status") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: AROUND 85% DATA IS SHARED BY FULLY PAID LOANS AND 14.67% IS SHARED BY CHARGED OFF LOANS.
#            IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS.


##A2. GRADE

ggplot(loan, aes (x = loan$grade,fill=loan$grade)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle(" Grade distribution ") +
  xlab("Grade ") + ylab("Count") + 
  labs(fill="Grade") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: LOANS DISTRIBUTED UNDER THE GRADE B IS MORE AS COMPARE TO OTHER GRADES.
#           IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS.


## A3. FUNDING AMOUNT

ggplot(loan, aes (x = loan$funded_amnt_bin,fill=loan$funded_amnt_bin)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  ggtitle(" Funded amount distribution ") +
  xlab("Funded amount bin") + ylab("Count") + 
  labs(fill="Bin level") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)


la<-ggplot(loan,aes(x=1,y=loan$loan_amnt))+geom_boxplot() 
fa<-ggplot(loan,aes(x=1,y=loan$funded_amnt))+geom_boxplot() 
fi<-ggplot(loan,aes(x=1,y=loan$funded_amnt_inv))+geom_boxplot() 

grid.arrange(la,fa,fi,nrow=1)

#INFERENCE: IN GENERAL 500-10000 CURRENCY FUNDS LOANS ARE ASSIGNED .
#           FROM THE BOX PLOT WE CAN INFER THAT THERE IS NOT MUCH VARIANCE B\W THE 3 VARIABLEs,HENCE WE ARE CONSIDERING FUNDED AMOUNT FOR FURTHER ANALYSIS
#           IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS.

## A4. EMP LENGTH 

ggplot(loan, aes (x = factor(loan$emp_length),fill=factor(loan$emp_length))) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle(" Employee Length Distribution ") +
  xlab("Employee Experience") + ylab("Count") + 
  labs(fill="No of years") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: WE CAN INFER THAT MAXIMUM NO OF LOANS ARE APPLIED BY 10+ YEARS EXPERIENCED PERSONS.


## A5. VERIFICATION STATUS

ggplot(loan, aes (x = loan$verification_status,fill=loan$verification_status)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle(" Verification Status of Loan distribution ") +
  xlab("Verification Status") + ylab("Count") + 
  labs(fill="Status") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: WE CAN INFER THAT DOCUMENTS ARE NOT VERIFIED IN MOST OF THE LOANS.

## A6. INTEREST RATE

ggplot(loan, aes (x = loan$int_rate_bin,fill=loan$int_rate_bin)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle(" Interest Rate-wise distribution ") +
  xlab("Interest Rate bin (in %)") + ylab("Count") + 
  labs(fill="Interest Rate Bin") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: LOANS WERE DISTRIBUTED MORE IN THE INTEREST RATE RANGE 10-15 %.
#           IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS.


### A7 : DTI

ggplot(loan, aes (x = loan$dti_bin,fill=loan$dti_bin)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Dti Wise distribution ") +
  xlab("Dti bin (in %)") + ylab("Count") + 
  labs(fill="Dti Bin") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: LOANS WERE DISTRIBUTED MORE IN THE DTI RANGE 10-15 %.

### A8 : ISSUE DATE

ggplot(loan, aes (x = loan$issue_d,fill=loan$issue_d)) +
  geom_histogram(fill="blue",binwidth = 12)+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Loan Issued Year wise  ") +
  xlab("Years") + ylab("Count") + 
  theme_classic()

#INFERENCE: MORE LOANS ARE DISTRIBUTED AS YEAR INCREASES
#           WE ARE DERIVING CREDIT_LINE FROM ISSUE DATE AND EARLIEST CREDIT LINE 
#           AND USING THAT INTO BIVARIATE ANALYSIS.

### A9 : TERM


ggplot(loan, aes (x = loan$term,fill=loan$term)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Term wise distribution ") +
  xlab("Terms") + ylab("Count") + 
  labs(fill="Months") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: SHORT TERM LOAN APPLICANTS ARE MORE AS COMPARE TO LONG TERM LOANS.
#           IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS.


### A10 : ANNUAL INCOME


ggplot(loan, aes (x = loan$annual_inc_bin,fill=loan$annual_inc_bin)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Annual Income distribution ") +
  xlab("Annual Income bin (in $)") + ylab("Count") + 
  labs(fill="Annual Income Bin") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: LOAN ISSUED FOR ANNUAL INCOME B/W 40K-60K IS MORE AS COMPARE TO OTHER ANNUAL INCOME RANGE.
#           IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS.


###B . UNORDERED UNIVARIATE ANALYSIS

### B1 : PURPOSE

ggplot(loan, aes (x = loan$purpose,fill=loan$purpose)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  ggtitle(" Purpose wise distribution ") +
  xlab("Purpose") + ylab("Count") + 
  labs(fill="Purpose") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: MORE LOANS ARE ASSIGNED FOR PURPOSE OF DEBT_CONSOLIDATION. 
#           IT IS IMPORTANT ASPECT AND WE ARE CONSIDERING IT FOR BIVARIATE ANALYSIS. 

### B2 : HOME OWNERSHIP

ggplot(loan, aes (x = loan$home_ownership,fill=loan$home_ownership)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Home Ownership wise distribution ") +
  xlab("Home Ownership") + ylab("Count") + 
  labs(fill="Home Ownership") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE: MORE LOANS ARE ASSIGNED FOR RENTED AND MORTGAGED OWNERS.

### B3 : ADDR_STATE

ggplot(loan, aes (x = loan$addr_state,fill=loan$addr_state)) +
  geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" State wise distribution ") +
  xlab("State") + ylab("Count") + 
  labs(fill="States") +
  theme_classic() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#INFERENCE : MORE LOANS ARE ASSIGNED FOR CALIFORNIA STATE


########################################################################################################################
## INFERENCE FROM UNIVARIATE ANALYSIS

## WE ARE CONSIDERING GRADE, LOAN STATUS, FUNDED AMOUNT, INTEREST RATE,  CREDIT_LINE, TERM, ANNUAL INCOME, PURPOSE,ADDRESS STATE FOR BIVARIATE ANALYSIS.

############################ UNIVARIATE ANALYSIS END HERE ##############################################################


#########################DERIVED METRICS ############################################################################


###Data Driven
#1.length_of_credit_line(Absolute difference of issue_d and earliest_cr_line)


###Type Driven 
#1.Purpose
#2.Grade
#3.Term
#4.Annual Income

### BUSINESS DRIVEN
#1.INTEREST RATE
#2.LOAN STATUS.


#####################################################################################################################


############################ STEP 6 - SEGMENTED UNIVARIATE AND BIVARIATE ANALYSIS ######################################


## 6.1 LOAN STATUS VS TERM

ggplot(loan, aes(fill=loan$loan_status,x=loan$term)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Loan Status vs Term ") +
  xlab("Term") + ylab("Count") + 
  labs(fill="Loan Status") +
  theme_classic()  

#INFERENCE: MORE LOANS ARE CHARGED OFF FOR LARGER TERM.


## 6.2 LOAN STATUS VS INTEREST RATE

ggplot(loan, aes (x = loan$int_rate_bin,fill=loan$loan_status)) +
  geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Loan Status vs Interest rate ") +
  xlab("Interest rate bin") + ylab("Count") + 
  labs(fill="Loan Status") +
  theme_classic() 

#INFERENCE: MORE LOANS ARE CHARGED OFF IF INTEREST RATES ARE HIGH.


## 6.3 LOAN STATUS VS GRADE 

ggplot(loan, aes (x = loan$grade,fill=loan$loan_status)) +
  geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Grade vs Loan status ") +
  xlab("Grade") + ylab("Count") + 
  labs(fill="Loan Status ") +
  theme_classic() 

#INFERENCE: IF GRADES ARE INCREASING FROM A-G MORE LOANS ARE GETTING CHARGED OFF.


## 6.4 LOAN STATUS VS ANNUAL INCOME

ggplot(loan, aes (x = loan$annual_inc_bin,fill=loan$loan_status)) +
  geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Annual Income vs Loan status ") +
  xlab("Annual Income (in bins)") + ylab("Count") + 
  labs(fill="Loan Status ") +
  theme_classic() 

#INFERENCE: MORE CHARGED OFF ARE THERE FOR LOW ANNUAL INCOME.


## 6.5 LOAN STATUS VS FUNDED AMOUNT 

ggplot(loan, aes (x = loan$funded_amnt_bin,fill=loan$loan_status)) +
  geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Funded Amount vs Loan status ") +
  xlab("Funded Amount(in bins)") + ylab("Count") + 
  labs(fill="Loan Status ") +
  theme_classic() 

#INFERENCE: MORE LOANS ARE CHARGED OFF FOR HIGH FUNDED AMOUNTS.


## 6.6 LOAN STATUS VS PURPOSE 

ggplot(loan, aes (x = loan$purpose,fill=loan$loan_status)) +
  geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 0, hjust = 1))  +
  ggtitle(" Purpose vs Loan status ") +
  xlab("Purpose") + ylab("Count") + 
  labs(fill="Loan Status ") +
  theme_classic() 

#INFERENCE: MORE LOANS ARE CHARGED OFF IF FUNDS ARE TAKEN FOR SMALL BUSINESS


## 6.7 LOAN STATUS VS ADDRESS STATE

ggplot(loan, aes (x = loan$addr_state,fill=loan$loan_status)) +
  geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ggtitle(" Address State vs Loan Status ") +
  xlab("Address State") + ylab("Count") + 
  labs(fill="Loan status ") +
  theme_classic()

#INFERENCE: WE DONT HAVE A CONCRETE PATTERN ON STATES VS LOAN STATUS.WE ARE CONSIDERING NEBRASKA STATE(NE) AS A OUTLIER BECAUSE NO OF APPLICANTS ARE ONLY 5.
##          SO ITS NOT A DRIVING FACTOR FOR FINAL RECOMMENDATION.


## 6.8 LOAN STATUS VS CREDIT LINE

ggplot(loan,aes(x=loan$length_of_credit_line_bin,fill=loan$int_rate_bin)) + geom_bar(position = "fill")+
  ggtitle("Length of credit year vs interest rate analysis") +
  xlab("Length of credit line in years bin") + ylab("Count") +labs(fill="Interest Rate Bin") + theme_classic()

#INFERENCE: AS LENGTH OF CREDIT LINE INCREASES , INTEREST RATE ON THE LOANS ARE DECREASING.

## 6.9 DTI VS LOAN STATUS

ggplot(loan,aes(x=loan$dti_bin,fill=loan$loan_status)) + 
  geom_bar(position = "fill")+  
  ggtitle("Dti Vs Loan Status") +   xlab("Dti Bin") + ylab("Count") +labs(fill="Loan Status") + theme_classic()

# INFERENCE : WE DONT HAVE A CONCRETE PATTERN OF DTI VS LOAN STATUS,HENCE IGNORING IT.



############################ SEGMENTED UNIVARIATE AND BIVARIATE ANALYSIS ENDS HERE######################################


############################################### 7. RECOMMANDATIONS###############################################################################

##FOLLOWING ARE THE RECOMMENDATIONS TO PREVENT LOANS BEING CHARGED OFF:

#FUNDING AMOUNT SHOULD BE GRANTED LOW IF GRADE IS INCREASING FROM A-G. AS MORE LOANS ARE BEING CHARGED OFF WHEN GRADE INCREASES FROM A-G.
#LONG TERM LOANS SHOULD BE CONSIDERED AS ONE OF THE RISK FACTOR BEFORE APPROVAL OF THE LOAN.
#RATE OF CHARGED OFF LOANS ARE MORE FOR LONG TERM LOANS.
#FUNDING AMOUNT SHOULD BE LOW FOR HIGHER VALUE OF INTEREST RATE. AS MORE LOANS ARE BEING CHARGED OF MORE THAN 10-15 % OF INTEREST RATE.
#ANNUAL INCOME LESS THAN 20K-40K PA SHOULD BE CONSIDERED AS RISK FACTOR. RATE OF CHARGED OFF LOANS ARE DECREASING WHEN ANNUAL INCOME INCREASES.
#THOUGH WE HAVE NOT RECEIVED ANY CONCRETE PATTERN ON PURPOSE CODE BUT WE HAVE OBSERVED THAT LOANS WHICH HAVE PURPOSE CODE SMALL BUSINESS HAVE HIGHER RATE OF CHARGED OFF LOANS.
#LONGER CREDIT LINE HISTORY SHOULD BE CONSIDERED FOR ANALYZING RISK. AS THIS WILL GIVE 360 DEGREE VIEW ON APPLICANTS PAST BEHAVIOR ON OPTED LOANS.


#############