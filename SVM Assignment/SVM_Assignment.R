##########################################################################################################################
#################################                                #########################################################
#################################   SVM CASE STUDY SOLUTION      #########################################################
#################################                                #########################################################
##### TEAM MEMBERS #######################################################################################################
##### BHARATH KUMAR B.S      #############################################################################################
##########################################################################################################################


# install.packages("caret", depndencies = c("Depends", "Suggests"))
# install.packages("kernlab")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("gridExtra")
#install.packages("backports")
#install.packages("tidyverse")
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(doParallel)

## WE ARE USING PARALLEL PROCESSING TO MAKE THE MODEL BUILD FASTER.PLEASE UN COMMENT IT OUT IN CASE YOU WANT TO USE IT.
#cl <- makePSOCKcluster(5)
#registerDoParallel(cl)


#########################################################################################################################
####STEPS FOLLOWED#######################################################################################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5. Hyperparameter tuning and cross validation
# 6. Final Model using derived hyperparameter.
#########################################################################################################################


# 1. Business Understanding: The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

#########################################################################################################################

# 2. DATA UNDERSTANDING
# EACH DIGIT HAS 784 features - pixels and a label - the digit the image represents.


#########################################################################################################################

# 3. DATA PREPARATION

## WE HAVE 2 SETS OF DATA.WE WILL PERFORM DATA PREPARATION TASKS ON EACH DATA SET RESPECTIVELY.

#########################################################################################################################


##### TRAIN DATA

## LOAD THE TRAIN DATASET AND CHECK ITS DIMENSIONS,STRUCTURE AND SUMMARY
trainData<-read.csv("mnist_train.csv",stringsAsFactors = FALSE)

str(trainData) ## ALL THE VARIABLES ARE IN INT FORMAT 

#Understanding Dimensions
dim(trainData) 
### 59999 ROWS with 785 COLUMNS

#printing first few rows
head(trainData)

#Exploring the data
summary(trainData)


# RENAMING COLUMNS.FIRST COLUMN WILL BE OUR TARGET VARIABLE
colnames(trainData)[1]<- "Number"
colnames(trainData)[2:785] <- c(1:784)

#Making our target class to factor
trainData$Number<-factor(trainData$Number)

## CHECKING THE SUMMARY OF OUR TARGET VARIABLE
summary(trainData$Number)

## WE CAN SEE THAT ALL THE NUMBERs HAVE EQUAL DISTRIBUTION.SO WE NEED NOT LOOK FOR OUTLIERS


####TEST DATA

## SAME STEPS FOR TRAIN DATA

TestData<- read.csv("mnist_test.csv",stringsAsFactors = FALSE)

str(TestData) ## ALL THE VARIABLES ARE IN INT FORMAT 

#Understanding Dimensions
dim(TestData)
### 9999 ROWS with 785 Columns


#printing first few rows
head(TestData)

#Exploring the data
summary(TestData)


# RENAMING COLUMNS.FIRST COLUMN WILL BE OUR TARGET VARIABLE
colnames(TestData)[1]<- "Number"
colnames(TestData)[2:785] <- c(1:784)

#Making our target class to factor
TestData$Number<-factor(TestData$Number)

## CHECKING THE SUMMARY OF OUR TARGET VARIABLE
summary(TestData$Number)

## WE CAN SEE THAT ALL THE NUMBERs HAVE EQUAL DISTRIBUTION.SO WE NEED NOT LOOK FOR OUTLIERS

### DATA CLEANSING

####1.1 Check for Duplicates.
nrow(unique(trainData))==nrow(trainData) 
##No Duplicates

####1 2 Check for NAs
sum(is.na(trainData))
## NO NA VALUES

##REMOVE COLUMNS WHICH HAVE ONLY NA's.WE WON'T BE ABLE TO DERIVE ANYTHING FROM THEM.
##  ALSO REMOVE COLUMNS WHICH HAVE SINGLE  VALUE FOR ALL ROWS
filter_cols <- apply(trainData, 2, function(x) !all(length(unique(x))==1| is.na(x) | x == 0))

## WE HAVE COLUMNS WHICH HAVE SINGLE VALUES FOR ALL ROWS.BUT WE NEED NOT REMOVE THEM SINCE WE ARE USING SCALE = FALSE while building the model.


###CHECK FOR MIN MAX VALUES TO FIND OUTLIERS IN COLUMNS>>
sort(table(unlist(trainData)))
###NO OUTLIERS IN TRAIN DATA>>>>


### SAME PROCEDURE FOR TEST DATA...
nrow(unique(TestData))==nrow(TestData) 

####1 2 Check for NAs
sum(is.na(TestData))


###CHECK FOR MIN MAX VALUES TO FIND OUTLIERS IN COLUMNS>>
sort(table(unlist(TestData)))
###NO OUTLIERS IN TRAIN DATA

#########################################################################################################################

# 4. DATA PREPARATION

#########################################################################################################################

###SINCE TRAIN DATA IS VERY HUGE,WE WILL CONSIDER ONLY 15% OF THE DATA FOR MODEL BUILDING

set.seed(1)
train.indices = sample(1:nrow(trainData), 0.15*nrow(trainData))
modelData = trainData[train.indices, ]

#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(Number~ ., data = modelData, scale = FALSE, kernel = "vanilladot")
### WARNING MESSAGE CAN BE IGNORED

Eval_linear<- predict(Model_linear, TestData)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,TestData$Number)

###RESULTS OF LINEAR MODEL
### Accuracy : 0.9126          
### 95% CI : (0.9069, 0.9181)
### P-Value [Acc > NIR] : < 2.2e-16       
### Kappa : 0.9028

#Using RBF Kernel
Model_RBF <- ksvm(Number~ ., data = modelData, scale = FALSE, kernel = "rbfdot")
### WARNING MESSAGE CAN BE IGNORED

Eval_RBF<- predict(Model_RBF, TestData)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,TestData$Number)

### RESULTS OF RBF KERNEL MODEL
# Accuracy : 0.9551          
# 95% CI : (0.9509, 0.9591)
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.9501 

## THE ACCURACY OF THE MODEL HAS INCREASED FROM 90 to 95% WHEN WE USE RBF KERNEL MODEL.

Model_RBF

### FROM THE MODEL WE HAVE 
#COST(C) = 1 
#sigma =  1.61077104725183e-07

### WE WILL USE THE SAME PARAMETERS AS BASELINE FOR CROSS VALIDATIONS.

#########################################################################################################################

#5. Hyperparameter tuning and Cross Validation

#########################################################################################################################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

## WE ARE USING THE SIGMA & C VALUE WE GOT FROM RBF MODEL AS BASELINE.
set.seed(7)
grid <- expand.grid(.sigma=c(1.61077104725183e-07,2.61077104725183e-07,3.61077104725183e-07), .C=c(1,2,3) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.


fit.svm <- train(Number~., data=modelData, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

# No pre-processing
# Resampling: Cross-Validated (3 fold) 
# Summary of sample sizes: 7199, 7199, 7199 
# Resampling results across tuning parameters:
#   
#   sigma       C  Accuracy   Kappa    
# 1.610771e-07  1  0.9525493  0.9472454
# 1.610771e-07  2  0.9575497  0.9528049
# 1.610771e-07  3  0.9588838  0.9542881
# 2.610771e-07  1  0.9587720  0.9541640
# 2.610771e-07  2  0.9623284  0.9581176
# 2.610771e-07  3  0.9628841  0.9587352
# 3.610771e-07  1  0.9618836  0.9576243
# 3.610771e-07  2  0.9636618  0.9596006
# 3.610771e-07  3  0.9638839  0.9598474
# 
# Accuracy was used to select the optimal model  using the largest value.
# The final values used for the model were 
# sigma = 3.610771e-07 and C = 3.

## PLOT
plot(fit.svm)

#########################################################################################################################

# 6. Final Model using derived hyperparameter.

#########################################################################################################################
### BUILDING THE FINAL RBF MODEL with Above parameters.

RBF_final_model <- ksvm(Number~.,data=modelData,kernel="rbfdot",scale=FALSE,C=3,kpar=list(sigma=3.610771e-07))

RBF_final_model

Eval_final_RBF<- predict(RBF_final_model, TestData)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_final_RBF,TestData$Number)


#########################################################################################################################


#### CONCLUSION
#The final RBF model obtained has the following Tuned Hyper Parameters:
#Tuned HyperParameter Penalty for Misclassification Cost [C=3]
#Tuned Hyperparameter Sigma =  3.610771e-07 

# THE FINAL RBF MODEL HAS ACCURACY OF 0.96930[96.93%] and since hyper parameters are not high the model is not overfitted.
# SENSITIVITY RANGING FROM 95% TO 98.9%
# SPECIFICITY AT 99 %

#########################################################################################################################
