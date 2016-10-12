setwd("/home/arun/Documents/Upgrad/Course3/GroupCaseStudy")
#*******************LOADING LIBRARIES***********************
library(caret)
library(car)
library(ROCR)
library(Hmisc)
library(ggplot2)
library(caTools)
library(MASS)
library(e1071)
library(class)
#*******************LOADING COMPLETE************************

#***********STARTING Assignment*****************************

#**************STARTING CHECK POINT 1***********************
#Load files
churn_data <- read.csv("churn_data.csv", stringsAsFactors = FALSE)
customer_data <- read.csv("customer_data.csv", stringsAsFactors = FALSE)
internet_data <- read.csv("internet_data.csv", stringsAsFactors = FALSE)

#Check for duplicated primary key customerID
sum(duplicated(churn_data$customerID)) #zero indicating no duplicated rows
sum(duplicated(customer_data$customerID)) #zero indicating no duplicated rows
sum(duplicated(internet_data$customerID)) #zero indicating no duplicated rows

# Collate the 3 files in a single file.
churn <- merge(churn_data,customer_data,by.x="customerID",by.y="customerID")
churn <- merge(churn,internet_data,by.x="customerID",by.y="customerID")

#Check for NA values
sapply(churn, function(x) sum(is.na(x)))
print("TotalCharges has 11 NA values")

#As number of rows with NA values is less than 1%, remove these rows from analysis
churn <- subset(churn, !is.na(churn$TotalCharges))
str(churn)
#****************END OF CHECK POINT 1***********************

#**************STARTING CHECK POINT 2***********************

#Plots

#****************END OF CHECK POINT 2***********************

#**************STARTING CHECK POINT 3***********************

nms <- c(3,4,5,6,9,10,11,12,13,14,15,16,17,18,19,20,21) 
churn[nms] <- lapply(churn[nms], as.factor) 

#Outlier Treatment - Make Box plots for numeric variables to look for outliers.
boxplot(churn$tenure) #No need of outlier treatment
boxplot(churn$MonthlyCharges) #No need of outlier treatment
boxplot(churn$TotalCharges) #Outlier treatment required
quantile(churn$TotalCharges,seq(0,1,0.01))
churn$TotalCharges[which(churn$TotalCharges > 8039.8830)] <- 8039.8830 #Outliers capped after 99%

#scale variables
churn$tenure <- scale(churn$tenure)
churn$MonthlyCharges <- scale(churn$MonthlyCharges)
churn$TotalCharges <- scale(churn$TotalCharges)


churn_new <- churn
#Creation of dummy variables
churn_new$PhoneService <- as.numeric(churn_new$PhoneService) #Only 2 levels
dummy_1<-model.matrix(~Contract - 1,data=churn_new)
dummy_1<-dummy_1[,-2]
churn_new$PaperlessBilling <- as.numeric(churn_new$PaperlessBilling) #Only 2 levels
dummy_2<-model.matrix(~PaymentMethod - 1,data=churn_new)
dummy_2<-dummy_2[,-1]
churn_new$gender <- as.numeric(churn_new$gender) #Only 2 levels
churn_new$SeniorCitizen <- as.numeric(churn_new$SeniorCitizen) #Only 2 levels
churn_new$Partner <- as.numeric(churn_new$Partner) #Only 2 levels
churn_new$Dependents <- as.numeric(churn_new$Dependents) #Only 2 levels
dummy_3<-model.matrix(~MultipleLines - 1,data=churn_new)
dummy_3<-dummy_3[,-1]
dummy_4<-model.matrix(~InternetService - 1,data=churn_new)
dummy_4<-dummy_4[,-1]
dummy_5<-model.matrix(~OnlineSecurity - 1,data=churn_new)
dummy_5<-dummy_5[,-1]
dummy_6<-model.matrix(~OnlineBackup - 1,data=churn_new)
dummy_6<-dummy_6[,-1]
dummy_7<-model.matrix(~DeviceProtection - 1,data=churn_new)
dummy_7<-dummy_7[,-1]
dummy_8<-model.matrix(~TechSupport - 1,data=churn_new)
dummy_8<-dummy_8[,-1]
dummy_9<-model.matrix(~StreamingTV - 1,data=churn_new)
dummy_9<-dummy_9[,-1]
dummy_10<-model.matrix(~StreamingMovies - 1,data=churn_new)
dummy_10<-dummy_10[,-1]

#create new data frame combining numerical columns and dummy vectors 
#Also remove customer ID
churn_new <- cbind(churn_new[, c(-1,-4,-6,-14,-15,-16,-17,-18,-19,-20,-21)], data.frame(dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7,dummy_8,dummy_9,dummy_10))

#create test and train data
set.seed(100)
split_indices <- sample.split(churn_new$Churn, SplitRatio = 0.70)
train <- churn_new[split_indices == T, ]
test <- churn_new[split_indices == F, ]

#****************END OF CHECK POINT 3***********************

#**************STARTING CHECK POINT 4***********************
#KNN Model

#Naive Bayes

#logistic regression

#svm



