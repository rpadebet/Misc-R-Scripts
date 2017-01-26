## XGBoost package Demo

## Using Caret Package+XGBoost on Loan Prediction Problem

library(caret)

## Downloading Data

url<-"https://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii/media/"

# Train File
download.file(paste0(url,"train_file/train_u6lujuX_CVtuZ9i.csv"),"./Data/train.csv",method="curl")
# Test File
download.file(paste0(url,"test_file/test_Y3wMUE5_7gLdaTN.csv"),"./Data/tEST.csv",method = "curl")

## Loading Files
train<-read.csv("./Data/train.csv")
test<-read.csv("./Data/test.csv")

## Structure of the Data
str(train)

## PreProcess Data
sum(is.na(train))
#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(train, method = c("knnImpute","center","scale"))

library('RANN')
train_processed <- predict(preProcValues, train)
sum(is.na(train_processed))

#Converting outcome variable to numeric
train_processed$Loan_Status<-ifelse(train_processed$Loan_Status=='N',"N","Y")

#Remove Loan ID from training
id<-train_processed$Loan_ID
train_processed$Loan_ID<-NULL

# One Hot Encoding
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T,sep = "_")
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
numeric_cols<-c("ApplicantIncome","CoapplicantIncome","LoanAmount","Loan_Amount_Term","Credit_History")
train_fact<-lapply(train_transformed[,!names(train_transformed) %in% numeric_cols],as.factor)
train_transformed<-cbind(data.frame(train_fact),train_transformed[,numeric_cols])
colnames(train_transformed)<-sub(make.names(names(train_transformed)),pattern = "[.]",replacement = "")
train_transformed$Loan_StatusY<-factor(train_transformed$Loan_StatusY,levels = c(1,0),labels = c("Yes","No"))



## Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_transformed$Loan_Status, p=0.75, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]

## Feature selection using rfe in caret

str(trainSet)
control <- rfeControl(functions = treebagFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'Loan_StatusY'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

Loan_Pred_Profile  

#Taking only the top 10 predictors
predictors<-Loan_Pred_Profile$optVariables[1:10]


## Training models using CARET

# Param lookup
names(getModelInfo())
modelLookup(model='xgbTree')
library(xgboost)

# Train Control
fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    classProbs = TRUE,
    allowParallel = T,
    verboseIter = T,
    summaryFunction = twoClassSummary)

# Search Grid
grid <- expand.grid(nrounds=c(100,500),
                    max_depth=c(1,3),
                    eta=c(0.1,0.05,0.01),
                    gamma=c(0,1),
                    min_child_weight =2,
                    colsample_bytree = 0.5,
                    subsample = 0.5)

# Training the model
model_xgb<-train(data.matrix(trainSet[,predictors]),trainSet[,outcomeName],
                 method='xgbTree'
                 ,trControl=fitControl,tuneGrid=grid)


## Summarizing the model
print(model_xgb)
plot(model_xgb)

## Variable Importance

varImp(object=model_xgb)
plot(varImp(object=model_xgb),main="XGB Boosting - Variable Importance")

## Predictions

predictions<-predict(model_xgb,data.matrix(testSet[,predictors]),type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName]) 







