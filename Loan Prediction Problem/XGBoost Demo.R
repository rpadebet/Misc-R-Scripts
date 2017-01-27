## XGBoost package Demo

## Using Caret Package+XGBoost on Loan Prediction Problem

library(caret)

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
grid <- expand.grid(nrounds=c(50,200),
                    max_depth=c(1,2),
                    eta=c(0.1,0.01),
                    gamma=0,
                    min_child_weight =2,
                    colsample_bytree = 0.6,
                    subsample = 0.8)

# Training the model
model_xgb<-train(data.matrix(trainSet[,predictors]),trainSet[,outcomeName],
                 method='xgbTree',trControl=fitControl,tuneGrid=grid)


## Summarizing the model
print(model_xgb)
plot(model_xgb)

## Variable Importance

varImp(object=model_xgb)
plot(varImp(object=model_xgb),main="XGB Boosting - Variable Importance")

## Predictions

# on Train Set
predictions<-predict(model_xgb,data.matrix(trainSet[,predictors]),type="raw")
table(predictions)
confusionMatrix(predictions,trainSet[,outcomeName]) 

# on Test Set
predictions<-predict(model_xgb,data.matrix(testSet[,predictors]),type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName]) 






