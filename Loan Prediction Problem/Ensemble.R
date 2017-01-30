## Caret Ensemble
library(caretEnsemble)
model_list <- caretList(trainSet[,predictors],
                        trainSet[,outcomeName],
                        trControl=fitControl,
                        methodList=c("rf","nnet","glm","C5.0","rpart")
)

# Model Correlation
modelCor(resamples(model_list))

# Ensemble
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)
## Predictions
library("caTools")


model_preds <- lapply(model_list, predict, newdata=testSet[,predictors], type="prob")
model_predsY <- lapply(model_preds, function(x) x[,"No"])
model_predsY <- data.frame(model_predsY)
ens_preds <- predict(greedy_ensemble, newdata=testSet[,predictors], type="prob")
model_predsY$ensemble <- ens_preds
caTools::colAUC(model_predsY, testSet$Loan_Status,plotROC = T)

predictions_greedy<-predict(greedy_ensemble,testSet[,predictors],type="raw")
table(predictions_greedy)
confusionMatrix(predictions_greedy,testSet[,outcomeName]) 



## Caret Stack
# Caret Stack
gbm_ensemble <- caretStack(
    model_list,
    method="gbm",
    tuneLength=10,
    metric="ROC",
    trControl=trainControl(
        method="boot",
        number=10,
        savePredictions="final",
        classProbs=TRUE,
        summaryFunction=twoClassSummary
    )
)

model_preds3 <- model_predsY
model_preds3$ensemble <- predict(gbm_ensemble, newdata=testSet[,predictors], type="prob")
colAUC(model_preds3, testSet$Loan_Status,plotROC = T)

predictions<-predict(object=gbm_ensemble,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName]) 
