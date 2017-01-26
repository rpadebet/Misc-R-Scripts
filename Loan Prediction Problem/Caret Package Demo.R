## Using Caret Package on Loan Prediction Problem

    library(caret)
    library()

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
    
    id<-train_processed$Loan_ID
    train_processed$Loan_ID<-NULL
    
    #Converting every categorical variable to numerical using dummy variables
    dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T,sep = "_")
    train_transformed <- data.frame(predict(dmy, newdata = train_processed))
    
    #Checking the structure of transformed train file
    str(train_transformed)
    
    #Converting column names to syntactically valid names
    names(train_transformed)
    colnames(train_transformed)<-sub(make.names(names(train_transformed)),pattern = "[.]",replacement = "")
    
    #Converting the dependent variable back to categorical
    train_transformed$Loan_StatusY<-factor(train_transformed$Loan_StatusY,levels = c(1,0),labels = c("Yes","No"))
    train_transformed$Loan_Status<-NULL
    

## Spliting training set into two parts based on outcome: 75% and 25%
    index <- createDataPartition(train_transformed$Loan_StatusY, p=0.75, list=FALSE)
    trainSet <- train_transformed[ index,]
    testSet <- train_transformed[-index,]
    
    #Checking the structure of trainSet
    str(trainSet)

## Feature selection using rfe in caret
    control <- rfeControl(functions = rfFuncs,
                          method = "repeatedcv",
                          repeats = 3,
                          verbose = FALSE)
    outcomeName<-'Loan_StatusY'
    predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
    Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                             rfeControl = control)
    Loan_Pred_Profile  
    #Taking only the top 5 predictors
    predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome", "Property_Area_Semiurban")
    
    #Taking top 10
    f<-as.formula(paste0("Loan_Status~",paste0(Loan_Pred_Profile$optVariables[1:10],collapse = "+")))
    
## Training models using CARET
    
    names(getModelInfo())
    
    #H2O Init
    library(h2o)
    h2o.init(nthreads=-1)
    
    
    ## Model initialization
    model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
    model_gbm_h2o<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm_h2o')
    model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
    model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
    model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glmboost')
    
## Parameter Tuning
    
    #' If the search space for parameters is not defined, Caret will 
    #' use 3 random values of each tunable parameter and use the cross-validation 
    #' results to find the best set of parameters for that algorithm. 
    
    # Train Control
    fitControl <- trainControl(
        method = "repeatedcv",
        number = 5,
        repeats = 5,
        classProbs = TRUE)
    
    # Find parameters used by model
    modelLookup(model='gbm_h2o')
    
    # Creating Tune Grid 
    grid <- expand.grid(ntrees=c(10,20,50,100,500,1000),
                        learn_rate=c(0.01,0.05,0.1),
                        min_rows = c(3,5,10),
                        max_depth=c(1,5,10),
                        col_sample_rate = c(0.25,0.5,1))
    
    # training the model
    model_gbm_h2o<-train(trainSet[,predictors],trainSet[,outcomeName],
                         method='gbm_h2o',trControl=fitControl,tuneGrid=grid)
    
    
    # Training via Tune Length
    model_rf_tune<-train(trainSet[,predictors],trainSet[,outcomeName],
                     method='rf',trControl=fitControl,tuneLength=10)
    
## Summarizing the model
    print(model_rf_tune)
    plot(model_rf_tune)
    
## Variable Importance

    varImp(object=model_rf)
    varImp(object=model_gbm)
    varImp(object=model_rf_tune)
    
    #Plotting Variable importance for Random Forest
    plot(varImp(object=model_rf),main="RF - Variable Importance")
    plot(varImp(object=model_gbm),main="Boosting - Variable Importance")
    plot(varImp(object=model_nnet),main="Neural Net- Variable Importance")
    
## Predictions
   # Random forest
     predictions<-predict.train(object=model_rf_tune,testSet[,predictors],type="raw")
    table(predictions)
    confusionMatrix(predictions,testSet[,outcomeName]) 
  
   # Generalized Linear Model  
    predictions<-predict.train(object=model_glm,testSet[,predictors],type="raw")
    table(predictions)
    confusionMatrix(predictions,testSet[,outcomeName]) 
   
  # Gradient Boosting Model     
    predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
    table(predictions)
    confusionMatrix(predictions,testSet[,outcomeName]) 
    
  # Neural net Model 
    model_nn_tune<-train(trainSet[,predictors],trainSet[,outcomeName],
                         method='nnet',trControl=fitControl,tuneLength=10)
    predictions<-predict.train(object=model_nn_tune,testSet[,predictors],type="raw")
    table(predictions)
    confusionMatrix(predictions,testSet[,outcomeName]) 
    
    
## Caret Ensemble
    library(caretEnsemble)
    
    
# tuneList=list(
#    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
#        rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
#        nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
#    )
    
    model_list <- caretList(trainSet[,predictors],
        trainSet[,outcomeName],
        trControl=fitControl,
        methodList=c("rf","gbm","nnet","glmboost")
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
    caTools::colAUC(model_predsY, testSet$Loan_StatusY,plotROC = T)
    
    predictions_greedy<-predict(greedy_ensemble,testSet[,predictors],type="raw")
    table(predictions_greedy)
    confusionMatrix(predictions_greedy,testSet[,outcomeName]) 
    #Accuracy: 0.7908
    
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
    colAUC(model_preds3, testSet$Loan_StatusY,plotROC = T)
    
    predictions<-predict.train(object=gbm_ensemble,testSet[,predictors],type="raw")
    table(predictions)
    confusionMatrix(predictions,testSet[,outcomeName]) 
    # Accuracy: 0.7712
    
    
    
    
    
    
    
# Actual Test of the Models
    test<-read.csv("./Data/test.csv")
    test_processed <- predict(preProcValues, test)
    test_processed$Loan_ID<-NULL
    
    dmy2 <- dummyVars(" ~ .", data = test_processed,fullRank = T,sep = "_")
   
    test_transformed <- data.frame(predict(dmy2, newdata = test_processed))
    colnames(test_transformed)<-sub(make.names(names(test_transformed)),pattern = "[.]",replacement = "")
    str(test_transformed)
    
    
    # Ensemble model
    pred_test_en<-predict(greedy_ensemble,test_transformed[,predictors],type="raw")
    table(pred_test_en)
    confusionMatrix(pred_test_en,test_trans[,outcomeName])
    # Stacking Model
    pred_test_st<-predict(object=gbm_ensemble,test_transformed[,predictors],type="raw")
    table(pred_test_st)
    confusionMatrix(pred_test_st,test_trans[,outcomeName])
    