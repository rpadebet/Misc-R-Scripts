###############################################################
## http://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/ ##
###############################################################


library(h2o)
h2o.init(nthreads=-1)
## optional: connect to a running H2O cluster
#h2o.init(ip="mycluster", port=55555) 

## 'path' can point to a local file, hdfs, s3, nfs, Hive, directories, etc.
df <- h2o.importFile(path = "http://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/titanic.csv",)
dim(df)
head(df)
tail(df)
summary(df,exact_quantiles=TRUE)

## pick a response for the supervised problem
response <- "survived"

## the response variable is an integer, we will turn it into a categorical/factor for binary classification
df[[response]] <- as.factor(df[[response]])           

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df), c(response, "name")) 

## We split the data into three pieces: 60% for training, 20% for validation, 20% for final testing
splits <- h2o.splitFrame(
  data = df, 
  ratios = c(0.6,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]


## We only provide the required parameters, everything else is default
gbm <- h2o.gbm(x = predictors, y = response, training_frame = train)

## Show a detailed model summary
gbm

## Get the AUC on the validation set
h2o.auc(h2o.performance(gbm, newdata = valid)) 

## The second model is another default GBM, but trained on 80% of the data (here, we combine the training and validation splits to get more training data), and cross-validated using 4 folds.
## Note that cross-validation takes longer and is not usually done for really large datasets.
## h2o.rbind makes a copy here, so it's better to use splitFrame with `ratios = c(0.8)` instead above
gbm <- h2o.gbm(x = predictors, y = response, 
               training_frame = h2o.rbind(train, valid), 
               nfolds = 4, seed = 0xDECAF)


## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
gbm@model$cross_validation_metrics_summary

## Get the cross-validated AUC by scoring the combined holdout predictions.
## (Instead of taking the average of the metrics across the folds)
h2o.auc(h2o.performance(gbm, xval = TRUE))

## TUNING parameters with early stopping
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       

  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   

  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)

## Get the AUC on the validation set (passed during training)
h2o.auc(h2o.performance(gbm,valid = TRUE))

## Hypersearch of parameters
#' First trying to figure out the depth of the trees necessary
#



## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list( max_depth = seq(1,29,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       

  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid 


## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))

###Predicting
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
preds <- h2o.predict(gbm, test)
head(preds)
gbm@model$validation_metrics@metrics$max_criteria_and_metric_scores


### Ensemble of gbm models

prob = NULL
k=10
for (i in 1:k) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  if (is.null(prob)) prob = h2o.predict(gbm, test)$p1
  else prob = prob + h2o.predict(gbm, test)$p1
}
prob <- prob/k
head(prob)


### Bringing it back to R local to calculate accuracy
probInR  <- as.vector(prob)
labelInR <- as.vector(as.numeric(test[[response]]))
if (! ("cvAUC" %in% rownames(installed.packages()))) { install.packages("cvAUC") }
library(cvAUC)
cvAUC::AUC(probInR, labelInR)



packages<-c("h2o","caret","RPostgreSQL","data.table","quantmod","rJava","RWeka","DBI"
            ,"devtools","e1071","git2r","gbm","Hmisc","haven","httr","IBrokers","IRkernel",
            "xlsx","curl","openssl","XLConnect","RHadoop","xts","zoo","RODBC","glmnet",
            "jsonlite","randomForest","RColorBrewer","readr","ROCR","rvest","shiny","spam",
            "xml2","xtable","rpart","nnet","tm", "SnowballCC","wordcloud","igraph","ngram",
            "textmineR","ada","adabag","C50","foreign","gridExtra","jpeg","lda","MASS",
            "Matrix","ModelMetrics","NLP","plotly","ProjectTemplate","rmarkdown","sparklyr",
            "rsparkling","sqldf","swirl","xgboost")

