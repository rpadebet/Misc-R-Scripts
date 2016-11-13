library(h2o)
h2o.init(nthreads=-1)
## optional: connect to a running H2O cluster
#h2o.init(ip="mycluster", port=55555) 

## 'path' can point to a local file, hdfs, s3, nfs, Hive, directories, etc.
df <- h2o.importFile(path = "http://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/titanic.csv")
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
