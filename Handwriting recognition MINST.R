library(h2o)
library(data.table)

h2o.init(port = 54321,nthreads = -1)

#' Download MINST Data files
#' 
#' CSV training set http://www.pjreddie.com/media/files/mnist_train.csv
#' CSV test set http://www.pjreddie.com/media/files/mnist_test.csv


download.file("http://www.pjreddie.com/media/files/mnist_train.csv",
                "~/Downloads/MINST/train.csv")

download.file("http://www.pjreddie.com/media/files/mnist_test.csv",
              "~/Downloads/MINST/test.csv")

# Reading files into local memory
train<-fread(input = "~/Downloads/MINST/train.csv")
test<-fread(input = "~/Downloads/MINST/test.csv")

# Reading files in H2O
train_h2o<-h2o.importFile("/home/rohit/Downloads/MINST/train.csv")
test_h2o<-h2o.importFile("/home/rohit/Downloads/MINST/test.csv")
# Deep neural network model

model <- h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame  = train_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs


## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model, test_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)

# View the specified parameters of your deep learning model

model@parameters
# Examine the performance of the trained model

model
# display all performance metrics

h2o.performance(model)

# Variable Importance
model2 <- 
  h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame  = train_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100,  # max. no. of epochs
                   variable_importances = TRUE, # variable importance
                   validation_frame = test_h2o, # test frame
                   nfolds = 5 ) # cross validation folds


h2o.performance(model2)

plot(model2)

h2o.varimp_plot(model2,25)

h2o.varimp(model2)

# View the specified parameters of your deep learning model

model2@parameters

## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model2, test_h2o)
