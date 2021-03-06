#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# To run this example use
# ./bin/spark-submit examples/src/main/r/ml/naiveBayes.R

# Load SparkR library into your R session
library(SparkR)
library("SparkR",lib.loc = "/Users/rohitpittu/Library/Caches/spark/spark-2.0.0-bin-hadoop2.7/R/lib/")

# Initialize SparkSession
sparkR.session(appName = "SparkR-ML-naiveBayes-example")

# $example on$
# Fit a Bernoulli naive Bayes model with spark.naiveBayes
titanic <- as.data.frame(Titanic)
titanicDF <- createDataFrame(titanic[titanic$Freq > 0, -5])
nbDF <- titanicDF
nbTestDF <- titanicDF
nbModel <- spark.naiveBayes(nbDF, Survived ~ Class + Sex + Age)

# Model summary
summary(nbModel)

# Prediction
nbPredictions <- predict(nbModel, nbTestDF)
showDF(nbPredictions)
# $example off$
