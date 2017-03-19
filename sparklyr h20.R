library(sparklyr)
library(rsparkling)
library(h2o)
library(dplyr)

##  http://spark.rstudio.com/h2o.html

## connect to spark - check if needed - two instances installed

# 
library(devtools)
devtools::install_github("h2oai/sparkling-water", ref = "master", subdir = "/r/rsparkling",force=TRUE)
Sys.getenv("SPARK_HOME")
Sys.setenv(SPARK_HOME = "/home/rohit/spark/spark")


## H2o version update (CRAN is few versions behind)
## https://github.com/h2oai/h2o-3/tree/master/h2o-r
repos <- c("https://h2o-release.s3.amazonaws.com/h2o/rel-turing/9/R", getOption("repos"))

# spark connection
sc <- spark_connect(master = "local",version = "2.")

# copy mtcars dataset into spark
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

#using sql with spark
library(DBI)
mtcars_preview <- dbGetQuery(sc, "SELECT * FROM mtcars LIMIT 10")
mtcars_preview

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

training <- as_h2o_frame(sc,partitions$training,strict_version_check = F)
test <- as_h2o_frame(sc,partitions$test,strict_version_check = F)


# fit a linear model to the training dataset
fit <- h2o.glm(x = c("wt", "cyl"), 
               y = "mpg", 
               training_frame = training,
               lambda_search = TRUE)

print(fit)



library(ggplot2)

# compute predicted values on our test dataset
pred <- h2o.predict(fit, newdata = test)
# convert from H2O Frame to Spark DataFrame
predicted <- as_spark_dataframe(sc, pred,strict_version_check = F)

# extract the true 'mpg' values from our test dataset
actual <- partitions$test %>%
  select(mpg) %>%
  collect() %>%
  `[[`("mpg")

# produce a data.frame housing our predicted + actual 'mpg' values
data <- data.frame(
  predicted = predicted,
  actual    = actual
)
# a bug in data.frame does not set colnames properly; reset here 
names(data) <- c("predicted", "actual")

# plot predicted vs. actual values
ggplot(data, aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual Fuel Consumption",
    y = "Predicted Fuel Consumption",
    title = "Predicted vs. Actual Fuel Consumption"
  )
