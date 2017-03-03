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


# Lubuntu
Sys.setenv(SPARK_HOME = "/home/rohit/spark/spark")
# Mac
Sys.setenv(SPARK_HOME = "/Users/rohitpittu/spark/spark-2.1.0-bin-hadoop2.7")

# Fresh Install
spark_install(version = "2.0.0",hadoop_version = "2.7")
Sys.setenv(SPARK_HOME = paste0(spark_install_dir(),"/spark-2.0.0-bin-hadoop2.7"))

## H2o version update (CRAN is few versions behind)
## https://github.com/h2oai/h2o-3/tree/master/h2o-r
repos <- c("https://h2o-release.s3.amazonaws.com/h2o/rel-turing/9/R", getOption("repos"))

# spark connection
sc <- spark_connect(master = 'local',version="2.0.0")

# copy mtcars dataset into spark
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

#using sql with spark
library(DBI)
mtcars_preview <- dbGetQuery(sc, "SELECT * FROM mtcars LIMIT 10")
mtcars_preview

# transform our data set, and then partition into 'training', 'test'
require(dplyr)
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)


# fit a linear model to the training dataset
fit <- partitions$training %>%
    ml_linear_regression(response = "mpg", features = c("wt", "cyl"))

fit
summary(fit)

predicted<-sdf_predict(fit,partitions$test)

# To be used with rsparkling
# training <- as_h2o_frame(sc,partitions$training,strict_version_check = F)
# test <- as_h2o_frame(sc,partitions$test,strict_version_check = F)
# compute predicted values on our test dataset
# pred <- h2o.predict(fit, newdata = test)
# convert from H2O Frame to Spark DataFrame
# predicted <- as_spark_dataframe(sc, pred,strict_version_check = F)


# fit a linear model to the training dataset
# fit <- h2o.glm(x = c("wt", "cyl"), 
#               y = "mpg", 
#               training_frame = training,
#               lambda_search = TRUE)

# print(fit)



library(ggplot2)



# extract the true 'mpg' values from our test dataset
actual <- partitions$test %>%
  select(mpg) %>%
  collect() %>%
  `[[`("mpg")

prediction<-select(predicted,prediction)%>%
    collect()

# produce a data.frame housing our predicted + actual 'mpg' values
data <- data.frame(
  predicted = prediction,
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





## Reading and Writing Files
temp_csv <- tempfile(fileext = ".csv")
temp_parquet <- tempfile(fileext = ".parquet")
temp_json <- tempfile(fileext = ".json")

iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)

spark_write_csv(iris_tbl, temp_csv)
iris_csv_tbl <- spark_read_csv(sc, "iris_csv", temp_csv)

spark_write_parquet(iris_tbl, temp_parquet)
iris_parquet_tbl <- spark_read_parquet(sc, "iris_parquet", temp_parquet)

spark_write_json(iris_tbl, temp_json)
iris_json_tbl <- spark_read_json(sc, "iris_json", temp_json)

src_tbls(sc)
