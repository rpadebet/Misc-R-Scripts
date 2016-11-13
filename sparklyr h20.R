library(sparklyr)
library(rsparkling)
library(h2o)
library(dplyr)

## connect to spark - check if needed - two instances installed
# Sys.setenv(SPARK_HOME = "/usr/local/src/Spark")
# Sys.setenv(SPARK_HOME = "/home/rohit/.cache/spark")
library(devtools)
devtools::install_github("h2oai/sparkling-water", ref = "master", subdir = "/r/rsparkling",force=TRUE)
# Sys.getenv("SPARK_HOME")

# spark connection
sc <- spark_connect(master = "local")

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

training <- as_h2o_frame(sc,partitions$training)
test <- as_h2o_frame(sc,partitions$test)

# fit a linear model to the training dataset
fit <- h2o.glm(x = c("wt", "cyl"), 
               y = "mpg", 
               training_frame = training,
               lambda_search = TRUE)

print(fit)