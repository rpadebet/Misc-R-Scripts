library(sparklyr)
library(rsparkling)
library(h2o)
library(dplyr)

# connect to spark
sc <- spark_connect("local", version = "1.6.2")

# copy mtcars dataset into spark
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

training <- as_h2o_frame(partitions$training)
test <- as_h2o_frame(partitions$test)

# fit a linear model to the training dataset
fit <- h2o.glm(x = c("wt", "cyl"), 
               y = "mpg", 
               training_frame = training,
               lambda_search = TRUE)

print(fit)