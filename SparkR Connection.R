library(SparkR, lib.loc = "/home/rstudio/spark-2.0.2-bin-hadoop2.7/R/lib")
sparkR.session(master = "spark://172.31.23.156:7077", 
               sparkHome = "/home/rstudio/spark-2.0.2-bin-hadoop2.7/", enableHiveSupport=FALSE)

ds <- createDataFrame(mtcars)
s_glm <- SparkR::glm(mpg~disp, ds, family="gaussian")
print(summary(s_glm))

head(ds)

str(ds)

sparkR.stop()
