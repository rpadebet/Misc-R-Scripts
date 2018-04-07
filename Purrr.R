library(purrr)

data("mtcars")

require(stats); require(graphics)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
xg
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)


mtcars%>%
    split(.$cyl)%>%
    map(~ lm(mpg ~ wt, data = .))%>%
    map(summary) %>%
    map_dbl("r.squared")

require(tidyverse)
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

df[[2]]
df[,[1]]

seq_along(df)
?seq_along

shapiro.test(iris$Sepal.Length[1:50])
