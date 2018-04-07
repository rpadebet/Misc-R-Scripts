my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
                 "gapminder", "GGally", "ggrepel", "ggridges","gridExtra",
                 "interplot", "margins", "maps", "mapproj", "mapdata",
                 "MASS", "quantreg", "scales", "survey", "srvyr",
                 "viridis", "viridisLite", "devtools")

install.packages(my_packages,
                 repos = "http://cran.rstudio.com")

devtools::install_github("kjhealy/socviz")

library(gapminder)
library(ggplot2)
head(gapminder)
p <- ggplot(data=gapminder,mapping = aes(x=gdpPercap,y=lifeExp))
p <- p + geom_point()
p


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y=lifeExp))
p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") + scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita",
       y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")+
  theme(plot.background = element_rect(fill ="gray60"),
        panel.background = element_rect(fill = "sienna1")  )

library(socviz)

p <- ggplot(data = gss_sm,
            mapping = aes(x = religion, fill = bigregion))
p + geom_bar(position = "dodge",
             mapping = aes(y = ..count.., group = bigregion)) 


p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                          y = donors, fill = world))
p + geom_boxplot() + 
  labs(x=NULL) +
  coord_flip() + theme(legend.position = "top")


p <- ggplot(data = by_country,
            mapping = aes(x = don.rate,
                          y = reorder(country, don.rate)))

p + geom_point(size=3) +
  facet_wrap(~ consent.law, scales = "free_y", ncol=1) +
  labs(x="Donor Procurement Rate",
       y="")


p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))
p + geom_point(size = 2) + scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")
p + geom_point(size = 2) + scale_color_brewer(palette = "Pastel2") +
  theme(legend.position = "top")
p + geom_point(size = 2) + scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "top")
p + geom_point(size = 2) + scale_color_brewer(palette = "Accent") +
  theme(legend.position = "top")

cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")




head(asasec)
p <- ggplot(subset(asasec, Year == 2014), mapping = aes(x = Members, y = Revenues, label = Sname))
p + geom_smooth(method = "lm", se = FALSE, color = "gray80") +
  geom_point(mapping = aes(color = Journal)) +
  geom_text_repel(data=subset(asasec,
                              Year == 2014 & Revenues > 7000),size=2)+
  labs(x="Membership",
       y="Revenues",
       color = "Section has own Journal",
       title = "ASA Sections",
       subtitle = "2014 Calendar year.",
       caption = "Source: ASA annual report.")+
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "bottom")+
  scale_color_manual(values = cb.palette) 



head(gss_lon)
yrs <- c(seq(1972, 1988, 4), 1993, seq(1996, 2016, 4))
library(dplyr)
mean_age <- gss_lon %>%
  filter(age %nin% NA && year %in% yrs) %>%
  group_by(year) %>%
  summarize(xbar = round(mean(age, na.rm = TRUE), 0))
mean_age$y <- 0.3

yr_labs <- data.frame(x = 85, y = 0.8,
                      year = yrs)

p <- ggplot(subset(gss_lon, year %in% yrs), aes(x = age))

p1 <- p + geom_density(fill = "gray20", color = FALSE,
                       alpha = 0.9, mapping = aes(y = ..scaled..)) +
  geom_vline(data = subset(mean_age, year %in% yrs),
             aes(xintercept = xbar), color = "white", size = 0.5) +
  geom_text(data = subset(mean_age, year %in% yrs),
            aes(x = xbar, y = y, label = xbar), nudge_x = 7.5,
            color = "white", size = 3.5, hjust = 1) +
  geom_text(data = subset(yr_labs, year %in% yrs),
            aes(x = x, y = y, label = year)) +
  facet_grid(year ~ .,switch = 'y')
p1

library(ggthemes)
p1 + theme_economist_white()+
  theme(plot.title = element_text(size = 16),
        axis.text.x= element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Age",
       y = NULL,
       title = "Age Distribution of\nGSS Respondents")



### Time series charts
library(tidyr)

head(fredts)
fredts_m <- fredts %>% select(date, sp500_i, monbase_i) %>%
  gather(key = series, value = score, sp500_i:monbase_i)

p <- ggplot(data = fredts_m,
            mapping = aes(x = date, y = score,
                          group = series,
                          color = series))
p1 <- p + geom_line() + theme(legend.position = "top") + theme_wsj()+
  labs(x = "Date",
       y = "Index",
       color = "Series")

p <- ggplot(data = fredts,
            mapping = aes(x = date, y = sp500_i - monbase_i))

p2 <- p + geom_line() +
  labs(x = "Date",
       y = "Difference")+
  theme_minimal()

cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(0.75, 0.25), align = "v")

data(iris)
g<-ggplot(data = iris,mapping= aes(x=Petal.Length,y=Petal.Width))
g <-g + 
  geom_point(aes(color=Species)) +
  scale_color_manual("Flower Species",values = color_used, labels = c("Setosa","Versicolor","Virginica"))+
  labs(x = "Petal Length",
       y = "Petal Width",
       title = " Petal Length-Width")+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom",
        plot.title = element_text(family = "serif",face = "bold",size = 14,hjust = 0.5 ))
g
ggplotly(p1)


library(RColorBrewer)
display.brewer.all()
brewer.pal(3,"Dark2")
color_used<-colorRampPalette(brewer.pal(4,"Dark2"))(3)

library(plotly)
