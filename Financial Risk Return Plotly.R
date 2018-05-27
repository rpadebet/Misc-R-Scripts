library(plotly)
library(quantmod)
library(zoo)
library(dplyr)
library(reshape2)
library(PerformanceAnalytics)

stocklist = c("AAPL","GOOGL","MSFT","BRK-A","AMZN","FB","JNJ","XOM","JPM","WFC","BABA",
              "T","BAC","GE","PG","CHL","BUD","RDS-A","WMT","V","VZ","PFE","CVX","ORCL",
              "KO","HD","NVS","CMCSA","DIS","MRK","PM","CSCO","TSM","C","INTC","IBM","UNH",
              "HSBC","PEP","MO","UL","CX","AMGN","MA","CCV","TOT","BTI","SAP","MMM","MDT")

ddf <- getSymbols(Symbols = stocklist[1], auto.assign = F)
ddf <- ddf[,6]

pb <- txtProgressBar(min = 0, max = length(stocklist) - 1, style=3)

for(i in stocklist[-1]){
  df <- getSymbols(Symbols = i, auto.assign = F)
  df <- df[,6]

  ddf <- merge(ddf, df)

  setTxtProgressBar(pb, which(stocklist[-1] == i))
}

month <- as.yearmon(index(ddf))
prices <- data.frame(ddf, month)
names(prices) <- c(stocklist, "Month")
prices <- melt(prices, id.vars = "Month")

# Calculate returns
CalcRet <- function(x, vec = F){
  ret <- (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]

  if(vec == T) {
    return(ret)
  }else{
    return(mean(ret))
  }
}

returns <- prices %>%
  group_by(Month, variable) %>%
  summarize(Return = CalcRet(value))

returns <- data.frame(returns, VAR = "Returns")
names(returns) <- c("Period", "Stock", "Value", "Variable")

# Calculate volatility
volatility <- prices %>%
  group_by(Month, variable) %>%
  summarize(Volatility = sd(CalcRet(value, vec = T)))

volatility <- data.frame(volatility, VAR = "Volatility")
names(volatility) <- c("Period", "Stock", "Value", "Variable")

# Create df for plotting
plot.df <- rbind(returns, volatility)
plot.df <- dcast(plot.df, Period + Stock ~ Variable, value.var = "Value")
plot.df$Year <- format(plot.df[,1], "%Y")

plot.tbl<-as.tbl(plot.df)

p <- plot_ly(plot.tbl, x = ~Volatility, y = ~Returns,color = ~Stock) %>%
  add_markers(color = ~Stock, size = ~(Returns / Volatility),
              frame = ~Year,
              marker = list(opacity = 0.6, line = list(width = 1, color = "black"))) %>%
  layout(title = "Monthly Return vs Volatility over last 10 years <br> for 50 US stocks over time",
         showlegend = F,
         plot_bgcolor = "#e6e6e6",
         paper_bgcolor = "#e6e6e6",
         xaxis= list(title= "Monthly Volatility", family = "Old Standard TT, serif", color="blue" )) %>%
    animation_opts(frame = 1000)
p


