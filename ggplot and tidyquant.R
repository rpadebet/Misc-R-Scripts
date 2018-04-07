con <- url("http://cran.r-project.org/src/contrib/PACKAGES")
y <- read.dcf(con, all = TRUE)
close(con)

library(quantmod)
library(tidyquant)

getSymbols('CPIAUCNS',src='FRED')
par(bg='white')
plot(CPIAUCNS,col='blue')

library(ggplot2)

CPI <- tq_get("CPIAUCNS","economic.data")
CPI%>%
    ggplot(aes(x=date,y=price))+
    geom_line(aes(colour='CPI'))+
    geom_ma( aes(colour = 'CPI EMA'),linetype = 4,ma_fun = EMA,n=30)+
    labs(title = "CPI Levels")+
    theme_bw()+
    scale_color_brewer(palette="Dark2",type = "div",
                        name="FRED CPI Data",
                       breaks = c("CPI EMA","CPI"),
                      # values = c("blue","green"),
                       labels=c( "Exp MA CPI","CPI"))+
    theme(legend.position = c(0.2,0.85),
          legend.text = element_text(family="Courier",face="bold"),
          plot.background = element_rect(fill="snow2"),
          axis.text = element_text(family = "Times",colour = "red",face = "bold"),
          panel.background = element_rect(fill = "gray9"),
          legend.box.background = element_rect(color="black",
                                               linetype = "dotted"),
          legend.background = element_rect(fill="white"))

    


df <- read.table(header=TRUE, text='
 cond yval
                 A 2
                 B 2.5
                 C 1.6
                 ')

# Three variables
df2 <- read.table(header=TRUE, text='
                  cond1 cond2 yval
                  A      I 2
                  A      J 2.5
                  A      K 1.6
                  B      I 2.2
                  B      J 2.4
                  B      K 1.2
                  C      I 1.7
                  C      J 2.3
                  C      K 1.9
                  ')
ggplot(df, aes(x=cond, y=yval, fill=cond)) + geom_bar(stat="identity") +
    scale_fill_hue(l=50,h = c(1,100),c=30)



library(tidyquant)
Ra <- c("FB", "TSLA", "IBKR","JPM","AMNF","MKL","DB","LILA","PYPL","NVO","FAST") %>%
    tq_get(get  = "stock.prices",
           from = "2015-08-01",
           to   = "2017-08-21") %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "Ra")
Ra

Rb <- c("SPY") %>%
    tq_get(get  = "stock.prices",
           from = "2015-08-01",
           to   = "2017-08-21") %>%
    #group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "Rb")
Rb

RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
RaRb

RaRb_capm <- RaRb %>%
    tq_performance(Ra = Ra, 
                   Rb = Rb, 
                   performance_fun=table.CAPM)

select(RaRb_capm,Alpha,Beta,`R-squared`,TrackingError,Correlation,`Correlationp-value`)

tq_performance_fun_options()




data(edhec)
head(edhec)
data(weights)
head(weights)
str(weights)

edhec_long<-tk_tbl(edhec)%>%
    gather(key = "Funds",value="Returns",-index)%>%
    mutate(Funds = as.factor(Funds))

weights_long<-tk_tbl(weights)%>%
    gather(key = "Funds",value ="Weights",-index)%>%
    mutate(Funds = as.factor(Funds))

portfolio_returns <- edhec_long%>%
    tq_portfolio(assets_col = Funds,
                 returns_col  = Returns, 
                 weights      = weights_long, 
                 rebalance_on = "months",
                 col_rename   = "investment.growth",
                 wealth.index = TRUE) 
%>%
    mutate(investment.growth = investment.growth * 10000)
                 
    





