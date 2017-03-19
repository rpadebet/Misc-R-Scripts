library(RPostgreSQL)

drv<- dbDriver("PostgreSQL")
conn<-dbConnect(drv,dbname="spam",host="localhost",port=5432,user="rohit",password="123")

data("iris")
dbWriteTable(conn,'iris',iris, row.names=FALSE)
dbListTables(conn)

dtab = dbGetQuery(conn, "select * from iris")
summary(dtab)

library(quantmod)

class(getSymbols('MSFT',src='yahoo'))

str(MSFT$MSFT)

JPM<-get(getSymbols('JPM',src='yahoo'))

JPM.data<-as.data.frame(JPM)
head(JPM.data)
str(JPM.data)



nrow(JPM.data)
JPM.data$Date<-index(JPM)
JPM.data$Ticker<-rep("JPM",times=nrow(JPM.data))
colnames(JPM.data)<-c("OPEN","HIGH","LOW","CLOSE","VOLUME","ADJUSTED.CLOSE","DATE","TICKER")

dbWriteTable(conn,'Stocks',JPM.data, row.names=FALSE)

Stkpx = dbGetQuery(conn, " SELECT * FROM \"Stocks\" where \"Date\">'01-01-2017'")
head(Stkpx)


MSFT<-get(getSymbols('MSFT',src='yahoo'))
MSFT.data<-as.data.frame(MSFT)
MSFT.data$Date<-index(MSFT)
MSFT.data$Ticker<-rep("MSFT",times=nrow(MSFT.data))
colnames(MSFT.data)<-c("OPEN","HIGH","LOW","CLOSE","VOLUME","ADJUSTED.CLOSE","DATE","TICKER")

dbWriteTable(conn,'Stocks',MSFT.data, row.names=FALSE,append=TRUE)
## Doesnt work because table already exists
