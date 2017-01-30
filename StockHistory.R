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

BRK<-get(getSymbols('BRK-B',src='yahoo'))

BRK.data<-as.data.frame(BRK)
head(BRK.data)
str(BRK.data)

BRK.data$Date<-index(BRK)

dbWriteTable(conn,"StockHistory",BRK.data, row.names=FALSE)
stkpx = dbGetQuery(conn, " Select * from \"StockHistory\" where \"Date\">'01-01-2017'")

