library(RPostgreSQL)

drv<- dbDriver("PostgreSQL")
conn<-dbConnect(drv,dbname="spam",host="rohit-lubuntu",port=5432,user="rohit",password="123")

data("iris")
dbWriteTable(conn,'iris',iris, row.names=FALSE)
dbListTables(conn)

dtab = dbGetQuery(conn, "select * from iris")
summary(dtab)

library(quantmod)

class(getSymbols('MSFT',src='yahoo'))

str(MSFT$MSFT)
