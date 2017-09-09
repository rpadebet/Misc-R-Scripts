library(RPostgreSQL)

drv<- dbDriver("PostgreSQL")
conn<-dbConnect(drv,dbname="spam",host="rohit-lubuntu",port=5432,user="rohit",password="rohit123")

data("iris")
dbWriteTable(conn,'iris',iris, row.names=FALSE)
dbListTables(conn)

dtab = dbGetQuery(conn, "select * from airports")
summary(dtab)
glimpse(dtab)


library(quantmod)

class(getSymbols('MSFT',src='yahoo'))

str(MSFT$MSFT)
