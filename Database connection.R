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

library(RMySQL)

drv2<- dbDriver("MySQL")
conn2<-dbConnect(drv2,
                 dbname="awsMySQLdb",
                 host="myawsdb.cmftapsh8tgl.us-east-1.rds.amazonaws.com",
                 port=3306,
                 user="rohit_aws",
                 password="rohit123")



dbListTables(conn2)

MSFTfile<-dbGetQuery(conn2,"Select * from MSFT")

library(readr)

write_csv(MSFTfile,"MSFT.csv")

list.files()

roth_prices<-read.csv("~/R Projects/Prices.csv",header=T)
str(roth_prices)
dbWriteTable(conn2,"RothPrices",roth_prices)

library(tidyquant)

x <- tidyquant::tq_get(x='IBM')


