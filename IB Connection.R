
library(IBrokers)
tws <- twsConnect(port=4001) #IB Gateway
tws<-twsConnect()
tws
reqCurrentTime(tws)
serverVersion(tws)

c<-twsEquity("IBKR")
IBKR<-reqContractDetails(tws, c)

IBKR[[1]]$tradingHours
IBKR[[1]]$contract$primary
IBKR[[1]]$orderTypes[]
which(IBKR[[1]]$orderTypes=="MOC")
IBKR[[1]]$orderTypes[19]

PV<-twsPortfolioValue(x = reqAccountUpdates(tws))
PV_simple<-PV[PV$sectype=="STK",c("local","marketValue")]
TMV<-sum(PV_simple$marketValue)

reqFundamentalData(tws,reqId = reqIds(tws) ,contract=c,reportType = "ReportsFinSummary" )

twsDisconnect(tws)


reqFundamentalData <- function(twsconn, reqId, contract, reportType) {
    if( !is.twsConnection(twsconn))
        stop('invalid twsConnection')
    if( !is.twsContract(contract))
        stop('invalid twsContract')
    
    VERSION <- "1"
    
    msg <- c( .twsOutgoingMSG$REQ_FUNDAMENTAL_DATA,
              VERSION,
              reqId,
              
              # contract fields
              contract$symbol,
              contract$sectype,
              contract$exch,
              contract$primary,
              contract$currency,
              contract$local,
              
              reportType)
    
    writeBin( as.character(msg), twsconn[[1]])
}