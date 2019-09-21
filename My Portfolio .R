#***** My Stocks ****

#Import Packages
library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
library(knitr, warn.conflicts = FALSE, quietly = TRUE)
library(raster)

getData<-function(tickers,datasrc){
  for (i in 1:length(tickers)){
    cat(tickers[i],i,"\n")
    getSymbols(tickers[i],src=datasrc,
               auto.assign=getOption("getSymbols.auto.assign",TRUE),
               env=parent.frame())
  }
}

makeIndex<-function(x,inv,ret){
  # Takes an xts object x and returns an index starting at 100 and evolving as the log returns of x.
  # The inv flag tells whether or not to invert the series before calculating returns.
  # The ret flag tells whether or not we have been passed a series of returns already.
  init.val<-100
  dts<-index(x,0)
  if (inv==TRUE) data<-1/x else data<-x
  if (ret==TRUE){ # we have a series of returns...
    ret.series<-x
  } else {
    ret.series<-periodReturn(data,period="daily",subset=NULL,type="log")
    dts<-index(ret.series,0)
  }
  n<-length(ret.series)
  new.series<-ret.series
  new.series[1]<-init.val
  
  for (i in 2:n){
    new.series[i]<-(1+ret.series[i-1])*new.series[i-1]
  }
  names(new.series)<-c("index")
  return(new.series)
} # My custom index funtion for converting indices to 100 based at inception.

calcWeights<-function(prices,numshares,initial){
  ret<-NULL
  for (i in 1:length(numshares)){
    sh<-numshares[i]
    ret<-cbind(ret,sh*prices[,i]/initial)
  }
  return(ret)
}

getOHLC<-function(assets,OHLC){
  # Takes a list of assets and returns either the Open, High, Low, or Close depending
  # on the passed value of HLOC. Return value is of type xts/zoo.
  ret<-NULL
  for (i in 1:length(assets)){
    if (OHLC=="O" || OHLC=="Open"){
      ret<-cbind(ret,assets[[i]][,1])
    } else {
      if (OHLC=="H" || OHLC=="High"){
        ret<-cbind(ret,assets[[i]][,2])
      } else {
        if (OHLC=="L" || OHLC=="Low"){
          ret<-cbind(ret,assets[[i]][,3])
        } else {
          if (OHLC=="C" || OHLC=="Close"){
            ret<-cbind(ret,assets[[i]][,4])
          }
        }
      }
    }
  }
  return(ret)
}

# Set up the data source and the tickers for all the ETFs...
data.source = c("yahoo")
tickers.etf = c("EEM", "XLV", "PG")
tickers.human = c("Real Est", "USTSY Long", "For Eq", "USTSY Short", "All Eq", "USTSY Int.", "EM Eq", "Infl Sec.")
tickers.bench = c("DJIA", "^GSPC")
tsy.2y <- c("DGS2")

suppressWarnings(getData(tickers.etf, data.source))
suppressWarnings(getData(tickers.bench, data.source))
suppressWarnings(getData(tsy.2y, datasrc = "FRED"))

MyPort <- list(EEM, XLV, PG)
first.date <- c("2016-03-01/")  # inception data for portfolio...

# Get all the closing prices into a vector starting from the inception date...
Port_close <- getOHLC(MyPort, "C")
Port_close <- Port_close[first.date]
print(Port_close)

# Specify the initial number of shares that were purchased and the initial investment...
shares <- c(20, 10, 8)
init.invest <- 1936.086

mtm_last <- sum(last(Port_close) * shares)
mtm_last

port_value <- as.xts(apply(Port_close, 1, FUN = function(x) sum(x * shares)))
plot.xts(port_value, las = 1)  # A line chart of the portfolio value in dollars...

#benchmark 

getSymbols("VTIAX")
print(VTIAX)
getSymbols("VHCIX")
print(VHCIX)
getSymbols("SPY")
print(SPY)

benchPort <- list(VTIAX, VHCIX, SPY)
BenchPortclose <- getOHLC(benchPort, "C")
print(BenchPortclose)

HCbench <- periodReturn(VHCIX[first.date][, 4], period = "daily", subset = NULL, type = "log")
USbench <- periodReturn(SPY[first.date][, 4], period = "daily", subset = NULL, type = "log")
EMbench <- periodReturn(VTIAX[first.date][, 4], period = "daily", subset = NULL, type = "log")

bench_ret <- 0.344 * HCbench + 0.335 * USbench + .321 * EMbench

port_ret <- periodReturn(port_value, period = "daily", subset = NULL, type = "log")  # Ret. stream for our portfolio...
port_index <- makeIndex(port_ret, inv = FALSE, ret = TRUE)  # Our portfolio indexed to 100 at inception...
bench_index <- makeIndex(bench_ret, inv = FALSE, ret = TRUE)  # Benchmark portfolio indexed to 100 at our inception date...

# Setting up some variables to mach the PerformanceAnalytics lexicon...
Ra <- port_ret
Rb <- bench_ret
dts <- index(Ra, 0)
Rb <- xts(Rb, dts)
Rf <- as.numeric(last(DGS2)/100/252)

chart.RelativePerformance(Ra, as.vector(Rb), main = "Relative Performace vs. Benchmark", xaxis = TRUE)

#how much portfolio outperforms benchmark on annualized basis 
act.premium <- ActivePremium(Ra, Rb, scale = 252)
act.premium

# The initial weights of the portfolio:
weights_init <- (first(Port_close) * shares)/init.invest
weights_init

# The weights of the portfolio now:
weights_last <- (last(Port_close) * shares)/init.invest
weights_last

# Change in weights since inception:
weights_chg <- as.vector(weights_last) - as.vector(weights_init)
weights_chg


