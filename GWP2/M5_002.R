# Comparing 3 asset classes during COVID
# Selecting a GOLD ETF, Equity EFT outside the USA and Bitcoin

install.packages("egcm")
install.packages("urca")
install.packages("tsDyn")
install.packages("data.table")

# load package 
library(quantmod) 
library(ggplot2)

# 1.0 import GLD ETF closing prices for 2020
getSymbols(Symbols = "GLD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
Gld_Closing_Prices <- GLD$GLD.Close
plot( x = index( GLD ), y = Gld_Closing_Prices )
plot(Gld_Closing_Prices, xlab = "Date", ylab = "Gold Closing Price")

# 1.0 import Equity ETF closing prices for 2020
# U.K Oil and Gas Investment PLC (UKOG.L)
getSymbols(Symbols = "UKOG.L", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
UKOG.L$UKOG.L.Close
Ukog_Closing_Prices <- UKOG.L$UKOG.L.Close
plot( x = index( UKOG.L ), y = Ukog_Closing_Prices )
plot(Ukog_Closing_Prices, xlab = "Date", ylab = "UKOG Closing Price")

# 1.0 import Bitcoin for 2020
getSymbols(Symbols = 'BTC-USD', from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
head(`BTC-USD`)
tail(`BTC-USD`)
BTC_closing_prices <- (`BTC-USD`)[,4]
BTC_closing_prices
plot(BTC_closing_prices, xlab = "Date", ylab = "BTC Closing Price")

# 1.1 import Gold ETF prices from the month of April-Dec 2020
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_GldPrices <- GLD$GLD.Close
plot(Aprl_Dec_GldPrices, xlab = "Date", ylab = "Gold Closing Price")

# 1.2 import UKOG.L Equity ETF from April-Dec 2020
getSymbols(Symbols = "UKOG.L", from = "2020-04-01", to = "2020-12-31" , src = "yahoo")
UKOG.L$UKOG.L.Close
Aprl_Dec_UkogPrices <- UKOG.L$UKOG.L.Close
plot(Aprl_Dec_UkogPrices, xlab = "Date", ylab = "UKOG Closing Price")

# 1.3 import Bitcoin closing prices from April-Dec 2020
getSymbols(Symbols = 'BTC-USD', from = "2020-04-01", to = "2020-12-31" , src = "yahoo")
head(`BTC-USD`)
tail(`BTC-USD`)
Aprl_Dec_BTCPrices <- (`BTC-USD`)[,4]
Aprl_Dec_BTCPrices
plot(Aprl_Dec_BTCPrices, xlab = "Date", ylab = "BTC Closing Price")

# 2.0 Data Processing and Daily returns computations
# 2.1 Gold ETF Daily return April - December 2020
Aprl_Dec_GldPrices <- GLD$GLD.Close
Gld_rt = diff( log( Aprl_Dec_GldPrices) )
Gld_rt
#Gldlog_rt = na.omit( Gld_rt)
Gldlog_rt = Gld_rt[-1]
Gldlog_rt
plot(Gldlog_rt, main="Gold Return")

# 2.1 UKOG Equity ETF Daily return April-December 2020
Aprl_Dec_UkogPrices <- UKOG.L$UKOG.L.Close
Ukog_rt = diff( log( Aprl_Dec_UkogPrices ) )
Ukog_rt
#Ukoglog_rt = na.omit( Ukog_rt)
Ukoglog_rt = Ukog_rt[-1]
Ukoglog_rt
plot(Ukoglog_rt, main="UKOG Return")

# 2.1 Bitcoin Daily return April - December 2020
Aprl_Dec_BTCPrices <- (`BTC-USD`)[,4]
btc_rt = diff( log( Aprl_Dec_BTCPrices ) )
btc_rt
#btclog return processing
btclog_rt = btc_rt[-1]
btclog2_rt = na.omit(btclog_rt)
btclog2_rt
plot(btclog2_rt, main="bitcoin Return")

# 3.0 Data Summaries | Compute a 20-day moving average 
# 3.1 To compute a 20-day moving average price for Gold ETF for April 2020 including data from March 
getSymbols(Symbols = "GLD", from = "2020-03-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_GldMA <- GLD$GLD.Close 
# 3.1 Compute Gold ETF 20-days Moving average (n=20)
Gld_MA <- SMA(Aprl_Dec_GldMA, n = 20)
plot(Gld_MA, col="red", main="Gold 20 days moving average")
summary(Gld_MA)

# 3.2 compute a 20-day moving average for UKOG Equity ETF for April-December 2020
getSymbols(Symbols = "UKOG.L", from = "2020-03-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_UkogMA <- UKOG.L$UKOG.L.Close
# 3.2 Compute UKOG Equity ETF 20-days Moving average (n=20) 
Ukog_MA <- SMA(Aprl_Dec_UkogMA, n=20)
plot(Ukog_MA, col="blue", main="UKOG 20 days moving average")
summary(Ukog_MA)

# 3.3 compute a 20-day moving average for Bitcoin for April - December 2020
getSymbols(Symbols = 'BTC-USD', from = "2020-03-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_BTCMA <- (`BTC-USD`)[,4]
# process 'na' values 
Aprl_Dec_BTCMAvg <- na.omit(Aprl_Dec_BTCMA)
# 3.3 Compute BTC 20-day (n=20) moving average for April - December 2020
BTC_MA <- SMA(Aprl_Dec_BTCMAvg, n=20)
plot(BTC_MA, col="green", main="bitcoin 20 days moving average")
summary(BTC_MA)

# 4.0 Graphing

# 4.1 Graph each price series along with its moving average from April 2020 through Dec 2020.
# Gold ETF with Moving average (n=5,20,60)
getSymbols(Symbols = "GLD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldMA <- GLD$GLD.Close
Gld_MA1 <- SMA(GldMA, n = 5)
lines( x = index( GLD ), y = Gld_MA1, type="l", xlab="Time", ylab="Closing price", col="green")
Gld_MA2 <- SMA(GldMA, n = 20)
lines( x = index( GLD ), y = Gld_MA2, type="l", xlab="Time", ylab="Closing price", col="red")
Gld_MA3 <- SMA(GldMA, n = 60)
lines( x = index( GLD ), y = Gld_MA3, type="l", xlab="Time", ylab="Closing price", col="purple")
legend("topleft",legend=c("GOLD ETF","MA 5 days","MA 20 days","MA 60 days"),
       text.col=c("black","green","red","purple"),col=c("black","green","red","purple"), lty=1, cex = 0.75)

# U.K Oil and Gas Investment PLC ETF with Moving average (n=5,20,60)
getSymbols(Symbols = "UKOG.L", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UkogMA <- UKOG.L$UKOG.L.Close
Ukog_MA1 <- SMA(UkogMA, n = 5)
lines( x = index( UKOG.L ), y = Ukog_MA1, type="l", xlab="Time", ylab="Closing price", col="green")
Ukog_MA2 <- SMA(UkogMA, n = 20)
lines( x = index( UKOG.L ), y = Ukog_MA2, type="l", xlab="Time", ylab="Closing price", col="red")
Ukog_MA3 <- SMA(UkogMA, n = 60)
lines( x = index( UKOG.L ), y = Ukog_MA3, type="l", xlab="Time", ylab="Closing price", col="purple")
legend("topright",legend=c("UKOG.L ETF","MA 5 days","MA 20 days","MA 60 days"),
       text.col=c("black","green","red","purple"),col=c("black","green","red","purple"), lty=1, cex = 0.75)

# bit coin with Moving average (n=5,20,60)
getSymbols(Symbols = "BTC-USD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitcMA <- `BTC-USD`$`BTC-USD.Close`
head(bitcMA)
bitcMA <- na.omit(bitcMA)
bitc_MA1 <- SMA(bitcMA, n = 5)
lines( x = index( bitcMA ), y = bitc_MA1, type="l", xlab="Time", ylab="Closing price", col="green")
bitc_MA2 <- SMA(bitcMA, n = 20)
lines( x = index( bitcMA ), y = bitc_MA2, type="l", xlab="Time", ylab="Closing price", col="red")
bitc_MA3 <- SMA(bitcMA, n = 60)
lines( x = index( bitcMA ), y = bitc_MA3, type="l", xlab="Time", ylab="Closing price", col="purple")
legend("topleft",legend=c("bitcoin","MA 5 days","MA 20 days","MA 60 days"),
       text.col=c("black","green","red","purple"),col=c("black","green","red","purple"), lty=1, cex = 0.75)


# 4.2 Comments on how the price series and average series intersect or not.

# intersection # for 5 days, 20 days, 60 days.
# gold :            so many,    ~20,     ~10 
# ukog :            so many,     ~9,     ~8
# bit coin :        so many,     ~9,     ~7
# it is meaningless to check the intersection of the 5 days moving average.
# we can see several intersections in the 20 and 60 days moving averages.

# 4.3 Graph gold and equity prices on the same plot.  Use a separate scale for each series, and be sure to add a label and legend
getSymbols(Symbols = "GLD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type = "l", col = "black", xlab = "Time", ylab = "GLD ETF price" )
par(mar=c(5, 4, 4, 6) + 0.1)
par(new=TRUE)
getSymbols(Symbols = "UKOG.L", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type = "l", col = "red", xlab = "", ylab = "", axes=FALSE)
mtext("UKOG.L ETF price",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
legend("topright",legend=c("GLD ETF","UKOG.L ETF"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 4.4 Graph gold and bitcoin prices on the same plot.  Use a separate scale for each series, and be sure to add a label and legend
getSymbols(Symbols = "GLD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type = "l", col = "black", xlab = "Time", ylab = "GLD ETF price" )
par(mar=c(5, 4, 4, 6) + 0.1)
par(new=TRUE)
getSymbols(Symbols = "BTC-USD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type = "l", col = "red", xlab = "", ylab = "", axes=FALSE)
mtext("bitcoin price",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
legend("topleft",legend=c("GLD ETF","bitcoin"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 4.5 Graph equity and bitcoin prices on the same plot.  Use a separate scale for each series, and be sure to add a label and legend
getSymbols(Symbols = "UKOG.L", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type = "l", col = "black", xlab = "Time", ylab = "UKOG.L ETF price" )
par(mar=c(5, 4, 4, 6) + 0.1)
par(new=TRUE)
getSymbols(Symbols = "BTC-USD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type = "l", col = "red", xlab = "", ylab = "", axes=FALSE)
mtext("bitcoin price",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
legend("top",legend=c("UKOG.L","bitcoin"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 5.1 Pick one of the 3 series (gold, equity, or bitcoin).  Fit a GARCH model for Q2.  Fit a GARCH model for Q3.  Fit a GARCH model for Q4
# pick gold Q2
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )
library(rugarch)
gld_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = 'norm')
gurcg_gldq2 <- ugarchfit(spec =gld_gurch, data = GldQ2)
print(gurcg_gldq2)
summary(gurcg_gldq2)

# pick gold Q3
getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ3 = GLD$GLD.Close
GldQ3 <- na.omit( GldQ3 )
library(rugarch)
gld_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = 'norm')
gurcg_gldq3 <- ugarchfit(spec =gld_gurch, data = GldQ3)
print(gurcg_gldq3)
summary(gurcg_gldq3)


# pick gold Q4
getSymbols(Symbols = "GLD", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ4 = GLD$GLD.Close
GldQ4 <- na.omit( GldQ4 )
library(rugarch)
gld_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = 'norm')
gurcg_gldq4 <- ugarchfit(spec =gld_gurch, data = GldQ4)
print(gurcg_gldq4)
summary(gurcg_gldq4)

# 5.2 Repeat the process, but instead using GARCH-M Model
# pick gold Q2
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq2 <- ugarchfit(spec =gld_gurch, data = GldQ2)
print(gurcg_gldq2)
summary(gurcg_gldq2)

# pick gold Q3
getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ3 = GLD$GLD.Close
GldQ3 <- na.omit( GldQ3 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq3 <- ugarchfit(spec =gld_gurch, data = GldQ3)
print(gurcg_gldq3)
summary(gurcg_gldq3)


# pick gold Q4
getSymbols(Symbols = "GLD", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ4 = GLD$GLD.Close
GldQ4 <- na.omit( GldQ4 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq4 <- ugarchfit(spec =gld_gurch, data = GldQ4)
print(gurcg_gldq4)
summary(gurcg_gldq4)

# 5.3 Repeat the process one more time, using another variation of GARCH (e.g. Threshold GARCH, Exponential GARCH, Integrated GARCH)

# pick gold Q2 threshold garch
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="TGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq2 <- ugarchfit(spec =gld_gurch, data = GldQ2)
print(gurcg_gldq2)
summary(gurcg_gldq2)

# pick gold Q3 threshold garch
getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ3 = GLD$GLD.Close
GldQ3 <- na.omit( GldQ3 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="TGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq3 <- ugarchfit(spec =gld_gurch, data = GldQ3)
print(gurcg_gldq3)
summary(gurcg_gldq3)


# pick gold Q4 threshold garch
getSymbols(Symbols = "GLD", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ4 = GLD$GLD.Close
GldQ4 <- na.omit( GldQ4 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="TGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq4 <- ugarchfit(spec =gld_gurch, data = GldQ4)
print(gurcg_gldq4)
summary(gurcg_gldq4)

# pick gold Q2 exponential garch
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="fGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq2 <- ugarchfit(spec =gld_gurch, data = GldQ2)
print(gurcg_gldq2)
summary(gurcg_gldq2)

# pick gold Q3 exponential garch
getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ3 = GLD$GLD.Close
GldQ3 <- na.omit( GldQ3 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="fGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq3 <- ugarchfit(spec =gld_gurch, data = GldQ3)
print(gurcg_gldq3)
summary(gurcg_gldq3)


# pick gold Q4 exponential garch
getSymbols(Symbols = "GLD", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ4 = GLD$GLD.Close
GldQ4 <- na.omit( GldQ4 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="fGARCH",
                      submodel="fGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq4 <- ugarchfit(spec =gld_gurch, data = GldQ4)
print(gurcg_gldq4)
summary(gurcg_gldq4)

# pick gold Q2  Integrated garch
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="iGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq2 <- ugarchfit(spec =gld_gurch, data = GldQ2)
print(gurcg_gldq2)
summary(gurcg_gldq2)

# pick gold Q3  Integrated garch
getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ3 = GLD$GLD.Close
GldQ3 <- na.omit( GldQ3 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="iGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq3 <- ugarchfit(spec =gld_gurch, data = GldQ3)
print(gurcg_gldq3)
summary(gurcg_gldq3)


# pick gold Q4  Integrated garch
getSymbols(Symbols = "GLD", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ4 = GLD$GLD.Close
GldQ4 <- na.omit( GldQ4 )
library(rugarch)
gld_gurch <- ugarchspec(
  variance.model=list(model="iGARCH"),
  mean.model=list(armaOrder=c(0,0) ), 
  distribution.model="std" )
gurcg_gldq4 <- ugarchfit(spec =gld_gurch, data = GldQ4)
print(gurcg_gldq4)
summary(gurcg_gldq4)

# 5.4 What are the 3 long-term variances indicated by the models.
# The results and interpretation for the 3 long-term variances indicated by the models are provided in the technical report 
# section 5.4 with the table indicating the different results.

# 6.1 Pick 1 of the 3 quarters in 2020 (Q2, Q3, or Q4).  Test your gold ETF for stationarity.  Explicitly state which test you used to determine stationarity
# pick Q2
# acf pacf test
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )

acf( GldQ2 )
pacf( GldQ2 )

# Ljung-Box test
lag.length = 25
Box.test( GldQ2, lag=lag.length, type="Ljung-Box")

# Augmented Dickey–Fuller (ADF) t-statistic test for unit root
library(tseries)
adf.test( GldQ2 )


# 6.2 Using the same quarter, test your equity ETF for stationarity.  Explicitly state which test you used to determine stationarity
# acf pacf test
getSymbols(Symbols = "UKOG.L", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UkogQ2 = UKOG.L$UKOG.L.Close
UkogQ2 <- na.omit( UkogQ2 )

acf( UkogQ2 )
pacf( UkogQ2 )

# Ljung-Box test
lag.length = 25
Box.test( UkogQ2, lag=lag.length, type="Ljung-Box")

# Augmented Dickey–Fuller (ADF) t-statistic test for unit root
library(tseries)
adf.test( UkogQ2 )

# 6.3 Using the same quarter, test Bitcoin for stationarity.  Explicitly state which test you used to determine stationarity
# acf pacf test
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ2 = `BTC-USD`$`BTC-USD.Close`
bitQ2 <- na.omit( bitQ2 )

acf( bitQ2 , lag.max = length( bitQ2 ) )
pacf( bitQ2 )

# Ljung-Box test
lag.length = 25
Box.test( bitQ2, lag=lag.length, type="Ljung-Box")

# Augmented Dickey–Fuller (ADF) t-statistic test for unit root
library(tseries)
adf.test( bitQ2 )

# 7.0 Modeling Cointegration 
# 7.1 Using Q2 data: Test all the 3 combinations of cointegration Engle-Granger
# Q2 of Gold ETF
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ2 = GLD$GLD.Close
GldQ2 <- na.omit( GldQ2 )

# Q2 of UKOG Equity ETF
getSymbols(Symbols = "UKOG.L", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UkogQ2 = UKOG.L$UKOG.L.Close
UkogQ2 <- na.omit( UkogQ2 )

# Q2 Bitcoin 
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ2 = `BTC-USD`$`BTC-USD.Close`
bitQ2 <- na.omit( bitQ2 )

#  7.2 Cointegration analysis of all 3 combinations using Engle-Granger | Checking if there any cointegrating vectors 

library(egcm)
library(urca)
library(tsDyn)

# checking if series are stationary and if found to be stationary,
# series can be defined to be cointegrated which implies that there exist some long-run relationship
# between the variable, and then we can estimate VECM. Then we can do innovation accounting (variance decomposition & 
# impulse response function)

# checking for Gold ETF
GldQ2_cointegration <- ur.df(GldQ2, type = "none", selectlags = "AIC")
summary(GldQ2_cointegration)
GldQ2_cointegration@teststat
GldQ2_cointegration@cval

# checking for UKOG Equity ETF
UkogQ2_cointegration <- ur.df(UkogQ2, type = "none", selectlags = "AIC")
summary(UkogQ2_cointegration)
UkogQ2_cointegration@teststat
UkogQ2_cointegration@cval

# checking for Bitcoin
BTCQ2_cointegration <- ur.df(bitQ2, type = "none", selectlags = "AIC")
summary(BTCQ2_cointegration)
BTCQ2_cointegration@teststat
BTCQ2_cointegration@cval

# Interpretation 
# The critical value of Gold ETF Q2 at 5% is -1.95 while the statistic value is 1.244037.
# The critical value of UKOG Equity Q2 at 5% is -1.95 while the statistic value is -1.011524.
# The critical value of Bitcoin Q2 at 5% is -1.95 while the statistic value is 0.7432714.
# Since the critical values at 5% are less than the statistics, we fail to reject the null hypothesis.
# Thus, no co-integrating vectors are obsereved in Q2 series, thus we perform further cointegration anlysis below:

# 7.3 Now re-run the cointegration analysis of all 3 combinations for Q2 using Engle-Grange
Q2_Gld_Ukog_cointegrate <- yegcm("GLD", "UKOG.L", start ="2020-04-01", end = "2020-06-30")
plot(Q2_Gld_Ukog_cointegrate)

Q2_Gld_BTC_cointegrate <- yegcm("GLD", "BTC-USD", start ="2020-04-01", end = "2020-06-30")
`BTC-USD`<- na.omit( `BTC-USD` ) # processing BTC-USD 
plot(Q2_Gld_BTC_cointegrate )

Q2_Ukog_BTC_cointegrate <- yegcm("UKOG.L", "BTC-USD", start ="2020-04-01", end = "2020-06-30")
`BTC-USD`<- na.omit( `BTC-USD` ) # processing BTC-USD 
plot(Q2_Ukog_BTC_cointegrate)

conintegration_model = cbind(GldQ2, UkogQ2, bitQ2)


# 7.4 Using Q3 data:Test all the 3 combinations of cointegration Engle-Granger
# Q3 of Gold ETF 
getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( GLD ), y = GLD$GLD.Close, type="l", xlab="Time", ylab="Closing price ($)", main="GOLD ETF price")
GldQ3 = GLD$GLD.Close
GldQ3 <- na.omit( GldQ3 )

# Q3 UKOG Equity ETF 
getSymbols(Symbols = "UKOG.L", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UkogQ3 = UKOG.L$UKOG.L.Close
UkogQ3 <- na.omit( UkogQ3 )

# Q3 Bitcoin 
getSymbols(Symbols = "BTC-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ3 = `BTC-USD`$`BTC-USD.Close`
bitQ3 <- na.omit( bitQ3 )

# checking if series are stationary and if found to be stationary,
# series can be defined to be cointegrated which implies that there exist some long-run relationship
# between the variable, and then we can estimate VECM. Then we can do innovation accounting (variance decomposition & 
# impulse response function)

# checking for Q3 Gold ETF
GldQ3_cointegration <- ur.df(GldQ3, type = "none", selectlags = "AIC")
summary(GldQ3_cointegration)
GldQ3_cointegration@teststat
GldQ3_cointegration@cval

# checking for Q3 UKOG Equity ETF
UkogQ3_cointegration <- ur.df(UkogQ3, type = "none", selectlags = "AIC")
summary(UkogQ3_cointegration)
UkogQ3_cointegration@teststat
UkogQ3_cointegration@cval

# checking for Q3 Bitcoin
BTCQ3_cointegration <- ur.df(bitQ3, type = "none", selectlags = "AIC")
summary(BTCQ3_cointegration)
BTCQ3_cointegration@teststat
BTCQ3_cointegration@cval

# Interpretation
# The critical value of Gold ETF Q3 at 5% is -1.95 while the statistic value is 0.5003117.
# The critical value of UKOG Equity Q3 at 5% is -1.95 while the statistic value is -0.7579268.
# The critical value of Bitcoin Q3 at 5% is -1.95 while the statistic value is 0.5853054.
# Since the critical values at 5% are less than the statistics, we fail to reject the null hypothesis.
# Thus, no co-integrating vectors are obsereved in Q3 series and perform further cointegration analysis using Engle_Granger below:

# 7.4 Now re-run the cointegration analysis of all 3 combinations for Q3 using Engle-Grange
Q3_Gld_Ukog_cointegrate <- yegcm("GLD", "UKOG.L", start ="2020-07-01", end = "2020-09-30")
plot(Q3_Gld_Ukog_cointegrate)

Q3_Gld_BTC_cointegrate <- yegcm("GLD", "BTC-USD", start ="2020-07-01", end = "2020-09-30")
plot(Q3_Gld_BTC_cointegrate )

Q3_Ukog_BTC_cointegrate <- yegcm("UKOG.L", "BTC-USD", start ="2020-07-01", end = "2020-09-30")
plot(Q3_Ukog_BTC_cointegrate)

# 7.5 Thus, there is no cointegration vectors observed and we will perform Johansen test for cointegration 

# 7.7 Johansen test for cointegration 
getSymbols(Symbols = "GLD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
getSymbols(Symbols = "UKOG.L", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")

cb = cbind( GLD$GLD.Close, UKOG.L$UKOG.L.Close, `BTC-USD`$`BTC-USD.Close` )
cb = na.omit( cb )
cb
jotest=ca.jo( cb, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

getSymbols(Symbols = "GLD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
getSymbols(Symbols = "UKOG.L", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
getSymbols(Symbols = "BTC-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")

cb = cbind( GLD$GLD.Close, UKOG.L$UKOG.L.Close, `BTC-USD`$`BTC-USD.Close` )
cb = na.omit( cb )
cb
jotest=ca.jo( cb, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

