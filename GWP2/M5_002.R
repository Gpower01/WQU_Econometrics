# Comparing 3 asset classes during COVID
# Selecting a GOLD ETF, Equity EFT outside the USA and Bitcoin

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
plot(Gldlog_rt)

# 2.1 UKOG Equity ETF Daily return April-December 2020
Aprl_Dec_UkogPrices <- UKOG.L$UKOG.L.Close
Ukog_rt = diff( log( Aprl_Dec_UkogPrices ) )
Ukog_rt
#Ukoglog_rt = na.omit( Ukog_rt)
Ukoglog_rt = Ukog_rt[-1]
Ukoglog_rt
plot(Ukoglog_rt)

# 2.1 Bitcoin Daily return April - December 2020
Aprl_Dec_BTCPrices <- (`BTC-USD`)[,4]
btc_rt = diff( log( Aprl_Dec_BTCPrices ) )
btc_rt
#btclog return processing
btclog_rt = btc_rt[-1]
btclog2_rt = na.omit(btclog_rt)
btclog2_rt
plot(btclog2_rt)

# 3.0 Data Summaries | Compute a 20-day moving average 
# 3.1 To compute a 20-day moving average price for Gold ETF for April 2020 including data from March 
getSymbols(Symbols = "GLD", from = "2020-03-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_GldMA <- GLD$GLD.Close 
# 3.1 Compute Gold ETF 20-days Moving average (n=20)
Gld_MA <- SMA(Aprl_Dec_GldMA, n = 20)
plot(Gld_MA, col="red")
summary(Gld_MA)

# 3.2 compute a 20-day moving average for UKOG Equity ETF for April-December 2020
getSymbols(Symbols = "UKOG.L", from = "2020-03-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_UkogMA <- UKOG.L$UKOG.L.Close
# 3.2 Compute UKOG Equity ETF 20-days Moving average (n=20) 
Ukog_MA <- SMA(Aprl_Dec_UkogMA, n=20)
plot(Ukog_MA, col="blue")
summary(Ukog_MA)

# 3.3 compute a 20-day moving average for Bitcoin for April - December 2020
getSymbols(Symbols = 'BTC-USD', from = "2020-03-01", to = "2020-12-31" , src = "yahoo")
Aprl_Dec_BTCMA <- (`BTC-USD`)[,4]
# process 'na' values 
Aprl_Dec_BTCMAvg <- na.omit(Aprl_Dec_BTCMA)
# 3.3 Compute BTC 20-day (n=20) moving average for April - December 2020
BTC_MA <- SMA(Aprl_Dec_BTCMAvg, n=20)
plot(BTC_MA, col="green")
summary(BTC_MA)

# 4.0 Graphing 

