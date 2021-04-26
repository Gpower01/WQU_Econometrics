# building trading strategy during COVID-19
# Selecting a GOLD ETF, Equity EFT outside the USA and Bitcoin

install.packages("egcm")
install.packages("evir") 
install.packages('extRemes')
install.packages("tidyverse") 
install.packages('TTR')
install.packages("urca")
install.packages("tsDyn")
install.packages("data.table")
install.packages("moments")
install.packages("tsoutliers")
install.packages("expsmooth")
install.packages("fma")
install.packages("forecast")
install.packages("EnvStats")
install.packages("outliers")
install.packages("VineCopula")
install.packages("copula")
install.packages("PerformanceAnalytics")
install.packages('xts')
install.packages('DEoptim')
install.packages('dygraphs')

install.packages("devtools")
install.packages("FinancialInstrument")
devtools::install_github("braverock/blotter")
devtools::install_github("braverock/quantstrat")


# load package 
library(quantmod) 
library(ggplot2)
library(tidyverse)
library(TTR)
library(evir)
library(extRemes)
library(moments)
library(tsoutliers)
library(expsmooth)
library(fma)
library(forecast)
library(EnvStats)
library(VineCopula)
library(copula)
library(PerformanceAnalytics)
library(xts)
library(DEoptim)
library(dygraphs)

# 1.0 Data Importing 

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

# 2.0 Data Processing and Daily return computation

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

# 3.0 Data Summaries | Extreme values 
# 3.1 compute return for Gold ETF series | Extreme values 
# store Gold ETF log returns in a variable 
GldLogReturns = Gldlog_rt
GldLogReturns

# To fit the GEV to minima, we take the negative of the variable
NegativeGldLogReturns = -GldLogReturns
NegativeGldLogReturns

# The following call finds the coefficients of the GEV distribution for blocks of 5 days
# (the approximate number of trading days in a month)
gev(NegativeGldLogReturns, block = 5)

# Interpretation of GoldLog Returns GEV
# The sample has 189 observations yielding 38 blocks of length 5
# We obtain statistically significant estimate for all the parameters and observed that the block
# maxima seem to follow a Frechet distribution since xi>0. This is the variance covariance matrix 
# of the parameters. However, the low number of data points clearly leads to severe deterioration 
# of the statistical significance of our parameter estimates. To evaluate the data further 

GldGEV <- gev(NegativeGldLogReturns, block = 5)
plot(GldGEV, main="Gold Log Returns GEV")
2
0

# Obtaining the histogram plot of Gold ETF series extreme values to evaluate the clossness of the GEV distribution
# that we estimated to the emperical density of the data.

GldGEV_hist <- gev(NegativeGldLogReturns, block = 5)
GldGEV_hist_block = GldGEV_hist$data
hist(GldGEV_hist_block)

# Interpretation
# We observe that the Gold ETF data series exhibited clossness of GEV distribution.

# 3.1 compute return for UKOG Equity ETF series | Extreme values 
# store UKOG Equity Returns in a variable
UkogLogReturns = Ukoglog_rt
UkogLogReturns

# To fit the GEV to minima, we take the negative of the variable
NegativeUkogLogReturns = -UkogLogReturns
NegativeUkogLogReturns

# The following call finds the coefficients of the GEV distribution for blocks of 5 days
# (the approximate number of trading days in a month)
gev(NegativeUkogLogReturns, block = 5)

# Interpretation
# The sample has 188 observations yielding 38 blocks of length 5
# We obtain statistically significant estimate for all the parameters and observed that the block
# maxima seem to follow a Frechet distribution since xi>0. This is the variance covariance matrix 
# of the parameters. However, the low number of data points clearly leads to severe deterioration 
# of the statistical significance of our parameter estimates. To evaluate the data further 

UkogGEV <- gev(NegativeUkogLogReturns, block = 5)
plot(UkogGEV, main="UKOG Log Returns GEV")
2
0
# Interpretation
# From the plot, we conclude that for the smaller end of the block maxima, we found the GEV
# distribution does a reasonable job at capturing the behaviour of smaller extrema. For the largest 
# extrema, the distribution of the extrema deviates further from the perfect line. Thus, the most extreme
# negative returns of the UKOG Equity ETF are less well modelled by the GEV approach.


# Obtaining the histogram plot of UKOG Equity ETF series extreme values to evaluate the clossness of the GEV distribution
# that we estimated to the emperical density of the data.
UkogGEV_hist <- gev(NegativeUkogLogReturns, block = 5)
UkogGEV_hist_block = UkogGEV_hist$data
hist(UkogGEV_hist_block)

# Interpretation:
# We conclude that the lower end of the distribution appears to conform to the possible shape of the GEV distribution,
# the very extreme values seems to deviate from the model.

# 3.1 compute return for Bitcoin series | Extreme values 
# store Bitcoin Returns in a variable
BTCLogReturns  = btclog2_rt
BTCLogReturns

# To fit the GEV to minima, we take the negative of the variable
NegativeBTCLogReturns = -BTCLogReturns
NegativeBTCLogReturns

# The following call finds the coefficients of the GEV distribution for blocks of 5 days
# (the approximate number of trading days in a month)
gev(NegativeBTCLogReturns, block = 5)

# Interpretation
# The sample has 267 observations yielding 54 blocks of length 5
# We obtain statistically significant estimate for all the parameters and observed that the block
# maxima seem to follow a Frechet distribution since xi>0. This is the variance covariance matrix 
# of the parameters. However, the low number of data points clearly leads to severe deterioration 
# of the statistical significance of our parameter estimates. To evaluate the data further 

BTCGEV <- gev(NegativeBTCLogReturns, block = 5)
plot(BTCGEV, main="BTC Log Returns GEV")
2
0

# Interpretation
# From the plot, it can be observed that the due the smaller data point, the GEV distribution found it 
# difficult to capture the behaviour of the smaller extrema which are particularly observed towards the end of the plot.

# Obtaining the histogram plot of Bitcoin series extreme values to evaluate the clossness of the GEV distribution
# that we estimated to the emperical density of the data.
BTCGEV_hist <- gev(NegativeBTCLogReturns, block = 5)
BTCGEV_hist_block = BTCGEV_hist$data
hist(BTCGEV_hist_block)

# Interpretation
# We observe that the BTC data series exhibited clossness of GEV distribution.


# 3.2 Computing the kurtosis for each series.
# 3.2 Kurtosis for Gold ETF series
require(e1071)
kurtosis(GldLogReturns)

# Gold ETF kurtosis = 4.074877
# We can observed that Gold ETF kurtosis > 3 which implies a Leptokurtic distribution, sharper than a normal distribution
# with values concentrated around the mean and thicker tails, implying higher probability for extreme values.

# 3.2 Kurtosis for UKOG Equity ETF series 
kurtosis(UkogLogReturns)

# Similarly, the kurtosis of UKOG Equity ETF = 12.1798
# This implies a Leptokurtic distribution since kurtosis > 3: implying a sharper than a normal distribution, with values 
# concentrated around the mean and thicker tails. This implies a higher probability for extreme values 

# 3.2 Kurtosis for BTC series 
kurtosis(BTCLogReturns)

# The kurtosis for Bitcoin = 2.614437:
# This implies a Platykurtic distribution since kurtosis < 3: implying a flatter than normal distribution with a wider peak. 
# The probability for extreme values is less than a normal distribution and the values are wider spread around the mean.

# 3.3 using a common metric that is not 2 SIGMA to identify extreme values with explanation
# Rosner's test is one godd example of a common metric that can be used to identify extreme values. 
# Rosner's test for outliers has the advantages that: it is used to detect several outliers at once 
# (unlike Grubbs and Dixon test which must be performed iteratively to screen for multiple outliers), and
# it is designed to avoid the problem of masking, where an outlier that is close in value to another outlier can go undetected.
# Unlike Dixon test, note that Rosner test is most appropriate when the sample size is large (n???20). 

# 3.3 Using Rosner test to identify Gold ETF extreme values 
#http://finzi.psych.upenn.edu/R/library/EnvStats/html/rosnerTest.html
rosnerTest(GldLogReturns, k=3, alpha = 0.05, warn = TRUE)

# Interpretation: 
# Using Rosner test, it can be observed that Gold ETF series contains 2 main outliers (Extreme Values)

# 3.3 Using Rosner test to identify UKOG Equity ETF extreme values 
# http://finzi.psych.upenn.edu/R/library/EnvStats/html/gofOutlier.object.html
rosnerTest(UkogLogReturns, k=3, alpha = 0.05, warn = TRUE)

# Interpretation
# Using Rosner's test, it can be observed that UKOG Equity ETF series contains 3 main outliers (extreme values)

# 3.3 Using Rosner test to identify Bitcoin Extreme values 
rosnerTest(BTCLogReturns, k=3, alpha = 0.05, warn = TRUE)

# Interpretation
# Using Rosner's test, it can be observed that Bitcoin series contains 2 main outliers (extreme values)


# 3.4 Applying Rosner's test metric to each of the return series:

# 3.4 Applying Rosner's test metric to Gold ETF outliers
# Inspect visually
plot(GldLogReturns, type="p")

OGld <- rosnerTest(GldLogReturns, k = 3)
OGld
OGld$all.stats

# Results: 2 outliers, observation 91 value: -0.55, observation 154 value: -0.452

# Applying Rosner's test to UKOG Equity ETF outliers
# Inspect visually
plot(UkogLogReturns, type= "p")

Oukog <- rosnerTest(UkogLogReturns, k = 7)
Oukog
Oukog$all.stats
# Results: 6 outliers, observation 55,167,84,17,59,165,1 values: 0.48,-0.39,0.30,-0.25,0.19,0.18


# Applying Rosner's test to Bitcoin outliers
# Inspect visually
plot(BTCLogReturns, type= "p")

Obtc <- rosnerTest(BTCLogReturns, k = 3)
Obtc
Obtc$all.stats
# Results: 2 outliers, observation 26,53 values: 0.12,-0.10

# 4.0 Data comparison using Copulas
# 4.1 Pick 2 of the series  that had the most extreme values.
# using the Rosner's test, it can be observed that UKOG Equity ETF series have 6 extreme values (outliers)
# while Gold ETF and Bitcoin have 2 extreme values (outliers). Therefore, we select UKOG Equity ETF and Gold ETF 
# as the series with the most extreme values. 

# 4.2 Graphing the returns of Gold ETF series and UKOG Equity ETF series on x-y plot.
plot( x = index(GldLogReturns ), y = GldLogReturns, type = "l", col = "black", xlab = "Time", ylab = "GLD ETF return", main="GOLD ETF vs UKOG ETF returns") 
par(mar=c(5, 4, 4, 6) + 0.1)
par(new=TRUE)

plot( x = index(UkogLogReturns), y = UkogLogReturns, type = "l", col = "red", xlab = "", ylab = "" , axes=FALSE)
mtext("UKOG ETF return",side=4,col="red",line=4) 
axis(4.5, ylim=c(0,0.2), col="red",col.axis="red",las=1)
legend("topleft",legend=c("GLD ETF returns","UKOG ETF returns"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 4.3 Fit a non-Gaussian copula to Gold ETF returns series and UKOG Equity ETF return series.
# Reference: Statistics and Data Analysis for Financial Engineering chapter 8 p206
# Four parametric copulas were fit to the uniform-transformed data: t, Gaussian, Frank and Clayton.

NGCopula_data = cbind(GldLogReturns, UkogLogReturns)
# Processing NGCopula data 
NGCopula_data<- na.omit(NGCopula_data) 
NGCopula_data

# Fitting the copulas using parametric pseudo-maximum likelihood.
var_a <- pobs(NGCopula_data)[,1]
var_b <- pobs(NGCopula_data)[,2]

PobsGLD <- unclass(var_a) 
PobsUKog <- unclass(var_b) 
plot(PobsGLD,PobsUKog)

# Assuming that the two flows have a meta-Gaussian distribution. There are three ways to estimate the correlation in their 
# Gaussian copula. The first, Spearman's rank correlation, is estimated ???0.0237. The
# second is sin(??????/2), where ???? is the sample Kendall's tau rank correlation; its value is ???0.024. 
# The third, Pearson correlation of the normal-transformed flows, is ???0.0237.
# There is reasonably close agreement among the three values
rhos <- cor(var_a,var_b, method = "spearman")
rhoTau <- cor(var_a,var_b, method = "kendall")
omega <- sin(pi/2*rhoTau)
rhop <- cor(var_a,var_b, method = "pearson")

# Since we used parametric estimates to transform the flows, we are fitting the copulas by parametric pseudo-maximum likelihood
# Reference: https://www.r-bloggers.com/2016/03/how-to-fit-a-copula-model-in-r-heavily-revised-part-2-fitting-the-copula/

# Fitting non-Gaussian Copula to the return series of Gold ETF and UKOG Equity ETF series using Student's T
cop_model <- tCopula(dim = 2)
copula_data <- pobs(as.matrix(NGCopula_data))
start <- c(omega, 10)

Ct = fitCopula(cop_model, copula_data, method="ml", start)
Ct@estimate
loglikCopula(Ct@estimate, copula_data, cop_model)
-2*.Last.value + 2*length(Ct@estimate)

# Fitting non-Gaussian to the return series of Gold ETF and UKOG Equity ETF series using normalCoupla
 cop_model1 <- normalCopula(dim = 2)
 copula_data <- pobs(as.matrix(NGCopula_data))
 start1 <- c(omega)

Cgauss = fitCopula(cop_model1, copula_data, method="ml", start1)
Cgauss@estimate
loglikCopula(Cgauss@estimate, copula_data, cop_model1)
-2*.Last.value + 2*length(Cgauss@estimate)

# Fitting non-Gaussian to the return series of Gold ETF and UKOG Equity ETF series using frankCopula
cop_model2 <- frankCopula(1, dim = 2)
copula_data <- pobs(as.matrix(NGCopula_data))

Cfr = fitCopula(cop_model2, copula_data, method="ml")
Cfr@estimate
loglikCopula(Cfr@estimate, copula_data, cop_model2)
-2*.Last.value + 2*length(Cfr@estimate)

# Fitting non-Gaussian to the return series of Gold ETF and UKOG Equity ETF series using claytonCopula
cop_model3 <- claytonCopula(1, dim = 2)
copula_data <- pobs(as.matrix(NGCopula_data))

Ccl = fitCopula(cop_model3, copula_data, method="ml")
Ccl@estimate
loglikCopula(Ccl@estimate, copula_data, cop_model3)
-2*.Last.value + 2*length(Ccl@estimate)

# Estimates of copula parameters, maximized log-likelihood, and AIC using the uniform-transformed pipeline flow data
# t ??=  -6.973664e-03 ; v= 6.896399e+03 ; Max log like = 0.003390885 ; AIC = 3.993218
# Gaussian ??= -0.006950957; Max log like = 0.004014086 ; AIC = 1.991972
# Frank Theta= -0.1456373; Max log like = 0.05249771 ; AIC = 1.895005
# Clayton Theta= 0.02694718; Max log like = 0.05893623 ; AIC = 1.882128
# According to AIC the Clayton copula fits best, followed closely by Frank Copula since they have the smallest values

# 5.0 Category 1 Model using just 1 variable 
# 5.1 Selecting UKOG Equity ETF series to develop trading strategies 
getSymbols(Symbols = "UKOG.L", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
UKOG.L$UKOG.L.Close
Ukog_Closing_Prices <- UKOG.L$UKOG.L.Close
plot( x = index( UKOG.L ), y = Ukog_Closing_Prices )
plot(Ukog_Closing_Prices, xlab = "Date", ylab = "UKOG Closing Price")

# Processing UKOG Equity ETF series 
Ukog_Closing_Prices <- na.omit(Ukog_Closing_Prices)
Ukog_Closing_Prices 

# 5.2 Extracting Q2 UKOG Equity ETF data series for estimation 
UkogQ2_estimate <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices) >="2020-04-01" & index(Ukog_Closing_Prices) <= "2020-06-30"),]
UkogQ2_estimate

# Calculating Q2 UKOG daily returns using closing prices: Calculating returns for different periods can be done by changing k parameter
# For example to calculate returns for lag 1 to 3, we would use the k=1:3 command.
return_UkogQ2_estimate <- Delt(UkogQ2_estimate, k=1)
return_UkogQ2_estimate

# 5.2 Extracting Q3 UKOG Equity ETF series for evaluating performance 
UkogQ3_permAnalysis <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices) >= "2020-07-01" & index(Ukog_Closing_Prices)<= "2020-09-30"),]
UkogQ3_permAnalysis  

# Calculating Q3 UKOG daily returns using closing prices 
return_UkogQ3_permAnalysis <- Delt(UkogQ3_permAnalysis, k=1)
return_UkogQ3_permAnalysis

# 5.3 Extracting Q3 UKOG EQuity ETF series for estimation 
UkogQ3_estimate <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices) >= "2020-07-01" & index(Ukog_Closing_Prices)<= "2020-09-30"),]
UkogQ3_estimate

# Calculating the Q3 UKOG daily returns using closing prices 
return_UkogQ3_estimate <- Delt(UkogQ3_estimate, k=1)
return_UkogQ3_estimate

# 5.3 Extracting Q4 UKOG Equity ETF series for evaluating performance 
UkogQ4_permAnalysis <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices)>= "2020-10-01" & index(Ukog_Closing_Prices)<= "2020-12-31"),]
UkogQ4_permAnalysis

# Calculating Q4 UKOG daily returns using closing prices 
return_UkogQ4_permAnalysis <- Delt(UkogQ4_permAnalysis, k=1)
return_UkogQ4_permAnalysis

# 5.3 and 5.4 Building 2 trading strategies using the above extracted data series for UKOG Equity ETF Q2 & Q3; Q3 & Q4

# 5.4 Building first trading strategy using UKOG Equity ETF Q2 for data estimation and Q3 for evaluating performance
# Applying Backtesting to simulate investment strategy based on historical data, calibrating and evaluating investment strategy.
# Here we will name the Q2 estimate dataset "In-sample dataset" and Q3 perAnalysis dataset "out-sample dataset" and specifically specify start and end dates 

# In-Sample for Estimation (Q2)
UkogQ2_estimate <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices) >="2020-04-01" & index(Ukog_Closing_Prices) <= "2020-06-30"),]
UkogQ2_estimate

# Daily Returns 
return_UkogQ2_estimate <- Delt(UkogQ2_estimate, k=1)
return_UkogQ2_estimate

# Out- Sample for Performance Evaluation (Q3)
UkogQ3_permAnalysis <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices) >= "2020-07-01" & index(Ukog_Closing_Prices)<= "2020-09-30"),]
UkogQ3_permAnalysis  

# Daily Returns 
return_UkogQ3_permAnalysis <- Delt(UkogQ3_permAnalysis, k=1)
return_UkogQ3_permAnalysis

# Next we will generate automated trading signals using: Moving average convergence divergence (MACD) and Bollinger band indicators
# Note: MACD parameters can be adjusted considering the trading strategy
macd_UkogQ2 <- MACD(UkogQ2_estimate, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
bollingerband_UkogQ2 <- BBands(UkogQ2_estimate, n=20, maType = "SMA", sd=2)

# The trading strategy is based on the following outcome:
# Signal is first initialized with NULL 
# If UKOG > upper Bollinger band and MACD value > MACD signal -> Buy signal(1)
# If UKOG < lower Bollinger band and MACD value < MACD signal -> Sell signal (-1)
# Out of the market -> signal 0

signal <- NULL 
signal <- ifelse(UkogQ2_estimate > bollingerband_UkogQ2[, "up"]
                 &macd_UkogQ2[, "macd"]>macd_UkogQ2[, "signal"],1, ifelse(UkogQ2_estimate< bollingerband_UkogQ2[, "dn"]
                                                            &macd_UkogQ2[, "macd"]<macd_UkogQ2[, "signal"], -1, 0))

# Trading Strategy Assumptions:
# No transaction costs are included in the analysis 
# The trading strategy can be used for both long and short positions.
# A long only or short only strategy can also be implemented 
# The exit criteria can also be changed.

# Calculating trade returns
UkogQ2_trade_return <- return_UkogQ2_estimate*stats::lag(signal)
UkogQ2_trade_return

# Calculate the cumulative return
UkogQ2_Cum_return <- Return.cumulative(UkogQ2_trade_return)
UkogQ2_Cum_return

# calculate the annualized return 
UkogQ2_Annual_ret <- Return.annualized(UkogQ2_trade_return)
UkogQ2_Annual_ret

# Chart Performance Summary 
charts.PerformanceSummary(UkogQ2_trade_return)

#Summary 
summary(as.ts(UkogQ2_trade_return))

# Interpretations | Comments
# It was observed that cumulative returns are positive at the end of the UKOG Q2 period, implying that the strategy
# is profitatble. However, there are periods when the cumulative returns were negative.
# Therefore this strategy should be evaluated with the Q3 series.

# maxDrwdown
maxDrawdown(UkogQ2_trade_return)

# Calculating Daily and annualized standard deviations
StdDev(UkogQ2_trade_return)

StdDev.annualized(UkogQ2_trade_return)

# Calculating Value at Risk (VaR)
VaR(UkogQ2_trade_return, p=0.95)

# Calculating the Sharpe ratio on daily and annualized basis
SharpeRatio(as.ts(UkogQ2_trade_return), Rf=0, p=0.95, FUN = "StdDev")

SharpeRatio.annualized(UkogQ2_trade_return, Rf=0)

# Next, Evaluating Trading Strategy Performance Using UKOG Q3 data series
# The trading strategy appears to provide a good result in the UKOG Q2 estimated dataset.
# This strategy is now being tested further using UKOG Q3 data series to evaluate performance 
# in a generalized sample dataset.
require("quantmod"); require("PerformanceAnalytics"); require("DEoptim")

# Calculating Moving average and bBllinger bands for UKOG Equity ETF Q3
macd_UkogQ3 <- MACD(UkogQ3_permAnalysis, nFast = 7, nSlow = 12, nSig = 15, maType = "SMA", percent = FALSE)
bollingerband_UkogQ3 <- BBands(UkogQ3_permAnalysis, n=20, maType = "SMA", sd=2)

# Generate signals for Q3 UKOG Equity ETF data series for performance evaluation 
signal <- NULL 
signal <- ifelse(UkogQ3_permAnalysis > bollingerband_UkogQ3[, "up"]
                 &macd_UkogQ3[, "macd"]>macd_UkogQ3[, "signal"],1, ifelse(UkogQ3_permAnalysis<bollingerband_UkogQ3[, "dn"]
                                                                          &macd_UkogQ3[, "macd"]<macd_UkogQ3[, "signal"], -1, 0))
# Testing performance 
# Calculate  UKOG Equity ETF trade return 

UkogQ3_trade_return <- return_UkogQ3_permAnalysis*stats::lag(signal)
UkogQ3_trade_return

# Calculate the cumulative return
UkogQ3_Cum_return <- Return.cumulative(UkogQ3_trade_return )
UkogQ3_Cum_return

# calculate the annualized return 
UkogQ3_Annual_ret <- Return.annualized(UkogQ3_trade_return)
UkogQ3_Annual_ret

# Chart Performance Summary 
charts.PerformanceSummary(UkogQ3_trade_return)
maxdd_UkogQ3 <- maxDrawdown(UkogQ3_trade_return)
sd_UkogQ3 <- StdDev(UkogQ3_trade_return)
sda_UkogQ3 <- StdDev.annualized(UkogQ3_trade_return)
VaR(UkogQ3_trade_return, p=0.95)
SharpeRatio(as.ts(UkogQ3_trade_return), Rf=0, p=0.95, FUN="StdDev")
SharpeRatio.annualized(UkogQ3_trade_return, Rf=0)

# 5.3 and 5.4 repeating Trading strategy using UKOG Q3 for estimation and Q4 for evaluation
# 5.4 In-sample: Extracting Q3 UKOG EQuity ETF series for estimation 
UkogQ3_estimate <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices) >= "2020-07-01" & index(Ukog_Closing_Prices)<= "2020-09-30"),]
UkogQ3_estimate

# Calculating the Q3 UKOG daily returns using closing prices 
return_UkogQ3_estimate <- Delt(UkogQ3_estimate, k=1)
return_UkogQ3_estimate

# 5.3 and 5.4 Out-sample: Extracting Q4 UKOG Equity ETF series for evaluating performance 
UkogQ4_permAnalysis <- Ukog_Closing_Prices[(index(Ukog_Closing_Prices)>= "2020-10-01" & index(Ukog_Closing_Prices)<= "2020-12-31"),]
UkogQ4_permAnalysis

# Calculating Q4 UKOG daily returns using closing prices 
return_UkogQ4_permAnalysis <- Delt(UkogQ4_permAnalysis, k=1)
return_UkogQ4_permAnalysis

# Next we will generate automated trading signals using: Moving average convergence divergence (MACD) and Bollinger band indicators
# Note: MACD parameters can be adjusted considering the trading strategy
macd_UkogQ3 <- MACD(UkogQ3_estimate, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
bollingerband_UkogQ3 <- BBands(UkogQ3_estimate, n=20, maType = "SMA", sd=2)

# The trading strategy is based on the following outcome:
# Signal is first initialized with NULL 
# If UKOG > upper Bollinger band and MACD value > MACD signal -> Buy signal(1)
# If UKOG < lower Bollinger band and MACD value < MACD signal -> Sell signal (-1)
# Out of the market -> signal 0

signal <- NULL 
signal <- ifelse(UkogQ3_estimate > bollingerband_UkogQ3[, "up"]
                 &macd_UkogQ3[, "macd"]>macd_UkogQ3[, "signal"],1, ifelse(UkogQ3_estimate< bollingerband_UkogQ3[, "dn"]
                                                                          &macd_UkogQ3[, "macd"]<macd_UkogQ3[, "signal"], -1, 0))

# Trading Strategy Assumptions:
# No transaction costs are included in the analysis 
# The trading strategy can be used for both long and short positions.
# A long only or short only strategy can also be implemented 
# The exit criteria can also be changed.

# Calculating trade returns
UkogQ3_trade_return <- return_UkogQ3_estimate*stats::lag(signal)
UkogQ3_trade_return

# Calculate the cumulative return
UkogQ3_Cum_return <- Return.cumulative(UkogQ3_trade_return)
UkogQ3_Cum_return

# calculate the annualized return 
UkogQ3_Annual_ret <- Return.annualized(UkogQ3_trade_return)
UkogQ3_Annual_ret

# Chart Performance Summary 
charts.PerformanceSummary(UkogQ3_trade_return)

#Summary 
summary(as.ts(UkogQ3_trade_return))

# Interpretations | Comments
# It was observed that cumulative returns are positive at the end of the UKOG Q2 period, implying that the strategy
# is profitatble. However, there are periods when the cumulative returns were negative.
# Therefore this strategy should be evaluated with the Q3 series.

# maxDrwdown
maxDrawdown(UkogQ2_trade_return)

# Calculating Daily and annualized standard deviations
StdDev(UkogQ2_trade_return)

StdDev.annualized(UkogQ2_trade_return)

# Calculating Value at Risk (VaR)
VaR(UkogQ2_trade_return, p=0.95)

# Calculating the Sharpe ratio on daily and annualized basis
SharpeRatio(as.ts(UkogQ2_trade_return), Rf=0, p=0.95, FUN = "StdDev")

SharpeRatio.annualized(UkogQ2_trade_return, Rf=0)


#5.3 and 5.4:  Next UKOG Q4 for evaluation performance 
# Calculating Moving average and bBllinger bands for UKOG Equity ETF Q3
macd_UkogQ4 <- MACD(UkogQ4_permAnalysis, nFast = 7, nSlow = 12, nSig = 15, maType = "SMA", percent = FALSE)
bollingerband_UkogQ4 <- BBands(UkogQ4_permAnalysis, n=20, maType = "SMA", sd=2)

# Generate signals for Q3 UKOG Equity ETF data series for performance evaluation 
signal <- NULL 
signal <- ifelse(UkogQ4_permAnalysis > bollingerband_UkogQ4[, "up"]
                 &macd_UkogQ4[, "macd"]>macd_UkogQ4[, "signal"],1, ifelse(UkogQ4_permAnalysis<bollingerband_UkogQ4[, "dn"]
                                                                          &macd_UkogQ4[, "macd"]<macd_UkogQ4[, "signal"], -1, 0))
# Testing performance 
# Calculate  UKOG Equity ETF trade return 

UkogQ4_trade_return <- return_UkogQ4_permAnalysis*stats::lag(signal)
UkogQ4_trade_return

# Calculate the cumulative return
UkogQ4_Cum_return <- Return.cumulative(UkogQ4_trade_return )
UkogQ4_Cum_return

# calculate the annualized return 
UkogQ4_Annual_ret <- Return.annualized(UkogQ4_trade_return)
UkogQ4_Annual_ret

# Chart Performance Summary 
charts.PerformanceSummary(UkogQ4_trade_return)
maxdd_UkogQ4 <- maxDrawdown(UkogQ4_trade_return)
sd_UkogQ4 <- StdDev(UkogQ4_trade_return)
sda_UkogQ4 <- StdDev.annualized(UkogQ4_trade_return)
VaR(UkogQ4_trade_return, p=0.95)
SharpeRatio(as.ts(UkogQ4_trade_return), Rf=0, p=0.95, FUN="StdDev")
SharpeRatio.annualized(UkogQ4_trade_return, Rf=0)

# Overall Model Interpretations
# Overall interpretation and comparison of the models for Q2 & Q3 (estimation data seriesn) and (Q3 & Q4 perfomance evaluation)
# It is observed that model with trading strategy Q3 (estimate) and Q4 (performance evaluation) appears to perform better than the model with 
# Q2 (estimate) and Q3 (performance evaluation) for the UKOG Equity ETF time series. 


# 5.4 and 5.5 Implementing a trading strategy and applying several different indicators with ARMA: Moving averages 

install.packages("remotes")
remotes::install_github("braverock/quantstrat")

# package 
library(blotter)
library(foreach)
library(quantstrat)

# Get UKOG Dataset 
getSymbols(Symbols = "UKOG.L", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
UKOG.L$UKOG.L.Close
Ukog_Closing_Prices <- UKOG.L$UKOG.L.Close
plot( x = index( UKOG.L ), y = Ukog_Closing_Prices )
plot(Ukog_Closing_Prices, xlab = "Date", ylab = "UKOG Closing Price")

# Extract UKOG Q2| Estimating data series 
getSymbols("UKOG.L", 
           from = "2020-04-01",
           to = "2020-06-30",
           src = "yahoo",
           adjust = TRUE)
plot(Hi(UKOG.L))

# Plot the closing price of UKOG
plot(Cl(UKOG.L))

# Adding indicators to the financial data series 
# 1. Add moving average: add a 5- days moving average using lines 
lines(SMA(Cl(UKOG.L), n=5), col="red")

# Add boiler plates for quantsrat strategies | Setting up a strategy 
# Set time zone 
Sys.setenv(TZ = "Europe/London")
currency("EUR")

# Now create the initDate, from and to 
initdate <- "2020-04-01"
from <- "2020-04-01"
to <- "2020-06-30"

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD 
currency("USD")

# Retrieve UKOG.L Equity ETF Data from yahoo
getSymbols("UKOG.L", from = from, to = to, src = "yahoo", adjust = TRUE)

# Use stock to initialize UKOG.L and set currency to USD
stock("UKOG.L", currency = "USD")

# Define trading size and initial equity 
tradesize <- 100000
initeq <- 100000

# Define  the names of strategy, portfolio and account 
strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

# Remove existing strategy if it exits 
rm.strat(strategy.st)

# initialize portfolio 
initPortf(portfolio.st, symbols = "UKOG.L", initDate = initdate, currency = "USD")

# Initialize trading account 
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize orders 
initOrders(portfolio.st, initDate = initdate)

# store strategy 
strategy(strategy.st, store = TRUE)

# Add trading indicators | SMA and RSI
# create a 5-days moving average SMA
ukogq2_sma <- SMA(x = Cl(UKOG.L), n = 5)

# create an RSI with a 3-day lookback period and duration 
ukogq2_rsi <- RSI(price = Cl(UKOG.L), n = 3)

# Visualize UKOG Q2 closing price and visualize SMAs
plot(Cl(UKOG.L))
# overlay a 5-day SMA 
lines(SMA(Cl(UKOG.L), n = 5), col = "red")

# Type of indicator is:
print("trend")

# Visualize RSI 2
plot(RSI(Cl(UKOG.L), n = 2))

# Type of indicator is:
print("reversion")

# Applying trading strategy with SMA=5 indicator
add.indicator(strategy = strategy.st, 
              
              # create the SMA function
              name = "SMA", 
              
              # generate a lookback function
              arguments = list(x = quote(Cl(mktdata)), n = 5), 
              
              # Label my indicator SMA5
              label = "SMA5")

# Implement another SMA10 indicator to the trading strategy 
add.indicator(strategy = strategy.st, 
              
              # create the SMA function
              name = "SMA", 
              
              # generate a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 10), 
              
              # Label my indicator SMA10
              label = "SMA10")

# Implement RSI indicator to the trading strategy 
add.indicator(strategy = strategy.st, 
              
              # create RSI 3 function
              name = "RSI", 
              
              # generate a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              
              # Label your indicator RSI_3
              label = "RSI_3")

# Write a function to calculate RSI average 
calc_RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 accepts an input data of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  
  # RSI 2 accepts an input data of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # To calculate the RSI average of 1 and 2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  # customise the output column name 
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}
# Add RSI_3 and RSI_4 to the trading strategy with n1=3 and n2 =4
add.indicator(strategy.st, name = "calc_RSI_avg", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

# function to create DVO indictor 
DVO <- function(HLC, navg = 2, percentlookback = 50) {
  
  # function to compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/(Hi(HLC) + Lo(HLC))/2
  
  # function to smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # function to convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Now apply the created DVO indicator 
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 50),
              label = "DVO_2_50")


# Use the apply indicator to test the created DVO indicator 
test <- applyIndicators(strategy = strategy.st, mktdata = HLC(UKOG.L))

# Now to test the trading strategy using UKOG Q3
testQ3<- getSymbols("UKOG.L", 
           from = "2020-07-01", 
           to = "2020-09-30", 
           src =  "yahoo", 
           adjust =  TRUE)
testQ3

# Function to test Trading strategy with Q3: from "2020-07-01" to "2020-09-30"
test_strategy <- test["2020-07-01/2020-09-030"]

# Adding signals
# Add a signal comparison that indicated that SMA5 must be greater than SMA10 and call this longfitter
add.signal(strategy.st, name = "sigComparison", 
           
           # Only interested in the relationship between the SMA5 and the SMA10
           arguments = list(columns = c("SMA5", "SMA10"), 
                            
                            # Most importantly interested when the SMA5 is greater than the SMA10
                            relationship = "gt"),
           
           # call the signal longfilter
           label = "longfilter")

# Next add a SigCrossover that indicates that the SMA5 is less than SMA10 and call this signal filterexit
add.signal(strategy.st, name = "sigCrossover",
           
           # Only interested in the relationship between the SMA5 and the SMA10
           arguments = list(columns = c("SMA5", "SMA10"),
                            
                            # function that indicates when the SMA5 crosses under the SMA10
                            relationship = "lt"),
           
           # call this signal filterexit
           label = "filterexit")

# Now specify threshold | sigThreshold
# Implement sigThreshold that indicates DVO_2_50 must be less than 10 and call this longthreshold.
add.signal(strategy.st, name = "sigThreshold", 
           
           # function use the created DVO_2_50 column
           arguments = list(column = "DVO_2_50", 
                            
                            #set threshold to 10
                            threshold = 10, 
                            
                            # This function will ensure the oscillator remain under this value
                            relationship = "lt", 
                            
                            # This is to make sure that in every instance that the oscillator is less than 10
                            cross = FALSE), 
           
           # call thus longthreshold
           label = "longthreshold")

# Now compute exit sigThreshold
# This function (sigThreshold signal exit) indicates that the created signal indicator DVO_2_50 must cross above 40 and call this thresholdexit
add.signal(strategy.st, name = "sigThreshold", 
           
           # This column makes use of DVO_2_50 signal
           arguments = list(column = "DVO_2_50", 
                            
                            # set the max threshold of 40
                            threshold = 40, 
                            
                            # ensures that the oscillator must be greater than 40
                            relationship = "gt", 
                            
                            # Here we are particularly interested only in the cross
                            cross = TRUE), 
           
           # call this thresholdexit
           label = "thresholdexit")

# Now let's create a test dataset: test 
test_init <- applyIndicators(strategy.st, mktdata = OHLC(UKOG.L))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

# Combine the longfilter and longthreshold signals| Using the UKOG Equity ETF Q3 ("2020-07-01/2020-09-030") data series to test the trdading strategy 
test["2020-07-01/2020-09-030"]

# The indicator signales: 
print("longfilter = 1, longthreshold = 1")

# 5.5 and 5.6 Repeat model trading strategy with UKOG Equity ETF Q3 (estimate) and Q4 (performance evaluation) incorporating some form of GRACH model
# perform GARCH Modeling on UKOG Q3
getSymbols(Symbols = "UKOG.L", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ3 = UKOG.L$UKOG.L.Close
UKOGQ3 <- na.omit(UKOGQ3)
library(rugarch)
ukog_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = 'norm')
gurcg_ukogq3 <- ugarchfit(spec =gld_gurch, data = UKOGQ3)
print(gurcg_ukogq3)
summary(gurcg_ukogq3)

# perform GARCH Modeling on UKOG Q4
getSymbols(Symbols = "UKOG.L", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ4 = UKOG.L$UKOG.L.Close
UKOGQ4 <- na.omit(UKOGQ4)
library(rugarch)
ukog_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(model = "sGARCH"),
                         distribution.model = 'norm')
gurcg_ukogq4 <- ugarchfit(spec =gld_gurch, data = UKOGQ4)
print(gurcg_ukogq4)
summary(gurcg_ukogq4)

# 5.5 and 5.6 Building trading strategy incorporating GARCH model results using Backtesting strategy 
# Using UKOG Q3 series for estimation and Q4 for performance evaluation 
# Retrieve Q3 Data 
getSymbols(Symbols = "UKOG.L", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ3 = UKOG.L$UKOG.L.Close
UKOGQ3 <- na.omit(UKOGQ3)

# create technical indicators: Simple moving averages: SMA using Q3 for estimate 
barChart(UKOGQ3, theme = chartTheme('black'))
sma5 <- SMA(UKOG.L$UKOG.L.Close, n=5)
sma10 <- SMA(UKOG.L$UKOG.L.Close, n=10)
lineChart(UKOG.L, theme = chartTheme('black'))
addSMA(n = 5, col = 'blue')
addSMA(n = 10, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('UKOG.L','SMA5','SMA10'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Apply parabolic Stop and Reverse (SAR) strategy 
sar_ukog <- SAR(cbind(Hi(UKOG.L),Lo(UKOG.L)), accel = c(0.02, 0.2))
barChart(UKOG.L, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')

# Apply Commodity Channeling Index (CCI): Here we have to pass on daily High , Low and Close Prices 
# along with a specified time period and a constant value
# So here we are going to take 20 days period and 0.015 as the constant value
cci_Ukog <- CCI(HLC(UKOG.L), n = 20, c = 0.015)
barChart(UKOG.L, theme = 'black')
addCCI(n = 20, c = 0.015)

# Now calculate the rate of chnage ROC and we will use 25-days as the period
roc_ukog <- ROC(UKOG.L$UKOG.L.Close, n = 25)
barChart(UKOG.L, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Add Stochastic momentum Index (SMI)
smi_ukog <- SMI(HLC(UKOG.L),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(UKOG.L, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# Add Williams % R  and we chose 14 days period
wpr_ukog <- WPR(HLC(UKOG.L), n = 14)
colnames(wpr_ukog) <- 'wpr'
barChart(UKOG.L, theme = 'black')
addWPR(n = 14)

# Create Trading Signals 
# The following code will create trading signals using SMA
# SMA 5 Crossover Signal 
# SMA 5 Crossover Signal 
sma5_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma5) & Cl(UKOG.L) > sma5,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma5) & Cl(UKOG.L) < sma5,-1,0)))
sma5_ts[is.na(sma5_ts)] <- 0
# SMA 10 Crossover Signal
sma10_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma10) & Cl(UKOG.L) > sma10,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma10) & Cl(UKOG.L) < sma10,-1,0)))
sma10_ts[is.na(sma10_ts)] <- 0
# SMA 5 and SMA 10 Crossover Signal
sma_ts <- Lag(
  ifelse(Lag(sma5) < Lag(sma10) & sma5 > sma10,1,
         ifelse(Lag(sma5) > Lag(sma10) & sma5 < sma10,-1,0)))
sma_ts[is.na(sma_ts)] <- 0

# Trading Signals using Parabolic and Reverse (SAR)
sar_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sar_ukog) & Cl(UKOG.L) > sar_ukog,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sar_ukog) & Cl(UKOG.L) < sar_ukog,-1,0)))
sar_ts[is.na(sar_ts)] <- 0

# Add trading signals using CCI 
cci_ts <- Lag(
  ifelse(Lag(cci_Ukog) < (-100) & cci_Ukog > (-100),1,
         ifelse(Lag(cci_Ukog) < (100) & cci_Ukog > (100),-1,0)))
cci_ts[is.na(cci_ts)] <- 0

# Add trading signals using rate of change ROC
roc_ts <- Lag(
  ifelse(Lag(roc_ukog) < (-0.05) & roc_ukog > (-0.05),1,
         ifelse(Lag(roc_ukog) < (0.05) & roc_ukog > (0.05),-1,0)))
roc_ts[is.na(roc_ts)] <- 0

# Add trading signals using Stochastic Simulation (SMI)
smi_ts <- Lag(
  ifelse(Lag(smi_ukog[,1]) < Lag(smi_ukog[,2]) & smi_ukog[,1] > smi_ukog[,2],1, 
         ifelse(Lag(smi_ukog[,1]) > Lag(smi_ukog[,2]) & smi_ukog[,1] < smi_ukog[,2],-1,0)))
smi_ts[is.na(smi_ts)] <- 0

# Add Williams % R signals 
wpr_ts <- Lag(
  ifelse(Lag(wpr_ukog) > 0.8 & wpr_ukog < 0.8,1,
         ifelse(Lag(wpr_ukog) > 0.2 & wpr_ukog < 0.2,-1,0)))
wpr_ts[is.na(wpr_ts)] <- 0

# Creating Trading Strategies 
sma_strat <- ifelse(sma_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sma_strat[i] <- ifelse(sma_ts[i] == 1,1,ifelse(sma_ts[i] == -1,0,sma_strat[i-1]))
}
sma_strat[is.na(sma_strat)] <- 1
sma_stratcomp <- cbind(sma5, sma10, sma_ts, sma_strat)
colnames(sma_stratcomp) <- c('SMA(5)','SMA(10)','SMA SIGNAL','SMA POSITION')

# Parabolic Stop and Reverse (SAR)
sar_strat <- ifelse(sar_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sar_strat[i] <- ifelse(sar_ts[i] == 1,1,ifelse(sar_ts[i] == -1,0,sar_strat[i-1]))
}
sar_strat[is.na(sar_strat)] <- 1
sar_stratcomp <- cbind(Cl(UKOG.L), sar_ukog, sar_ts, sar_strat)
colnames(sar_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')


# Commodity Channel Index (CCI)
cci_strat <- ifelse(cci_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  cci_strat[i] <- ifelse(cci_ts[i] == 1,1,ifelse(cci_ts[i] == -1,0,cci_strat[i-1]))
}
cci_strat[is.na(cci_strat)] <- 1
cci_stratcomp <- cbind(cci_Ukog, cci_ts, cci_strat)
colnames(cci_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# Rate of Change ROC
roc_strat <- ifelse(roc_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  roc_strat[i] <- ifelse(roc_ts[i] == 1,1,ifelse(roc_ts[i] == -1,0,roc_strat[i-1]))
}
roc_strat[is.na(roc_strat)] <- 1
roc_stratcomp <- cbind(roc_ukog, roc_ts, roc_strat)
colnames(roc_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# Stochastic Momentum Index (SMI) 
smi_strat <- ifelse(smi_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  smi_strat[i] <- ifelse(smi_ts[i] == 1,1,ifelse(smi_ts[i] == -1,0,smi_strat[i-1]))
}
smi_strat[is.na(smi_strat)] <- 1
smi_stratcomp <- cbind(smi_ukog[,1],smi_ukog[,2],smi_ts,smi_strat)
colnames(smi_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# Williams % R
wpr_strat <- ifelse(wpr_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  wpr_strat[i] <- ifelse(wpr_ts[i] == 1,1,ifelse(wpr_ts[i] == -1,0,wpr_strat[i-1]))
}
wpr_strat[is.na(wpr_strat)] <- 1
wpr_stratcomp <- cbind(wpr_ukog, wpr_ts, wpr_strat)
colnames(wpr_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# Backtesting and Comparing result 
ret_ukog <- diff(log(Cl(UKOG.L)))

benchmark_ukog <- ret_ukog


# Simple Moving Average SMA
sma_ret <- ret_ukog*sma_strat
sma_ret_commission_adj <- ifelse((sma_ts == 1|sma_ts == -1) & sma_strat != Lag(sma_ts), (ret_ukog-0.05)*sma_strat, ret_ukog*sma_strat)
sma_comp <- cbind(sma_ret, sma_ret_commission_adj, benchmark_ukog)
colnames(sma_comp) <- c('SMA','SMA Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sma_comp, main = 'UKOG.L SMA Performance')
sma_comp_table <- table.AnnualizedReturns(sma_comp)

# Parabolic performance 
sar_ret <- ret_ukog*sar_strat
sar_ret_commission_adj <- ifelse((sar_ts == 1|sar_ts == -1) & sar_strat != Lag(sar_ts), (ret_ukog-0.05)*sar_strat, ret_ukog*sar_strat)
sar_comp <- cbind(sar_ret, sar_ret_commission_adj, benchmark_ukog)
colnames(sar_comp) <- c('SAR','SAR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(sar_comp, main = 'UKOG.L Parabolic SAR Performance')
sar_comp_table <- table.AnnualizedReturns(sar_comp)

# Commodity Channel Index (CCI)
cci_ret <- ret_ukog*cci_strat
cci_ret_commission_adj <- ifelse((cci_ts == 1|cci_ts == -1) & cci_strat != Lag(cci_ts), (ret_ukog-0.05)*cci_strat, ret_ukog*cci_strat)
cci_comp <- cbind(cci_ret, cci_ret_commission_adj, benchmark_ukog)
colnames(cci_comp) <- c('CCI','CCI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(cci_comp, main = 'UKOG.L CCI Performance')
cci_comp_table <- table.AnnualizedReturns(cci_comp)

# Rate of Change ROC 
roc_ret <- ret_ukog*roc_strat
roc_ret_commission_adj <- ifelse((roc_ts == 1|roc_ts == -1) & roc_strat != Lag(roc_ts), (ret_ukog-0.05)*roc_strat, ret_ukog*roc_strat)
roc_comp <- cbind(roc_ret, roc_ret_commission_adj, benchmark_ukog)
colnames(roc_comp) <- c('ROC','ROC Commission Adj','Apple Benchmark')
charts.PerformanceSummary(roc_comp, main = 'UKOG.L ROC Performance')
roc_comp_table <- table.AnnualizedReturns(roc_comp)

# Stochastic Momentum Index (SMI)
smi_ret <- ret_ukog*smi_strat
smi_ret_commission_adj <- ifelse((smi_ts == 1|smi_ts == -1) & smi_strat != Lag(smi_ts), (ret_ukog-0.05)*smi_strat, ret_ukog*smi_strat)
smi_comp <- cbind(smi_ret, smi_ret_commission_adj, benchmark_ukog)
colnames(smi_comp) <- c('SMI','SMI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(smi_comp, main = 'UKOG.L SMI Performance')
smi_comp_table <- table.AnnualizedReturns(smi_comp)

# Williams % R
wpr_ret <- ret_ukog*wpr_strat
wpr_ret_commission_adj <- ifelse((wpr_ts == 1|wpr_ts == -1) & wpr_strat != Lag(wpr_ts), (ret_ukog-0.05)*wpr_strat, ret_ukog*wpr_strat)
wpr_comp <- cbind(wpr_ret, wpr_ret_commission_adj, benchmark_ukog)
colnames(wpr_comp) <- c('WPR','WPR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(wpr_comp, main = 'UKOG.L WPR Performance')
wpr_comp_table <- table.AnnualizedReturns(wpr_comp)


# Retrieve UKOG Q4 Data series | Evaluating Performance 
getSymbols(Symbols = "UKOG.L", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ4 = UKOG.L$UKOG.L.Close
UKOGQ4 <- na.omit(UKOGQ4)

# create technical indicators: Simple moving averages: SMA using Q4 for performance analysis  
barChart(UKOGQ4, theme = chartTheme('black'))
barChart(UKOGQ3, theme = chartTheme('black'))
sma5 <- SMA(UKOG.L$UKOG.L.Close, n=5)
sma10 <- SMA(UKOG.L$UKOG.L.Close, n=10)
lineChart(UKOG.L, theme = chartTheme('black'))
addSMA(n = 5, col = 'blue')
addSMA(n = 10, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('UKOG.L','SMA5','SMA10'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Apply parabolic Stop and Reverse (SAR) strategy 
sar_ukog <- SAR(cbind(Hi(UKOG.L),Lo(UKOG.L)), accel = c(0.02, 0.2))
barChart(UKOG.L, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')

# Apply Commodity Channeling Index (CCI): Here we have to pass on daily High , Low and Close Prices 
# along with a specified time period and a constant value
# So here we are going to take 5 days period and 0.015 as the constant value
cci_Ukog <- CCI(HLC(UKOG.L), n = 20, c = 0.015)
barChart(UKOG.L, theme = 'black')
addCCI(n = 20, c = 0.015)

# Now calculate the rate of change ROC and we will use 25-days as the period
roc_ukog <- ROC(UKOG.L$UKOG.L.Close, n = 25)
barChart(UKOG.L, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Add Stochastic momentum Index (SMI)
smi_ukog <- SMI(HLC(UKOG.L),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(UKOG.L, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# Add Williams % R  and we chose 14 days period
wpr_ukog <- WPR(HLC(UKOG.L), n = 14)
colnames(wpr_ukog) <- 'wpr'
barChart(UKOG.L, theme = 'black')
addWPR(n = 14)

# Create Trading Signals 
# The following code will create trading signals using SMA
# SMA 5 Crossover Signal 
# SMA 5 Crossover Signal 
sma5_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma5) & Cl(UKOG.L) > sma5,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma5) & Cl(UKOG.L) < sma5,-1,0)))
sma5_ts[is.na(sma5_ts)] <- 0
# SMA 10 Crossover Signal
sma10_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma10) & Cl(UKOG.L) > sma10,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma10) & Cl(UKOG.L) < sma10,-1,0)))
sma10_ts[is.na(sma10_ts)] <- 0
# SMA 5 and SMA 10 Crossover Signal
sma_ts <- Lag(
  ifelse(Lag(sma5) < Lag(sma10) & sma5 > sma10,1,
         ifelse(Lag(sma5) > Lag(sma10) & sma5 < sma10,-1,0)))
sma_ts[is.na(sma_ts)] <- 0

# Trading Signals using Parabolic and Reverse (SAR)
sar_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sar_ukog) & Cl(UKOG.L) > sar_ukog,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sar_ukog) & Cl(UKOG.L) < sar_ukog,-1,0)))
sar_ts[is.na(sar_ts)] <- 0

# Add trading signals using CCI 
cci_ts <- Lag(
  ifelse(Lag(cci_Ukog) < (-100) & cci_Ukog > (-100),1,
         ifelse(Lag(cci_Ukog) < (100) & cci_Ukog > (100),-1,0)))
cci_ts[is.na(cci_ts)] <- 0

# Add trading signals using rate of change ROC
roc_ts <- Lag(
  ifelse(Lag(roc_ukog) < (-0.05) & roc_ukog > (-0.05),1,
         ifelse(Lag(roc_ukog) < (0.05) & roc_ukog > (0.05),-1,0)))
roc_ts[is.na(roc_ts)] <- 0

# Add trading signals using Stochastic Simulation (SMI)
smi_ts <- Lag(
  ifelse(Lag(smi_ukog[,1]) < Lag(smi_ukog[,2]) & smi_ukog[,1] > smi_ukog[,2],1, 
         ifelse(Lag(smi_ukog[,1]) > Lag(smi_ukog[,2]) & smi_ukog[,1] < smi_ukog[,2],-1,0)))
smi_ts[is.na(smi_ts)] <- 0

# Add Williams % R signals 
wpr_ts <- Lag(
  ifelse(Lag(wpr_ukog) > 0.8 & wpr_ukog < 0.8,1,
         ifelse(Lag(wpr_ukog) > 0.2 & wpr_ukog < 0.2,-1,0)))
wpr_ts[is.na(wpr_ts)] <- 0

# Creating Trading Strategies 
sma_strat <- ifelse(sma_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sma_strat[i] <- ifelse(sma_ts[i] == 1,1,ifelse(sma_ts[i] == -1,0,sma_strat[i-1]))
}
sma_strat[is.na(sma_strat)] <- 1
sma_stratcomp <- cbind(sma5, sma10, sma_ts, sma_strat)
colnames(sma_stratcomp) <- c('SMA(5)','SMA(10)','SMA SIGNAL','SMA POSITION')

# Parabolic Stop and Reverse (SAR)
sar_strat <- ifelse(sar_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sar_strat[i] <- ifelse(sar_ts[i] == 1,1,ifelse(sar_ts[i] == -1,0,sar_strat[i-1]))
}
sar_strat[is.na(sar_strat)] <- 1
sar_stratcomp <- cbind(Cl(UKOG.L), sar_ukog, sar_ts, sar_strat)
colnames(sar_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')

# Commodity Channel Index (CCI)
cci_strat <- ifelse(cci_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  cci_strat[i] <- ifelse(cci_ts[i] == 1,1,ifelse(cci_ts[i] == -1,0,cci_strat[i-1]))
}
cci_strat[is.na(cci_strat)] <- 1
cci_stratcomp <- cbind(cci_Ukog, cci_ts, cci_strat)
colnames(cci_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# Rate of Change ROC
roc_strat <- ifelse(roc_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  roc_strat[i] <- ifelse(roc_ts[i] == 1,1,ifelse(roc_ts[i] == -1,0,roc_strat[i-1]))
}
roc_strat[is.na(roc_strat)] <- 1
roc_stratcomp <- cbind(roc_ukog, roc_ts, roc_strat)
colnames(roc_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# Stochastic Momentum Index (SMI) 
smi_strat <- ifelse(smi_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  smi_strat[i] <- ifelse(smi_ts[i] == 1,1,ifelse(smi_ts[i] == -1,0,smi_strat[i-1]))
}
smi_strat[is.na(smi_strat)] <- 1
smi_stratcomp <- cbind(smi_ukog[,1],smi_ukog[,2],smi_ts,smi_strat)
colnames(smi_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# Williams % R
wpr_strat <- ifelse(wpr_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  wpr_strat[i] <- ifelse(wpr_ts[i] == 1,1,ifelse(wpr_ts[i] == -1,0,wpr_strat[i-1]))
}
wpr_strat[is.na(wpr_strat)] <- 1
wpr_stratcomp <- cbind(wpr_ukog, wpr_ts, wpr_strat)
colnames(wpr_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# Backtesting and Comparing result 
ret_ukog <- diff(log(Cl(UKOG.L)))

benchmark_ukog <- ret_ukog

# Simple Moving Average SMA
sma_ret <- ret_ukog*sma_strat
sma_ret_commission_adj <- ifelse((sma_ts == 1|sma_ts == -1) & sma_strat != Lag(sma_ts), (ret_ukog-0.05)*sma_strat, ret_ukog*sma_strat)
sma_comp <- cbind(sma_ret, sma_ret_commission_adj, benchmark_ukog)
colnames(sma_comp) <- c('SMA','SMA Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sma_comp, main = 'UKOG.L SMA Performance')
sma_comp_table <- table.AnnualizedReturns(sma_comp)

# Parabolic performance 
sar_ret <- ret_ukog*sar_strat
sar_ret_commission_adj <- ifelse((sar_ts == 1|sar_ts == -1) & sar_strat != Lag(sar_ts), (ret_ukog-0.05)*sar_strat, ret_ukog*sar_strat)
sar_comp <- cbind(sar_ret, sar_ret_commission_adj, benchmark_ukog)
colnames(sar_comp) <- c('SAR','SAR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(sar_comp, main = 'UKOG.L Parabolic SAR Performance')
sar_comp_table <- table.AnnualizedReturns(sar_comp)

# Commodity Channel Index (CCI)
cci_ret <- ret_ukog*cci_strat
cci_ret_commission_adj <- ifelse((cci_ts == 1|cci_ts == -1) & cci_strat != Lag(cci_ts), (ret_ukog-0.05)*cci_strat, ret_ukog*cci_strat)
cci_comp <- cbind(cci_ret, cci_ret_commission_adj, benchmark_ukog)
colnames(cci_comp) <- c('CCI','CCI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(cci_comp, main = 'UKOG.L CCI Performance')
cci_comp_table <- table.AnnualizedReturns(cci_comp)

# Rate of Change ROC 
roc_ret <- ret_ukog*roc_strat
roc_ret_commission_adj <- ifelse((roc_ts == 1|roc_ts == -1) & roc_strat != Lag(roc_ts), (ret_ukog-0.05)*roc_strat, ret_ukog*roc_strat)
roc_comp <- cbind(roc_ret, roc_ret_commission_adj, benchmark_ukog)
colnames(roc_comp) <- c('ROC','ROC Commission Adj','Apple Benchmark')
charts.PerformanceSummary(roc_comp, main = 'UKOG.L ROC Performance')
roc_comp_table <- table.AnnualizedReturns(roc_comp)

# Stochastic Momentum Index (SMI)
smi_ret <- ret_ukog*smi_strat
smi_ret_commission_adj <- ifelse((smi_ts == 1|smi_ts == -1) & smi_strat != Lag(smi_ts), (ret_ukog-0.05)*smi_strat, ret_ukog*smi_strat)
smi_comp <- cbind(smi_ret, smi_ret_commission_adj, benchmark_ukog)
colnames(smi_comp) <- c('SMI','SMI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(smi_comp, main = 'UKOG.L SMI Performance')
smi_comp_table <- table.AnnualizedReturns(smi_comp)

# Williams % R
wpr_ret <- ret_ukog*wpr_strat
wpr_ret_commission_adj <- ifelse((wpr_ts == 1|wpr_ts == -1) & wpr_strat != Lag(wpr_ts), (ret_ukog-0.05)*wpr_strat, ret_ukog*wpr_strat)
wpr_comp <- cbind(wpr_ret, wpr_ret_commission_adj, benchmark_ukog)
colnames(wpr_comp) <- c('WPR','WPR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(wpr_comp, main = 'UKOG.L WPR Performance')
wpr_comp_table <- table.AnnualizedReturns(wpr_comp)

# 5.7: Interpretation and Explanation of Model Performance 
# The UKOG Equity ETF models for trading strategy was performed using the Q2, Q3, and Q4 to estimate trading signals and evaluate performance. 
# The results of the final models suggest that the Q3 and Q4 UKOG data series for estimation and performance evaluations performed better with better 
# SMA, CCI, SAR and Williams %R indicators with positive signals mostly at the end of the chart series.

# 6 Category 2 Models: Just use 2 variables: the one you are trading, and an additional (called exogeneous variable) to enhance prediction
# We choose Bitcoin and Ethereum (ETH-USD) 

# 6.1 , 6.2, 6.3, 6.4 
# Import BTC closing prices for 2020

getSymbols(Symbols = 'BTC-USD', from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
BTCPrices <- (`BTC-USD`)[,4]
BTCPrices <- na.omit(BTCPrices)
plot( x = index( BTCPrices ), y = BTCPrices )
plot(BTCPrices, xlab = "Date", ylab = "BTCPrices Closing Price")

# Exogeneous variable closing prices for 2020
# # Ethereum (ETH-USD)
getSymbols(Symbols = "ETH-USD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
ETHPrices <- (`ETH-USD`)[,4]
ETHPrices <- na.omit(ETHPrices)
plot( x = index( ETHPrices ), y = ETHPrices )
plot(ETHPrices, xlab = "Date", ylab = "ETHPrices Closing Price")

# 6.5 The 1st strategy should include a form of VAR or VARMA.
# Q2 BTC
getSymbols(Symbols = 'BTC-USD', from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
BTCPrices <- (`BTC-USD`)[,4]
BTCPrices <- na.omit(BTCPrices)
plot( x = index( BTCPrices ), y = BTCPrices )
plot(BTCPrices, xlab = "Date", ylab = "BTCPrices Closing Price")

BTC_rt_Q2 = diff( log( BTCPrices ) )
BTC_rt_Q2 = BTC_rt_Q2[-1]
plot(BTC_rt_Q2, main="BTC  Q2 Return")

# Q2 ETH
getSymbols(Symbols = 'ETH-USD', from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
ETHPrices <- (`ETH-USD`)[,4]
ETHPrices <- na.omit(ETHPrices)
plot( x = index( ETHPrices ), y = ETHPrices )
plot(ETHPrices, xlab = "Date", ylab = "ETHPrices Closing Price")

ETH_rt_Q2 = diff( log( ETHPrices ) )
ETH_rt_Q2 = ETH_rt_Q2[-1]
plot(ETH_rt_Q2, main="ETH  Q2 Return")

# Estimation of VAR models
# Ruey S. Tsay - Multivariate Time Series Analysis_ With R and Financial Applications-Wiley (2013) p74
data <- cbind(BTC_rt_Q2,ETH_rt_Q2)
data <- na.omit(data)
data <- data[,1:2]
# Converting to matrix
data = data.matrix(as.data.frame(data))
zt=diffM(data)
m1=VAR(zt,1)
dim(data)

# Order Selection
m2=VARorder(zt,2)
names(m2)

# Results: All three criteria (AIC, BIC, HQ) indicate a VAR (2) serve as a starting model for the two dimensional series

# Multivariate Portmanteau statistics.
names(m1)
resi=m1$residuals ### Obtain the residuals of VAR(2) fit
resi
mq(resi,adj=10) ## adj is used to adjust the degrees of freedom. Ljung-Box Statistics:

# Model simplification.
m1=VAR(zt,1) # fit a un-constrained VAR(1) model
m2=refVAR(m1,thres=1.96) # Model refinement

# Results  The AIC of the simplified model is ???13.79, which is smaller than ???13.77 of the unconstrained model. 
# For this particular instance, all three criteria have a smaller value for the constrained model

# Model checking
MTSdiag(m2,adj=2)

# In conclusion, the simplified VAR(2) model in Equation (2.65) is adequate for the
# GDP growth rate series. The model can be written as p99

# Q2 for estimation (or prediction)
VARpred(m1,8,orig=0)
colMeans(zt) ## Compute sample means
# Sample standard errors
sqrt(apply(zt,2,var))

VARpred(m1,10,orig=0)[pred]

# Impulse response functions of a VAR model.
Phi = m2$Phi ### m2 is the simplified VAR(1) model
Sig = m2$Sigma
VARirf(Phi,Sig) ### Orthogonal innovations
VARirf(Phi,Sig,orth=F) ## Original innovations
# p117 Forecast error decomposition

# Q3 For evaluating performance

# 6.6 Multivariate GARCH https://stackoverflow.com/questions/35035857/multivariate-garch1-1-in-r
getSymbols(Symbols = 'BTC-USD', from = "2019-09-30", to = "2020-09-30" , src = "yahoo")
BTCPrices <- (`BTC-USD`)[,4]
BTCPrices <- na.omit(BTCPrices)
BTC_rt_Q12 = diff( log( BTCPrices ) )
BTC_rt_Q12 = BTC_rt_Q12[-1]

getSymbols(Symbols = "ETH-USD", from = "2019-09-30", to = "2020-09-30" , src = "yahoo")
ETHPrices <- (`ETH-USD`)[,4]
ETHPrices <- na.omit(ETHPrices)
ETH_rt_Q12 = diff( log( ETHPrices ) )
ETH_rt_Q12 = ETH_rt_Q12[-1]

data <- cbind(BTC_rt_Q2,ETH_rt_Q2)
data <- na.omit(data)
data <- data[,1:2]
# Converting to matrix
dataMGARCH = data.matrix(as.data.frame(data))
zt=diffM(dataMGARCH)

xspec = ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), distribution.model = 'norm')
uspec = multispec(replicate(2, xspec))
spec1 = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
spec1a = dccspec(uspec = uspec, dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')

cl = makePSOCKcluster(4)
multf = multifit(uspec, dataMGARCH, cluster = cl)

fit1 = dccfit(spec1, data = dataMGARCH, fit.control = list(eval.se = TRUE), fit = multf, cluster = cl)
fit_adcc = dccfit(spec1, data = dataMGARCH, fit.control = list(eval.se = TRUE), fit = multf, cluster = cl)
print(fit1)           
print(fit_adcc)

stopCluster(cl)

# 6.7 Cointegration

# 6.7.1 Using Q2 data: Test all the combinations of cointegration Engle-Granger

# Q2 Bitcoin 
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ2 = `BTC-USD`$`BTC-USD.Close`
bitQ2 <- na.omit( bitQ2 )

# Q2 Ethereum
getSymbols(Symbols = "ETH-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
`ETH-USD`<- na.omit( `ETH-USD` )
plot( x = index( `ETH-USD` ), y = `ETH-USD`$`ETH-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="Ethereum price")
ETHQ2 = `ETH-USD`$`ETH-USD.Close`
ETHQ2 <- na.omit( ETHQ2 )

#  6.7.2 Cointegration analysis of all 3 combinations using Engle-Granger | Checking if there any cointegrating vectors 

library(egcm)
library(urca)
library(tsDyn)

# checking if series are stationary and if found to be stationary,
# series can be defined to be cointegrated which implies that there exist some long-run relationship
# between the variable, and then we can estimate VECM. Then we can do innovation accounting (variance decomposition & 
# impulse response function)

# checking for Bitcoin
BTCQ2_cointegration <- ur.df(bitQ2, type = "none", selectlags = "AIC")
summary(BTCQ2_cointegration)
BTCQ2_cointegration@teststat
BTCQ2_cointegration@cval

# checking for Ethereum
ETHQ2_cointegration <- ur.df(ETHQ2, type = "none", selectlags = "AIC")
summary(ETHQ2_cointegration)
ETHQ2_cointegration@teststat
ETHQ2_cointegration@cval

# Interpretation 

# The critical value of Bitcoin Q2 at 5% is -1.95 while the statistic value is 0.7432714.
# The critical value of Ethereum Q2 at 5% is -1.95 while the statistic value is 1.028364.
# Since the critical values at 5% are less than the statistics, we fail to reject the null hypothesis.
# Thus, no co-integrating vectors are obsereved in Q2 series, thus we perform further cointegration anlysis below:

# 6.7.3 Now re-run the cointegration analysis of the combinations for Q2 using Engle-Grange
Q2_BTC_ETH_cointegrate <- yegcm("BTC-USD", "ETH-USD", start ="2020-04-01", end = "2020-06-30")
plot(Q2_BTC_ETH_cointegrate)

cointegration_model = cbind(bitQ2,ETHQ2)


# 6.7.4 Using Q3 data:Test all the 3 combinations of cointegration Engle-Granger
# Q3 Bitcoin 
getSymbols(Symbols = "BTC-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ3 = `BTC-USD`$`BTC-USD.Close`
bitQ3 <- na.omit( bitQ2 )

# Q2 Ethereum
getSymbols(Symbols = "ETH-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
`ETH-USD`<- na.omit( `ETH-USD` )
plot( x = index( `ETH-USD` ), y = `ETH-USD`$`ETH-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="Ethereum price")
ETHQ3 = `ETH-USD`$`ETH-USD.Close`
ETHQ3 <- na.omit( ETHQ2 )

#  6.7.5 Cointegration analysis of all 3 combinations using Engle-Granger | Checking if there any cointegrating vectors 

# checking if series are stationary and if found to be stationary,
# series can be defined to be cointegrated which implies that there exist some long-run relationship
# between the variable, and then we can estimate VECM. Then we can do innovation accounting (variance decomposition & 
# impulse response function)

# checking for Bitcoin
BTCQ3_cointegration <- ur.df(bitQ2, type = "none", selectlags = "AIC")
summary(BTCQ3_cointegration)
BTCQ3_cointegration@teststat
BTCQ3_cointegration@cval

# checking for Ethereum
ETHQ3_cointegration <- ur.df(ETHQ2, type = "none", selectlags = "AIC")
summary(ETHQ3_cointegration)
ETHQ3_cointegration@teststat
ETHQ3_cointegration@cval

# Interpretation 

# The critical value of Bitcoin Q3 at 5% is -1.95 while the statistic value is 0.5853.
# The critical value of Ethereum Q3 at 5% is -1.95 while the statistic value is 0.546527.
# Since the critical values at 5% are less than the statistics, we fail to reject the null hypothesis.
# Thus, no co-integrating vectors are obsereved in Q3 series, thus we perform further cointegration anlysis below:

# 6.7.6 Now re-run the cointegration analysis of the combinations for Q2 using Engle-Grange
Q3_BTC_ETH_cointegrate <- yegcm("BTC-USD", "ETH-USD", start ="2020-07-01", end = "2020-09-30")
plot(Q3_BTC_ETH_cointegrate)

cointegration_model = cbind(bitQ3,ETHQ3)

# 6.7.7 Johansen test for cointegration 
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
getSymbols(Symbols = "ETH-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")

cb = cbind( `BTC-USD`$`BTC-USD.Close`,`ETH-USD`$`ETH-USD.Close` )
cb = na.omit( cb )
cb
jotest=ca.jo( cb, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

getSymbols(Symbols = "BTC-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
getSymbols(Symbols = "ETH-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")

cb = cbind( `BTC-USD`$`BTC-USD.Close`,`ETH-USD`$`ETH-USD.Close` )
cb = na.omit( cb )
cb
jotest=ca.jo( cb, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)



# perform GARCH Modeling on UKOG Q4
getSymbols(Symbols = "UKOG.L", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ4 = UKOG.L$UKOG.L.Close
UKOGQ4 <- na.omit(UKOGQ4)
library(rugarch)
ukog_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(model = "sGARCH"),
                         distribution.model = 'norm')
gurcg_ukogq4 <- ugarchfit(spec =gld_gurch, data = UKOGQ4)
print(gurcg_ukogq4)
summary(gurcg_ukogq4)

# 5.5 and 5.6 Building trading strategy incorporating GARCH model results using Backtesting strategy 
# Using UKOG Q3 series for estimation and Q4 for performance evaluation 
# Retrieve Q3 Data 
getSymbols(Symbols = "UKOG.L", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ3 = UKOG.L$UKOG.L.Close
UKOGQ3 <- na.omit(UKOGQ3)

# create technical indicators: Simple moving averages: SMA using Q3 for estimate 
barChart(UKOGQ3, theme = chartTheme('black'))
sma5 <- SMA(UKOG.L$UKOG.L.Close, n=5)
sma10 <- SMA(UKOG.L$UKOG.L.Close, n=10)
lineChart(UKOG.L, theme = chartTheme('black'))
addSMA(n = 5, col = 'blue')
addSMA(n = 10, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('UKOG.L','SMA5','SMA10'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Apply parabolic Stop and Reverse (SAR) strategy 
sar_ukog <- SAR(cbind(Hi(UKOG.L),Lo(UKOG.L)), accel = c(0.02, 0.2))
barChart(UKOG.L, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')

# Apply Commodity Channeling Index (CCI): Here we have to pass on daily High , Low and Close Prices 
# along with a specified time period and a constant value
# So here we are going to take 20 days period and 0.015 as the constant value
cci_Ukog <- CCI(HLC(UKOG.L), n = 20, c = 0.015)
barChart(UKOG.L, theme = 'black')
addCCI(n = 20, c = 0.015)

# Now calculate the rate of chnage ROC and we will use 25-days as the period
roc_ukog <- ROC(UKOG.L$UKOG.L.Close, n = 25)
barChart(UKOG.L, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Add Stochastic momentum Index (SMI)
smi_ukog <- SMI(HLC(UKOG.L),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(UKOG.L, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# Add Williams % R  and we chose 14 days period
wpr_ukog <- WPR(HLC(UKOG.L), n = 14)
colnames(wpr_ukog) <- 'wpr'
barChart(UKOG.L, theme = 'black')
addWPR(n = 14)

# Create Trading Signals 
# The following code will create trading signals using SMA
# SMA 5 Crossover Signal 
# SMA 5 Crossover Signal 
sma5_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma5) & Cl(UKOG.L) > sma5,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma5) & Cl(UKOG.L) < sma5,-1,0)))
sma5_ts[is.na(sma5_ts)] <- 0
# SMA 10 Crossover Signal
sma10_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma10) & Cl(UKOG.L) > sma10,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma10) & Cl(UKOG.L) < sma10,-1,0)))
sma10_ts[is.na(sma10_ts)] <- 0
# SMA 5 and SMA 10 Crossover Signal
sma_ts <- Lag(
  ifelse(Lag(sma5) < Lag(sma10) & sma5 > sma10,1,
         ifelse(Lag(sma5) > Lag(sma10) & sma5 < sma10,-1,0)))
sma_ts[is.na(sma_ts)] <- 0

# Trading Signals using Parabolic and Reverse (SAR)
sar_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sar_ukog) & Cl(UKOG.L) > sar_ukog,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sar_ukog) & Cl(UKOG.L) < sar_ukog,-1,0)))
sar_ts[is.na(sar_ts)] <- 0

# Add trading signals using CCI 
cci_ts <- Lag(
  ifelse(Lag(cci_Ukog) < (-100) & cci_Ukog > (-100),1,
         ifelse(Lag(cci_Ukog) < (100) & cci_Ukog > (100),-1,0)))
cci_ts[is.na(cci_ts)] <- 0

# Add trading signals using rate of change ROC
roc_ts <- Lag(
  ifelse(Lag(roc_ukog) < (-0.05) & roc_ukog > (-0.05),1,
         ifelse(Lag(roc_ukog) < (0.05) & roc_ukog > (0.05),-1,0)))
roc_ts[is.na(roc_ts)] <- 0

# Add trading signals using Stochastic Simulation (SMI)
smi_ts <- Lag(
  ifelse(Lag(smi_ukog[,1]) < Lag(smi_ukog[,2]) & smi_ukog[,1] > smi_ukog[,2],1, 
         ifelse(Lag(smi_ukog[,1]) > Lag(smi_ukog[,2]) & smi_ukog[,1] < smi_ukog[,2],-1,0)))
smi_ts[is.na(smi_ts)] <- 0

# Add Williams % R signals 
wpr_ts <- Lag(
  ifelse(Lag(wpr_ukog) > 0.8 & wpr_ukog < 0.8,1,
         ifelse(Lag(wpr_ukog) > 0.2 & wpr_ukog < 0.2,-1,0)))
wpr_ts[is.na(wpr_ts)] <- 0

# Creating Trading Strategies 
sma_strat <- ifelse(sma_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sma_strat[i] <- ifelse(sma_ts[i] == 1,1,ifelse(sma_ts[i] == -1,0,sma_strat[i-1]))
}
sma_strat[is.na(sma_strat)] <- 1
sma_stratcomp <- cbind(sma5, sma10, sma_ts, sma_strat)
colnames(sma_stratcomp) <- c('SMA(5)','SMA(10)','SMA SIGNAL','SMA POSITION')

# Parabolic Stop and Reverse (SAR)
sar_strat <- ifelse(sar_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sar_strat[i] <- ifelse(sar_ts[i] == 1,1,ifelse(sar_ts[i] == -1,0,sar_strat[i-1]))
}
sar_strat[is.na(sar_strat)] <- 1
sar_stratcomp <- cbind(Cl(UKOG.L), sar_ukog, sar_ts, sar_strat)
colnames(sar_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')


# Commodity Channel Index (CCI)
cci_strat <- ifelse(cci_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  cci_strat[i] <- ifelse(cci_ts[i] == 1,1,ifelse(cci_ts[i] == -1,0,cci_strat[i-1]))
}
cci_strat[is.na(cci_strat)] <- 1
cci_stratcomp <- cbind(cci_Ukog, cci_ts, cci_strat)
colnames(cci_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# Rate of Change ROC
roc_strat <- ifelse(roc_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  roc_strat[i] <- ifelse(roc_ts[i] == 1,1,ifelse(roc_ts[i] == -1,0,roc_strat[i-1]))
}
roc_strat[is.na(roc_strat)] <- 1
roc_stratcomp <- cbind(roc_ukog, roc_ts, roc_strat)
colnames(roc_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# Stochastic Momentum Index (SMI) 
smi_strat <- ifelse(smi_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  smi_strat[i] <- ifelse(smi_ts[i] == 1,1,ifelse(smi_ts[i] == -1,0,smi_strat[i-1]))
}
smi_strat[is.na(smi_strat)] <- 1
smi_stratcomp <- cbind(smi_ukog[,1],smi_ukog[,2],smi_ts,smi_strat)
colnames(smi_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# Williams % R
wpr_strat <- ifelse(wpr_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  wpr_strat[i] <- ifelse(wpr_ts[i] == 1,1,ifelse(wpr_ts[i] == -1,0,wpr_strat[i-1]))
}
wpr_strat[is.na(wpr_strat)] <- 1
wpr_stratcomp <- cbind(wpr_ukog, wpr_ts, wpr_strat)
colnames(wpr_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# Backtesting and Comparing result 
ret_ukog <- diff(log(Cl(UKOG.L)))

benchmark_ukog <- ret_ukog


# Simple Moving Average SMA
sma_ret <- ret_ukog*sma_strat
sma_ret_commission_adj <- ifelse((sma_ts == 1|sma_ts == -1) & sma_strat != Lag(sma_ts), (ret_ukog-0.05)*sma_strat, ret_ukog*sma_strat)
sma_comp <- cbind(sma_ret, sma_ret_commission_adj, benchmark_ukog)
colnames(sma_comp) <- c('SMA','SMA Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sma_comp, main = 'UKOG.L SMA Performance')
sma_comp_table <- table.AnnualizedReturns(sma_comp)

# Parabolic performance 
sar_ret <- ret_ukog*sar_strat
sar_ret_commission_adj <- ifelse((sar_ts == 1|sar_ts == -1) & sar_strat != Lag(sar_ts), (ret_ukog-0.05)*sar_strat, ret_ukog*sar_strat)
sar_comp <- cbind(sar_ret, sar_ret_commission_adj, benchmark_ukog)
colnames(sar_comp) <- c('SAR','SAR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(sar_comp, main = 'UKOG.L Parabolic SAR Performance')
sar_comp_table <- table.AnnualizedReturns(sar_comp)

# Commodity Channel Index (CCI)
cci_ret <- ret_ukog*cci_strat
cci_ret_commission_adj <- ifelse((cci_ts == 1|cci_ts == -1) & cci_strat != Lag(cci_ts), (ret_ukog-0.05)*cci_strat, ret_ukog*cci_strat)
cci_comp <- cbind(cci_ret, cci_ret_commission_adj, benchmark_ukog)
colnames(cci_comp) <- c('CCI','CCI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(cci_comp, main = 'UKOG.L CCI Performance')
cci_comp_table <- table.AnnualizedReturns(cci_comp)

# Rate of Change ROC 
roc_ret <- ret_ukog*roc_strat
roc_ret_commission_adj <- ifelse((roc_ts == 1|roc_ts == -1) & roc_strat != Lag(roc_ts), (ret_ukog-0.05)*roc_strat, ret_ukog*roc_strat)
roc_comp <- cbind(roc_ret, roc_ret_commission_adj, benchmark_ukog)
colnames(roc_comp) <- c('ROC','ROC Commission Adj','Apple Benchmark')
charts.PerformanceSummary(roc_comp, main = 'UKOG.L ROC Performance')
roc_comp_table <- table.AnnualizedReturns(roc_comp)

# Stochastic Momentum Index (SMI)
smi_ret <- ret_ukog*smi_strat
smi_ret_commission_adj <- ifelse((smi_ts == 1|smi_ts == -1) & smi_strat != Lag(smi_ts), (ret_ukog-0.05)*smi_strat, ret_ukog*smi_strat)
smi_comp <- cbind(smi_ret, smi_ret_commission_adj, benchmark_ukog)
colnames(smi_comp) <- c('SMI','SMI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(smi_comp, main = 'UKOG.L SMI Performance')
smi_comp_table <- table.AnnualizedReturns(smi_comp)

# Williams % R
wpr_ret <- ret_ukog*wpr_strat
wpr_ret_commission_adj <- ifelse((wpr_ts == 1|wpr_ts == -1) & wpr_strat != Lag(wpr_ts), (ret_ukog-0.05)*wpr_strat, ret_ukog*wpr_strat)
wpr_comp <- cbind(wpr_ret, wpr_ret_commission_adj, benchmark_ukog)
colnames(wpr_comp) <- c('WPR','WPR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(wpr_comp, main = 'UKOG.L WPR Performance')
wpr_comp_table <- table.AnnualizedReturns(wpr_comp)


# Retrieve UKOG Q4 Data series | Evaluating Performance 
getSymbols(Symbols = "UKOG.L", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ4 = UKOG.L$UKOG.L.Close
UKOGQ4 <- na.omit(UKOGQ4)

# create technical indicators: Simple moving averages: SMA using Q4 for performance analysis  
barChart(UKOGQ4, theme = chartTheme('black'))
barChart(UKOGQ3, theme = chartTheme('black'))
sma5 <- SMA(UKOG.L$UKOG.L.Close, n=5)
sma10 <- SMA(UKOG.L$UKOG.L.Close, n=10)
lineChart(UKOG.L, theme = chartTheme('black'))
addSMA(n = 5, col = 'blue')
addSMA(n = 10, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('UKOG.L','SMA5','SMA10'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Apply parabolic Stop and Reverse (SAR) strategy 
sar_ukog <- SAR(cbind(Hi(UKOG.L),Lo(UKOG.L)), accel = c(0.02, 0.2))
barChart(UKOG.L, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')

# Apply Commodity Channeling Index (CCI): Here we have to pass on daily High , Low and Close Prices 
# along with a specified time period and a constant value
# So here we are going to take 5 days period and 0.015 as the constant value
cci_Ukog <- CCI(HLC(UKOG.L), n = 20, c = 0.015)
barChart(UKOG.L, theme = 'black')
addCCI(n = 20, c = 0.015)

# Now calculate the rate of change ROC and we will use 25-days as the period
roc_ukog <- ROC(UKOG.L$UKOG.L.Close, n = 25)
barChart(UKOG.L, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Add Stochastic momentum Index (SMI)
smi_ukog <- SMI(HLC(UKOG.L),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(UKOG.L, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# Add Williams % R  and we chose 14 days period
wpr_ukog <- WPR(HLC(UKOG.L), n = 14)
colnames(wpr_ukog) <- 'wpr'
barChart(UKOG.L, theme = 'black')
addWPR(n = 14)

# Create Trading Signals 
# The following code will create trading signals using SMA
# SMA 5 Crossover Signal 
# SMA 5 Crossover Signal 
sma5_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma5) & Cl(UKOG.L) > sma5,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma5) & Cl(UKOG.L) < sma5,-1,0)))
sma5_ts[is.na(sma5_ts)] <- 0
# SMA 10 Crossover Signal
sma10_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma10) & Cl(UKOG.L) > sma10,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma10) & Cl(UKOG.L) < sma10,-1,0)))
sma10_ts[is.na(sma10_ts)] <- 0
# SMA 5 and SMA 10 Crossover Signal
sma_ts <- Lag(
  ifelse(Lag(sma5) < Lag(sma10) & sma5 > sma10,1,
         ifelse(Lag(sma5) > Lag(sma10) & sma5 < sma10,-1,0)))
sma_ts[is.na(sma_ts)] <- 0

# Trading Signals using Parabolic and Reverse (SAR)
sar_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sar_ukog) & Cl(UKOG.L) > sar_ukog,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sar_ukog) & Cl(UKOG.L) < sar_ukog,-1,0)))
sar_ts[is.na(sar_ts)] <- 0

# Add trading signals using CCI 
cci_ts <- Lag(
  ifelse(Lag(cci_Ukog) < (-100) & cci_Ukog > (-100),1,
         ifelse(Lag(cci_Ukog) < (100) & cci_Ukog > (100),-1,0)))
cci_ts[is.na(cci_ts)] <- 0

# Add trading signals using rate of change ROC
roc_ts <- Lag(
  ifelse(Lag(roc_ukog) < (-0.05) & roc_ukog > (-0.05),1,
         ifelse(Lag(roc_ukog) < (0.05) & roc_ukog > (0.05),-1,0)))
roc_ts[is.na(roc_ts)] <- 0

# Add trading signals using Stochastic Simulation (SMI)
smi_ts <- Lag(
  ifelse(Lag(smi_ukog[,1]) < Lag(smi_ukog[,2]) & smi_ukog[,1] > smi_ukog[,2],1, 
         ifelse(Lag(smi_ukog[,1]) > Lag(smi_ukog[,2]) & smi_ukog[,1] < smi_ukog[,2],-1,0)))
smi_ts[is.na(smi_ts)] <- 0

# Add Williams % R signals 
wpr_ts <- Lag(
  ifelse(Lag(wpr_ukog) > 0.8 & wpr_ukog < 0.8,1,
         ifelse(Lag(wpr_ukog) > 0.2 & wpr_ukog < 0.2,-1,0)))
wpr_ts[is.na(wpr_ts)] <- 0

# Creating Trading Strategies 
sma_strat <- ifelse(sma_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sma_strat[i] <- ifelse(sma_ts[i] == 1,1,ifelse(sma_ts[i] == -1,0,sma_strat[i-1]))
}
sma_strat[is.na(sma_strat)] <- 1
sma_stratcomp <- cbind(sma5, sma10, sma_ts, sma_strat)
colnames(sma_stratcomp) <- c('SMA(5)','SMA(10)','SMA SIGNAL','SMA POSITION')

# Parabolic Stop and Reverse (SAR)
sar_strat <- ifelse(sar_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sar_strat[i] <- ifelse(sar_ts[i] == 1,1,ifelse(sar_ts[i] == -1,0,sar_strat[i-1]))
}
sar_strat[is.na(sar_strat)] <- 1
sar_stratcomp <- cbind(Cl(UKOG.L), sar_ukog, sar_ts, sar_strat)
colnames(sar_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')

# Commodity Channel Index (CCI)
cci_strat <- ifelse(cci_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  cci_strat[i] <- ifelse(cci_ts[i] == 1,1,ifelse(cci_ts[i] == -1,0,cci_strat[i-1]))
}
cci_strat[is.na(cci_strat)] <- 1
cci_stratcomp <- cbind(cci_Ukog, cci_ts, cci_strat)
colnames(cci_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# Rate of Change ROC
roc_strat <- ifelse(roc_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  roc_strat[i] <- ifelse(roc_ts[i] == 1,1,ifelse(roc_ts[i] == -1,0,roc_strat[i-1]))
}
roc_strat[is.na(roc_strat)] <- 1
roc_stratcomp <- cbind(roc_ukog, roc_ts, roc_strat)
colnames(roc_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# Stochastic Momentum Index (SMI) 
smi_strat <- ifelse(smi_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  smi_strat[i] <- ifelse(smi_ts[i] == 1,1,ifelse(smi_ts[i] == -1,0,smi_strat[i-1]))
}
smi_strat[is.na(smi_strat)] <- 1
smi_stratcomp <- cbind(smi_ukog[,1],smi_ukog[,2],smi_ts,smi_strat)
colnames(smi_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# Williams % R
wpr_strat <- ifelse(wpr_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  wpr_strat[i] <- ifelse(wpr_ts[i] == 1,1,ifelse(wpr_ts[i] == -1,0,wpr_strat[i-1]))
}
wpr_strat[is.na(wpr_strat)] <- 1
wpr_stratcomp <- cbind(wpr_ukog, wpr_ts, wpr_strat)
colnames(wpr_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# Backtesting and Comparing result 
ret_ukog <- diff(log(Cl(UKOG.L)))

benchmark_ukog <- ret_ukog

# Simple Moving Average SMA
sma_ret <- ret_ukog*sma_strat
sma_ret_commission_adj <- ifelse((sma_ts == 1|sma_ts == -1) & sma_strat != Lag(sma_ts), (ret_ukog-0.05)*sma_strat, ret_ukog*sma_strat)
sma_comp <- cbind(sma_ret, sma_ret_commission_adj, benchmark_ukog)
colnames(sma_comp) <- c('SMA','SMA Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sma_comp, main = 'UKOG.L SMA Performance')
sma_comp_table <- table.AnnualizedReturns(sma_comp)

# Parabolic performance 
sar_ret <- ret_ukog*sar_strat
sar_ret_commission_adj <- ifelse((sar_ts == 1|sar_ts == -1) & sar_strat != Lag(sar_ts), (ret_ukog-0.05)*sar_strat, ret_ukog*sar_strat)
sar_comp <- cbind(sar_ret, sar_ret_commission_adj, benchmark_ukog)
colnames(sar_comp) <- c('SAR','SAR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(sar_comp, main = 'UKOG.L Parabolic SAR Performance')
sar_comp_table <- table.AnnualizedReturns(sar_comp)

# Commodity Channel Index (CCI)
cci_ret <- ret_ukog*cci_strat
cci_ret_commission_adj <- ifelse((cci_ts == 1|cci_ts == -1) & cci_strat != Lag(cci_ts), (ret_ukog-0.05)*cci_strat, ret_ukog*cci_strat)
cci_comp <- cbind(cci_ret, cci_ret_commission_adj, benchmark_ukog)
colnames(cci_comp) <- c('CCI','CCI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(cci_comp, main = 'UKOG.L CCI Performance')
cci_comp_table <- table.AnnualizedReturns(cci_comp)

# Rate of Change ROC 
roc_ret <- ret_ukog*roc_strat
roc_ret_commission_adj <- ifelse((roc_ts == 1|roc_ts == -1) & roc_strat != Lag(roc_ts), (ret_ukog-0.05)*roc_strat, ret_ukog*roc_strat)
roc_comp <- cbind(roc_ret, roc_ret_commission_adj, benchmark_ukog)
colnames(roc_comp) <- c('ROC','ROC Commission Adj','Apple Benchmark')
charts.PerformanceSummary(roc_comp, main = 'UKOG.L ROC Performance')
roc_comp_table <- table.AnnualizedReturns(roc_comp)

# Stochastic Momentum Index (SMI)
smi_ret <- ret_ukog*smi_strat
smi_ret_commission_adj <- ifelse((smi_ts == 1|smi_ts == -1) & smi_strat != Lag(smi_ts), (ret_ukog-0.05)*smi_strat, ret_ukog*smi_strat)
smi_comp <- cbind(smi_ret, smi_ret_commission_adj, benchmark_ukog)
colnames(smi_comp) <- c('SMI','SMI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(smi_comp, main = 'UKOG.L SMI Performance')
smi_comp_table <- table.AnnualizedReturns(smi_comp)

# Williams % R
wpr_ret <- ret_ukog*wpr_strat
wpr_ret_commission_adj <- ifelse((wpr_ts == 1|wpr_ts == -1) & wpr_strat != Lag(wpr_ts), (ret_ukog-0.05)*wpr_strat, ret_ukog*wpr_strat)
wpr_comp <- cbind(wpr_ret, wpr_ret_commission_adj, benchmark_ukog)
colnames(wpr_comp) <- c('WPR','WPR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(wpr_comp, main = 'UKOG.L WPR Performance')
wpr_comp_table <- table.AnnualizedReturns(wpr_comp)

# 5.7: Interpretation and Explanation of Model Performance 
# The UKOG Equity ETF models for trading strategy was performed using the Q2, Q3, and Q4 to estimate trading signals and evaluate performance. 
# The results of the final models suggest that the Q3 and Q4 UKOG data series for estimation and performance evaluations performed better with better 
# SMA, CCI, SAR and Williams %R indicators with positive signals mostly at the end of the chart series.

# 6 Category 2 Models: Just use 2 variables: the one you are trading, and an additional (called exogeneous variable) to enhance prediction
# We choose Bitcoin and Ethereum (ETH-USD) 

# 6.1 , 6.2, 6.3, 6.4 
# Import BTC closing prices for 2020

getSymbols(Symbols = 'BTC-USD', from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
BTCPrices <- (`BTC-USD`)[,4]
BTCPrices <- na.omit(BTCPrices)
plot( x = index( BTCPrices ), y = BTCPrices )
plot(BTCPrices, xlab = "Date", ylab = "BTCPrices Closing Price")

# Exogeneous variable closing prices for 2020
# # Ethereum (ETH-USD)
getSymbols(Symbols = "ETH-USD", from = "2020-01-01", to = "2020-12-31" , src = "yahoo")
ETHPrices <- (`ETH-USD`)[,4]
ETHPrices <- na.omit(ETHPrices)
plot( x = index( ETHPrices ), y = ETHPrices )
plot(ETHPrices, xlab = "Date", ylab = "ETHPrices Closing Price")

# 6.5 The 1st strategy should include a form of VAR or VARMA.
# Q2 BTC
getSymbols(Symbols = 'BTC-USD', from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
BTCPrices <- (`BTC-USD`)[,4]
BTCPrices <- na.omit(BTCPrices)
plot( x = index( BTCPrices ), y = BTCPrices )
plot(BTCPrices, xlab = "Date", ylab = "BTCPrices Closing Price")

BTC_rt_Q2 = diff( log( BTCPrices ) )
BTC_rt_Q2 = BTC_rt_Q2[-1]
plot(BTC_rt_Q2, main="BTC  Q2 Return")

# Q2 ETH
getSymbols(Symbols = 'ETH-USD', from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
ETHPrices <- (`ETH-USD`)[,4]
ETHPrices <- na.omit(ETHPrices)
plot( x = index( ETHPrices ), y = ETHPrices )
plot(ETHPrices, xlab = "Date", ylab = "ETHPrices Closing Price")

ETH_rt_Q2 = diff( log( ETHPrices ) )
ETH_rt_Q2 = ETH_rt_Q2[-1]
plot(ETH_rt_Q2, main="ETH  Q2 Return")

# Estimation of VAR models
# Ruey S. Tsay - Multivariate Time Series Analysis_ With R and Financial Applications-Wiley (2013) p74
data <- cbind(BTC_rt_Q2,ETH_rt_Q2)
data <- na.omit(data)
data <- data[,1:2]
# Converting to matrix
data = data.matrix(as.data.frame(data))
zt=diffM(data)
m1=VAR(zt,1)
dim(data)

# Order Selection
m2=VARorder(zt,2)
names(m2)

# Results: All three criteria (AIC, BIC, HQ) indicate a VAR (2) serve as a starting model for the two dimensional series

# Multivariate Portmanteau statistics.
names(m1)
resi=m1$residuals ### Obtain the residuals of VAR(2) fit
resi
mq(resi,adj=10) ## adj is used to adjust the degrees of freedom. Ljung-Box Statistics:

# Model simplification.
m1=VAR(zt,1) # fit a un-constrained VAR(1) model
m2=refVAR(m1,thres=1.96) # Model refinement

# Results  The AIC of the simplified model is ???13.79, which is smaller than ???13.77 of the unconstrained model. 
# For this particular instance, all three criteria have a smaller value for the constrained model

# Model checking
MTSdiag(m2,adj=2)

# In conclusion, the simplified VAR(2) model in Equation (2.65) is adequate for the
# GDP growth rate series. The model can be written as p99

# Q2 for estimation (or prediction)
VARpred(m1,8,orig=0)
colMeans(zt) ## Compute sample means
# Sample standard errors
sqrt(apply(zt,2,var))

VARpred(m1,10,orig=0)[pred]

# Impulse response functions of a VAR model.
Phi = m2$Phi ### m2 is the simplified VAR(1) model
Sig = m2$Sigma
VARirf(Phi,Sig) ### Orthogonal innovations
VARirf(Phi,Sig,orth=F) ## Original innovations
# p117 Forecast error decomposition

# Q3 For evaluating performance

# 6.6 Multivariate GARCH https://stackoverflow.com/questions/35035857/multivariate-garch1-1-in-r
getSymbols(Symbols = 'BTC-USD', from = "2019-09-30", to = "2020-09-30" , src = "yahoo")
BTCPrices <- (`BTC-USD`)[,4]
BTCPrices <- na.omit(BTCPrices)
BTC_rt_Q12 = diff( log( BTCPrices ) )
BTC_rt_Q12 = BTC_rt_Q12[-1]

getSymbols(Symbols = "ETH-USD", from = "2019-09-30", to = "2020-09-30" , src = "yahoo")
ETHPrices <- (`ETH-USD`)[,4]
ETHPrices <- na.omit(ETHPrices)
ETH_rt_Q12 = diff( log( ETHPrices ) )
ETH_rt_Q12 = ETH_rt_Q12[-1]

data <- cbind(BTC_rt_Q2,ETH_rt_Q2)
data <- na.omit(data)
data <- data[,1:2]
# Converting to matrix
dataMGARCH = data.matrix(as.data.frame(data))
zt=diffM(dataMGARCH)

xspec = ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), distribution.model = 'norm')
uspec = multispec(replicate(2, xspec))
spec1 = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
spec1a = dccspec(uspec = uspec, dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')

cl = makePSOCKcluster(4)
multf = multifit(uspec, dataMGARCH, cluster = cl)

fit1 = dccfit(spec1, data = dataMGARCH, fit.control = list(eval.se = TRUE), fit = multf, cluster = cl)
fit_adcc = dccfit(spec1, data = dataMGARCH, fit.control = list(eval.se = TRUE), fit = multf, cluster = cl)
print(fit1)           
print(fit_adcc)

stopCluster(cl)

# 6.7 Cointegration

# 6.7.1 Using Q2 data: Test all the combinations of cointegration Engle-Granger

# Q2 Bitcoin 
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ2 = `BTC-USD`$`BTC-USD.Close`
bitQ2 <- na.omit( bitQ2 )

# Q2 Ethereum
getSymbols(Symbols = "ETH-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
`ETH-USD`<- na.omit( `ETH-USD` )
plot( x = index( `ETH-USD` ), y = `ETH-USD`$`ETH-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="Ethereum price")
ETHQ2 = `ETH-USD`$`ETH-USD.Close`
ETHQ2 <- na.omit( ETHQ2 )

#  6.7.2 Cointegration analysis of all 3 combinations using Engle-Granger | Checking if there any cointegrating vectors 

library(egcm)
library(urca)
library(tsDyn)

# checking if series are stationary and if found to be stationary,
# series can be defined to be cointegrated which implies that there exist some long-run relationship
# between the variable, and then we can estimate VECM. Then we can do innovation accounting (variance decomposition & 
# impulse response function)

# checking for Bitcoin
BTCQ2_cointegration <- ur.df(bitQ2, type = "none", selectlags = "AIC")
summary(BTCQ2_cointegration)
BTCQ2_cointegration@teststat
BTCQ2_cointegration@cval

# checking for Ethereum
ETHQ2_cointegration <- ur.df(ETHQ2, type = "none", selectlags = "AIC")
summary(ETHQ2_cointegration)
ETHQ2_cointegration@teststat
ETHQ2_cointegration@cval

# Interpretation 

# The critical value of Bitcoin Q2 at 5% is -1.95 while the statistic value is 0.7432714.
# The critical value of Ethereum Q2 at 5% is -1.95 while the statistic value is 1.028364.
# Since the critical values at 5% are less than the statistics, we fail to reject the null hypothesis.
# Thus, no co-integrating vectors are obsereved in Q2 series, thus we perform further cointegration anlysis below:

# 6.7.3 Now re-run the cointegration analysis of the combinations for Q2 using Engle-Grange
Q2_BTC_ETH_cointegrate <- yegcm("BTC-USD", "ETH-USD", start ="2020-04-01", end = "2020-06-30")
plot(Q2_BTC_ETH_cointegrate)

cointegration_model = cbind(bitQ2,ETHQ2)


# 6.7.4 Using Q3 data:Test all the 3 combinations of cointegration Engle-Granger
# Q3 Bitcoin 
getSymbols(Symbols = "BTC-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
`BTC-USD`<- na.omit( `BTC-USD` )
plot( x = index( `BTC-USD` ), y = `BTC-USD`$`BTC-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="bitcoin price")
bitQ3 = `BTC-USD`$`BTC-USD.Close`
bitQ3 <- na.omit( bitQ2 )

# Q2 Ethereum
getSymbols(Symbols = "ETH-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
`ETH-USD`<- na.omit( `ETH-USD` )
plot( x = index( `ETH-USD` ), y = `ETH-USD`$`ETH-USD.Close`, type="l", xlab="Time", ylab="Closing price ($)", main="Ethereum price")
ETHQ3 = `ETH-USD`$`ETH-USD.Close`
ETHQ3 <- na.omit( ETHQ2 )

#  6.7.5 Cointegration analysis of all 3 combinations using Engle-Granger | Checking if there any cointegrating vectors 

# checking if series are stationary and if found to be stationary,
# series can be defined to be cointegrated which implies that there exist some long-run relationship
# between the variable, and then we can estimate VECM. Then we can do innovation accounting (variance decomposition & 
# impulse response function)

# checking for Bitcoin
BTCQ3_cointegration <- ur.df(bitQ2, type = "none", selectlags = "AIC")
summary(BTCQ3_cointegration)
BTCQ3_cointegration@teststat
BTCQ3_cointegration@cval

# checking for Ethereum
ETHQ3_cointegration <- ur.df(ETHQ2, type = "none", selectlags = "AIC")
summary(ETHQ3_cointegration)
ETHQ3_cointegration@teststat
ETHQ3_cointegration@cval

# Interpretation 

# The critical value of Bitcoin Q3 at 5% is -1.95 while the statistic value is 0.5853.
# The critical value of Ethereum Q3 at 5% is -1.95 while the statistic value is 0.546527.
# Since the critical values at 5% are less than the statistics, we fail to reject the null hypothesis.
# Thus, no co-integrating vectors are obsereved in Q3 series, thus we perform further cointegration anlysis below:

# 6.7.6 Now re-run the cointegration analysis of the combinations for Q2 using Engle-Grange
Q3_BTC_ETH_cointegrate <- yegcm("BTC-USD", "ETH-USD", start ="2020-07-01", end = "2020-09-30")
plot(Q3_BTC_ETH_cointegrate)

cointegration_model = cbind(bitQ3,ETHQ3)

# 6.7.7 Johansen test for cointegration 
getSymbols(Symbols = "BTC-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")
getSymbols(Symbols = "ETH-USD", from = "2020-04-01", to = "2020-06-30" , src = "yahoo")

cb = cbind( `BTC-USD`$`BTC-USD.Close`,`ETH-USD`$`ETH-USD.Close` )
cb = na.omit( cb )
cb
jotest=ca.jo( cb, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

getSymbols(Symbols = "BTC-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")
getSymbols(Symbols = "ETH-USD", from = "2020-07-01", to = "2020-09-30" , src = "yahoo")

cb = cbind( `BTC-USD`$`BTC-USD.Close`,`ETH-USD`$`ETH-USD.Close` )
cb = na.omit( cb )
cb
jotest=ca.jo( cb, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

# Overall model evaluation and Interpretation 
# 7.8: Interpretation and Explanation of Model Performance 
# The UKOG Equity ETF models for trading strategy was performed using the Q2, Q3, and Q4 to estimate trading signals and evaluate performance. 
# The results of the final models suggest that the Q3 and Q4 UKOG data series for estimation and performance evaluations performed better with better 
# SMA, CCI, SAR and Williams %R indicators with positive signals mostly at the end of the chart series.

# Question 8: Model with Q4 appears to be the model that performed better the most with assigned relative weights, SMA, CCI, Williams %R, ROC and SAR indicators
getSymbols(Symbols = "UKOG.L", from = "2020-10-01", to = "2020-12-31" , src = "yahoo")
plot( x = index(UKOG.L), y = UKOG.L$UKOG.L.Close, type="l", xlab="Time", ylab="Closing price ($)", main="UKOG.L ETF price")
UKOGQ4 = UKOG.L$UKOG.L.Close
UKOGQ4 <- na.omit(UKOGQ4)

# create technical indicators: Simple moving averages: SMA using Q4 for performance analysis  
barChart(UKOGQ4, theme = chartTheme('black'))
barChart(UKOGQ3, theme = chartTheme('black'))
sma5 <- SMA(UKOG.L$UKOG.L.Close, n=5)
sma10 <- SMA(UKOG.L$UKOG.L.Close, n=10)
lineChart(UKOG.L, theme = chartTheme('black'))
addSMA(n = 5, col = 'blue')
addSMA(n = 10, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('UKOG.L','SMA5','SMA10'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Apply parabolic Stop and Reverse (SAR) strategy 
sar_ukog <- SAR(cbind(Hi(UKOG.L),Lo(UKOG.L)), accel = c(0.02, 0.2))
barChart(UKOG.L, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')

# Apply Commodity Channeling Index (CCI): Here we have to pass on daily High , Low and Close Prices 
# along with a specified time period and a constant value
# So here we are going to take 5 days period and 0.015 as the constant value
cci_Ukog <- CCI(HLC(UKOG.L), n = 20, c = 0.015)
barChart(UKOG.L, theme = 'black')
addCCI(n = 20, c = 0.015)

# Now calculate the rate of change ROC and we will use 25-days as the period
roc_ukog <- ROC(UKOG.L$UKOG.L.Close, n = 25)
barChart(UKOG.L, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# Add Stochastic momentum Index (SMI)
smi_ukog <- SMI(HLC(UKOG.L),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(UKOG.L, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# Add Williams % R  and we chose 14 days period
wpr_ukog <- WPR(HLC(UKOG.L), n = 14)
colnames(wpr_ukog) <- 'wpr'
barChart(UKOG.L, theme = 'black')
addWPR(n = 14)

# Create Trading Signals 
# The following code will create trading signals using SMA
# SMA 5 Crossover Signal 
# SMA 5 Crossover Signal 
sma5_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma5) & Cl(UKOG.L) > sma5,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma5) & Cl(UKOG.L) < sma5,-1,0)))
sma5_ts[is.na(sma5_ts)] <- 0
# SMA 10 Crossover Signal
sma10_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sma10) & Cl(UKOG.L) > sma10,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sma10) & Cl(UKOG.L) < sma10,-1,0)))
sma10_ts[is.na(sma10_ts)] <- 0
# SMA 5 and SMA 10 Crossover Signal
sma_ts <- Lag(
  ifelse(Lag(sma5) < Lag(sma10) & sma5 > sma10,1,
         ifelse(Lag(sma5) > Lag(sma10) & sma5 < sma10,-1,0)))
sma_ts[is.na(sma_ts)] <- 0

# Trading Signals using Parabolic and Reverse (SAR)
sar_ts <- Lag(
  ifelse(Lag(Cl(UKOG.L)) < Lag(sar_ukog) & Cl(UKOG.L) > sar_ukog,1,
         ifelse(Lag(Cl(UKOG.L)) > Lag(sar_ukog) & Cl(UKOG.L) < sar_ukog,-1,0)))
sar_ts[is.na(sar_ts)] <- 0

# Add trading signals using CCI 
cci_ts <- Lag(
  ifelse(Lag(cci_Ukog) < (-100) & cci_Ukog > (-100),1,
         ifelse(Lag(cci_Ukog) < (100) & cci_Ukog > (100),-1,0)))
cci_ts[is.na(cci_ts)] <- 0

# Add trading signals using rate of change ROC
roc_ts <- Lag(
  ifelse(Lag(roc_ukog) < (-0.05) & roc_ukog > (-0.05),1,
         ifelse(Lag(roc_ukog) < (0.05) & roc_ukog > (0.05),-1,0)))
roc_ts[is.na(roc_ts)] <- 0

# Add trading signals using Stochastic Simulation (SMI)
smi_ts <- Lag(
  ifelse(Lag(smi_ukog[,1]) < Lag(smi_ukog[,2]) & smi_ukog[,1] > smi_ukog[,2],1, 
         ifelse(Lag(smi_ukog[,1]) > Lag(smi_ukog[,2]) & smi_ukog[,1] < smi_ukog[,2],-1,0)))
smi_ts[is.na(smi_ts)] <- 0

# Add Williams % R signals 
wpr_ts <- Lag(
  ifelse(Lag(wpr_ukog) > 0.8 & wpr_ukog < 0.8,1,
         ifelse(Lag(wpr_ukog) > 0.2 & wpr_ukog < 0.2,-1,0)))
wpr_ts[is.na(wpr_ts)] <- 0

# Creating Trading Strategies 
sma_strat <- ifelse(sma_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sma_strat[i] <- ifelse(sma_ts[i] == 1,1,ifelse(sma_ts[i] == -1,0,sma_strat[i-1]))
}
sma_strat[is.na(sma_strat)] <- 1
sma_stratcomp <- cbind(sma5, sma10, sma_ts, sma_strat)
colnames(sma_stratcomp) <- c('SMA(5)','SMA(10)','SMA SIGNAL','SMA POSITION')

# Parabolic Stop and Reverse (SAR)
sar_strat <- ifelse(sar_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  sar_strat[i] <- ifelse(sar_ts[i] == 1,1,ifelse(sar_ts[i] == -1,0,sar_strat[i-1]))
}
sar_strat[is.na(sar_strat)] <- 1
sar_stratcomp <- cbind(Cl(UKOG.L), sar_ukog, sar_ts, sar_strat)
colnames(sar_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')

# Commodity Channel Index (CCI)
cci_strat <- ifelse(cci_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  cci_strat[i] <- ifelse(cci_ts[i] == 1,1,ifelse(cci_ts[i] == -1,0,cci_strat[i-1]))
}
cci_strat[is.na(cci_strat)] <- 1
cci_stratcomp <- cbind(cci_Ukog, cci_ts, cci_strat)
colnames(cci_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# Rate of Change ROC
roc_strat <- ifelse(roc_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  roc_strat[i] <- ifelse(roc_ts[i] == 1,1,ifelse(roc_ts[i] == -1,0,roc_strat[i-1]))
}
roc_strat[is.na(roc_strat)] <- 1
roc_stratcomp <- cbind(roc_ukog, roc_ts, roc_strat)
colnames(roc_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# Stochastic Momentum Index (SMI) 
smi_strat <- ifelse(smi_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  smi_strat[i] <- ifelse(smi_ts[i] == 1,1,ifelse(smi_ts[i] == -1,0,smi_strat[i-1]))
}
smi_strat[is.na(smi_strat)] <- 1
smi_stratcomp <- cbind(smi_ukog[,1],smi_ukog[,2],smi_ts,smi_strat)
colnames(smi_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# Williams % R
wpr_strat <- ifelse(wpr_ts > 1,0,1)
for (i in 1 : length(Cl(UKOG.L))) {
  wpr_strat[i] <- ifelse(wpr_ts[i] == 1,1,ifelse(wpr_ts[i] == -1,0,wpr_strat[i-1]))
}
wpr_strat[is.na(wpr_strat)] <- 1
wpr_stratcomp <- cbind(wpr_ukog, wpr_ts, wpr_strat)
colnames(wpr_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# Backtesting and Comparing result 
ret_ukog <- diff(log(Cl(UKOG.L)))

benchmark_ukog <- ret_ukog

# Simple Moving Average SMA
sma_ret <- ret_ukog*sma_strat
sma_ret_commission_adj <- ifelse((sma_ts == 1|sma_ts == -1) & sma_strat != Lag(sma_ts), (ret_ukog-0.05)*sma_strat, ret_ukog*sma_strat)
sma_comp <- cbind(sma_ret, sma_ret_commission_adj, benchmark_ukog)
colnames(sma_comp) <- c('SMA','SMA Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sma_comp, main = 'UKOG.L SMA Performance')
sma_comp_table <- table.AnnualizedReturns(sma_comp)

# Parabolic performance 
sar_ret <- ret_ukog*sar_strat
sar_ret_commission_adj <- ifelse((sar_ts == 1|sar_ts == -1) & sar_strat != Lag(sar_ts), (ret_ukog-0.05)*sar_strat, ret_ukog*sar_strat)
sar_comp <- cbind(sar_ret, sar_ret_commission_adj, benchmark_ukog)
colnames(sar_comp) <- c('SAR','SAR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(sar_comp, main = 'UKOG.L Parabolic SAR Performance')
sar_comp_table <- table.AnnualizedReturns(sar_comp)

# Commodity Channel Index (CCI)
cci_ret <- ret_ukog*cci_strat
cci_ret_commission_adj <- ifelse((cci_ts == 1|cci_ts == -1) & cci_strat != Lag(cci_ts), (ret_ukog-0.05)*cci_strat, ret_ukog*cci_strat)
cci_comp <- cbind(cci_ret, cci_ret_commission_adj, benchmark_ukog)
colnames(cci_comp) <- c('CCI','CCI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(cci_comp, main = 'UKOG.L CCI Performance')
cci_comp_table <- table.AnnualizedReturns(cci_comp)

# Rate of Change ROC 
roc_ret <- ret_ukog*roc_strat
roc_ret_commission_adj <- ifelse((roc_ts == 1|roc_ts == -1) & roc_strat != Lag(roc_ts), (ret_ukog-0.05)*roc_strat, ret_ukog*roc_strat)
roc_comp <- cbind(roc_ret, roc_ret_commission_adj, benchmark_ukog)
colnames(roc_comp) <- c('ROC','ROC Commission Adj','Apple Benchmark')
charts.PerformanceSummary(roc_comp, main = 'UKOG.L ROC Performance')
roc_comp_table <- table.AnnualizedReturns(roc_comp)

# Stochastic Momentum Index (SMI)
smi_ret <- ret_ukog*smi_strat
smi_ret_commission_adj <- ifelse((smi_ts == 1|smi_ts == -1) & smi_strat != Lag(smi_ts), (ret_ukog-0.05)*smi_strat, ret_ukog*smi_strat)
smi_comp <- cbind(smi_ret, smi_ret_commission_adj, benchmark_ukog)
colnames(smi_comp) <- c('SMI','SMI Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(smi_comp, main = 'UKOG.L SMI Performance')
smi_comp_table <- table.AnnualizedReturns(smi_comp)

# Williams % R
wpr_ret <- ret_ukog*wpr_strat
wpr_ret_commission_adj <- ifelse((wpr_ts == 1|wpr_ts == -1) & wpr_strat != Lag(wpr_ts), (ret_ukog-0.05)*wpr_strat, ret_ukog*wpr_strat)
wpr_comp <- cbind(wpr_ret, wpr_ret_commission_adj, benchmark_ukog)
colnames(wpr_comp) <- c('WPR','WPR Commission Adj','UKOG.L Benchmark')
charts.PerformanceSummary(wpr_comp, main = 'UKOG.L WPR Performance')
wpr_comp_table <- table.AnnualizedReturns(wpr_comp)

# 8: Interpretation 
#Model with Q4 appears to be the model that performed better the most with assigned relative weights, SMA, CCI, Williams %R, ROC and SAR indicators.
# As observed from the chart series, there appears to be positive indicators towards the end of Q4 UKOG.L Equity ETF time series.



