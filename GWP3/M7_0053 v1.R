# building trading strategy during COVID-19
# Selecting a GOLD ETF, Equity EFT outside the USA and Bitcoin

install.packages("egcm")
install.packages("evir") 
install.packages('extRemes')
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

# load package 
library(quantmod) 
library(ggplot2)
library(tidyverse)
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

# 5.4 Building 2 trading strategies using the above extracted data series for UKOG Equity ETF Q2 & Q3; Q3 & Q4

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


