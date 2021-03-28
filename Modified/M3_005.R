library(quantmod) 
# 1.1 Obtain Treasury yield data
t2yr = getSymbols(Symbols = "DGS2", src = "FRED", auto.assign = FALSE)
t2yr = t2yr["2019-10/2019-11"]
t2yr
plot(x = index(t2yr),
     y = t2yr$DGS2,
     xlab = "Date",
     ylab = "Yield (%)",
     type = "l",
     col = "red",
     main = "10-Year US Treasury Yields")

plot(x = index(t2yr),
     y = t2yr$DGS2,
     xlab = "Date",
     ylab = "Yield (%)",
     type = "l",
     col = "red",
     main = "10-Year US Treasury Yields")

t3yr = getSymbols(Symbols = "DGS3", src = "FRED", auto.assign = FALSE)
t3yr = t3yr["2019-10/2019-11"]
t5yr = getSymbols(Symbols = "DGS5", src = "FRED", auto.assign = FALSE)
t5yr = t5yr["2019-10/2019-11"]
t7yr = getSymbols(Symbols = "DGS7", src = "FRED", auto.assign = FALSE)
t7yr = t7yr["2019-10/2019-11"]
t10yr = getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
t10yr = t10yr["2019-10/2019-11"]
t30yr = getSymbols(Symbols = "DGS30", src = "FRED", auto.assign = FALSE)
t30yr = t30yr["2019-10/2019-11"]
plot( x = index( t3yr ), y = t3yr$DGS3 )
plot( x = index( t5yr ), y = t5yr$DGS5 )
plot( x = index( t7yr ), y = t7yr$DGS7 )
plot( x = index( t10yr ), y = t10yr$DGS10 )
plot( x = index( t30yr ), y = t30yr$DGS30 )

# 1.2 import GLD ETF
getSymbols(Symbols = "GLD", from = "2019-10-01", to = "2019-11-30" , src = "yahoo")
Gld_Prices <- GLD$GLD.Close
plot( x = index( GLD ), y = Gld_Prices )

# 1.3 import my ETF
# U.K Oil and Gas Investment PLC (UKOG.L)
getSymbols(Symbols = "UKOG.L", from = "2019-10-01", to = "2019-11-30" , src = "yahoo")
UKOG.L
Ukog_prices <- UKOG.L$UKOG.L.Close

plot( x = index( UKOG.L ), y = Ukog_prices )

# 2.1 calcualte the log returns
GLD$GLD.Close
gldlog = diff( log( GLD$GLD.Close ) )
gldlog
#gldlog2 = na.omit( gldlog )
gldlog2 = gldlog[-1]
gldlog2
plot(gldlog2)

UKOG.L$UKOG.L.Close
ukoglog = diff( log( UKOG.L$UKOG.L.Close ) )
ukoglog
#ukoglog2 = na.omit(ukoglog )
ukoglog2 = ukoglog[-1]
ukoglog2
plot(ukoglog2)

# 3.1 calculate benchmark security average yield for Oct and Nov

#t2yr = na.omit( t2yr )
t2yravr1 = mean( t2yr$DGS2["2019-10"] )
t2yravr1
t2yravr2 = mean( t2yr$DGS2["2019-11"] )
t2yravr2

#t3yr = na.omit( t3yr )
t3yravr1 = mean( t3yr$DGS3["2019-10"] )
t3yravr1
t3yravr2 = mean( t3yr$DGS3["2019-11"] )
t3yravr2

#t5yr = na.omit( t5yr )
t5yravr1 = mean( t5yr$DGS5["2019-10"] )
t5yravr1
t5yravr2 = mean( t5yr$DGS5["2019-11"] )
t5yravr2

#t7yr = na.omit( t7yr )
t7yravr1 = mean( t7yr$DGS7["2019-10"] )
t7yravr1
t7yravr2 = mean( t7yr$DGS7["2019-11"] )
t7yravr2

#t10yr = na.omit( t10yr )
t10yravr1 = mean( t10yr$DGS10["2019-10"] )
t10yravr1
t10yravr2 = mean( t10yr$DGS10["2019-11"] )
t10yravr2

#t30yr = na.omit( t30yr )
t30yravr1 = mean( t30yr$DGS30["2019-10"] )
t30yravr1
t30yravr2 = mean( t30yr$DGS30["2019-11"] )
t30yravr2

# 3.2 average price of gold ETF 

gldavr1 = mean( GLD$GLD.Close["2019-10"])
gldavr1
gldavr2 = mean( GLD$GLD.Close["2019-11"])
gldavr2

# 3.3 average price of equity ETF 

ukogavr1 = mean( UKOG.L$UKOG.L.Close["2019-10"])
ukogavr1
ukogavr2 = mean( UKOG.L$UKOG.L.Close["2019-11"])
ukogavr2

# 3.4 std of benchmark security std for Oct and Nov

t2yrsd1 = sd( t2yr$DGS2["2019-10"] )
t2yrsd1
t2yrsd2 = sd( t2yr$DGS2["2019-11"] )
t2yrsd2

t3yrsd1 = sd( t3yr$DGS3["2019-10"] )
t3yrsd1
t3yrsd2 = sd( t3yr$DGS3["2019-11"] )
t3yrsd2

t5yrsd1 = sd( t5yr$DGS5["2019-10"] )
t5yrsd1
t5yrsd2 = sd( t5yr$DGS5["2019-11"] )
t5yrsd2

t7yrsd1 = sd( t7yr$DGS7["2019-10"] )
t7yrsd1
t7yrsd2 = sd( t7yr$DGS7["2019-11"] )
t7yrsd2

t10yrsd1 = sd( t10yr$DGS10["2019-10"] )
t10yrsd1
t10yrsd2 = sd( t10yr$DGS10["2019-11"] )
t10yrsd2

t30yrsd1 = sd( t30yr$DGS30["2019-10"] )
t30yrsd1
t30yrsd2 = sd( t30yr$DGS30["2019-11"] )
t30yrsd2

# 3.5 std price of gold ETF 
gldsd1 = sd( GLD$GLD.Close["2019-10"])
gldsd1
gldsd2 = sd( GLD$GLD.Close["2019-11"])
gldsd2

# 3.6 std price of gold ETF 

ukogsd1 = sd( UKOG.L$UKOG.L.Close["2019-10"])
ukogsd1
ukogsd2 = sd( UKOG.L$UKOG.L.Close["2019-11"])
ukogsd2

# 4.1 graph 6 benchmarks

plot( x = index( t2yr ), y = t2yr$DGS2, type = "l", col = "black", xlab = "Time", ylab = "US Treasury yields", ylim = c(0.5,2.5) )
lines( x = index( t3yr ), y = t3yr$DGS3, type = "l", col = "red")
lines( x = index( t5yr ), y = t5yr$DGS5, type = "l", col = "blue")
lines( x = index( t7yr ), y = t7yr$DGS7, type = "l", col = "gray")
lines( x = index( t10yr ), y = t10yr$DGS10, type = "l", col = "purple")
lines( x = index( t30yr ), y = t30yr$DGS30, type = "l", col = "green")
legend( "bottom", legend=c("2 year", "3 year", "5 year", "7 year", "10 year", "30 year"), lty=1, col=c("black","red", "blue","gray","purple","green"), ncol = 3)

# 4.2 graph gold ETF
plot( x = index( GLD ), y = GLD$GLD.Close, type = "l", col = "black", xlab = "Time", ylab = "GLD ETF price" )

# 4.3 graph equity ETF at the same plot
plot( x = index( GLD ), y = GLD$GLD.Close, type = "l", col = "black", xlab = "Time", ylab = "GLD ETF price" )
par(mar=c(5, 4, 4, 6) + 0.1)
par(new=TRUE)
plot( x = index( UKOG.L ), y = UKOG.L$UKOG.L.Close, type = "l", col = "red", xlab = "", ylab = "", axes=FALSE)
mtext("UKOG.L ETF price",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
legend("topright",legend=c("GLDF","UKOG.L ETF"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 5.1 Nelson-Siegel
# Equation : https://en.wikipedia.org/wiki/Fixed-income_attribution
# Equation : y_t(τ) = β_{0t} + β_{1t} \frac{1-\exp(-λ τ)}{λ τ} + β_{2t} ≤ft(\frac{1-\exp(-λ τ)}{λ τ} - \exp(-λ τ) \right)

install.packages("XML")
install.packages("YieldCurve")

library(xts)
library(zoo)
library(YieldCurve)

tyr = cbind( t2yr$DGS2, t3yr$DGS3, t5yr$DGS5, t7yr$DGS7, t10yr$DGS10, t30yr$DGS30)
tyr
maturity.tyr = c( 2, 3, 5, 7, 10, 30 )
NSParameters <- Nelson.Siegel( rate= tyr, maturity=maturity.tyr )
NSParameters
nsp_y = NSrates( NSParameters, maturity.tyr)
nsp_y

# 5.2 Fit yeild curve for Oct

tyr_avr1 = c( t2yravr1,t3yravr1,t5yravr1,t7yravr1,t10yravr1,t30yravr1 )
nsp_oct = c( mean(nsp_y$X2["2019-10"]),mean(nsp_y$X3["2019-10"]),mean(nsp_y$X5["2019-10"]),mean(nsp_y$X7["2019-10"]),mean(nsp_y$X10["2019-10"]),mean(nsp_y$X30["2019-10"]) )
plot( x = maturity.tyr, y = tyr_avr1, type = "b" , col = "black",  xlab = "Time", ylab = "US Treasury yield" )
par(new=TRUE)
plot( x = maturity.tyr, y = nsp_oct, type = "l", col = "red",  xlab = "Time", ylab = "US Treasury yield" )
legend("topleft",legend=c("US Treasury average Oct","Nelson-Siegel Fit"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 5.3 Fit yeild curve for Nov

tyr_avr2 = c( t2yravr2,t3yravr2,t5yravr2,t7yravr2,t10yravr2,t30yravr2 )
nsp_Nov = c( mean(nsp_y$X2["2019-11"]),mean(nsp_y$X3["2019-11"]),mean(nsp_y$X5["2019-11"]),mean(nsp_y$X7["2019-11"]),mean(nsp_y$X10["2019-11"]),mean(nsp_y$X30["2019-11"]) )
plot( x = maturity.tyr, y = tyr_avr2, type = "b" , col = "black",  xlab = "Time", ylab = "US Treasury yield" )
par(new=TRUE)
plot( x = maturity.tyr, y = nsp_Nov, type = "l", col = "red",  xlab = "Time", ylab = "US Treasury yield" )
legend("topleft",legend=c("US Treasury average Nov","Nelson-Siegel Fit"),
       text.col=c("black","red"),col=c("black","red"), lty=1, cex = 0.75)

# 5.4 Comapre Oct and Nov

nsp_para_oct = c( mean( NSParameters$beta_0["2019-10"] ), mean(NSParameters$beta_1["2019-10"]), mean(NSParameters$beta_2["2019-10"]),mean(NSParameters$lambda["2019-10"]) )
nsp_para_nov = c( mean( NSParameters$beta_0["2019-11"] ), mean(NSParameters$beta_1["2019-11"]), mean(NSParameters$beta_2["2019-11"]),mean(NSParameters$lambda["2019-11"]) )
nsp_para_oct
nsp_para_nov
?Nelson.Siegel

# discussion
# b0 : is interpreted as the long run levels of interest rates (the loading is 1, it is a constant that does not decay).
# b1 : is the short-term component (it starts at 1, and decays monotonically and quickly to 0);
# b2 : is the medium-term component (it starts at 0, increases, then decays to zero);
# lambda : is the decay factor: large values produce slow decay and can better fit the curve at long maturities, 
# while small values produce fast decay and can better fit the curve at short maturities;
# When we look at the overall yield curve, we can confirm that the yield curve is increasing S curve.
# This means that overheating of economy is expected and it will result in inflation.
# In the case of inflation, it is better to sell the US treasury.
# This is why the b1 and b2 are negative.
# The selling induce the increase of yield, this is why the yield curve is increasing.
# The fitting parameter of Nelson-Siegel shows the consistent expectations.
# b0 is much larger that b1 and b2 which means the long run interest rate is much larger than shor, mid-term.
# But lambda is small (1/5) that the fit is better for the short maturities.
# This can be explained by the data we collect. We picked many short maturities, yet the long matuiries are only 10, 30 years.
# When we compare the Oct and Nov fit result, all of Oct b0,2 < Nov b0,2 although Oct b1 > Nov b1.
# This means that for short and long term interest rate, Oct is lower than Nov.
# But for mid term interest, Oct is higher than Nov.

# 6.0 modeling prices 
library(tseries)
library(ggplot2)

# For separate months of October and November 
# import GLD ETF October 
getSymbols(Symbols = "GLD", from = "2019-10-01", to = "2019-10-31" , src = "yahoo")
Oct_Gld_Prices <- GLD$GLD.Close
plot( x = index( GLD ), y = Oct_Gld_Prices )

# import GLD ETF November 
getSymbols(Symbols = "GLD", from = "2019-11-01", to = "2019-11-30" , src = "yahoo")
Nov_Gld_Prices <- GLD$GLD.Close
plot(x = index( GLD ), y = Nov_Gld_Prices )

# import my UKOG Equity ETF October
getSymbols(Symbols = "UKOG.L", from = "2019-10-01", to = "2019-10-31" , src = "yahoo")
UKOG.L
Oct_Ukog_prices <- UKOG.L$UKOG.L.Close

plot( x = index( UKOG.L ), y = Oct_Ukog_prices )

# import my UKOG Equity ETF November
getSymbols(Symbols = "UKOG.L", from = "2019-11-01", to = "2019-11-30" , src = "yahoo")
UKOG.L
Nov_Ukog_prices <- UKOG.L$UKOG.L.Close

plot( x = index( UKOG.L ), y = Nov_Ukog_prices )

# ARM model for GLD prices October (Modeling prices)
# ACF & PACF tests for GLD October & November
acf(Oct_Gld_Prices)
pacf(Oct_Gld_Prices)

acf(Nov_Gld_Prices)
pacf(Nov_Gld_Prices)

# stationary test
adf.test(Oct_Gld_Prices)
adf.test(Nov_Gld_Prices)

# 6.1 ARMA model for GLD ETF prices for October 
Gld_oct_prices <- arima(Oct_Gld_Prices, order = c(1,0,0))
summary(Gld_oct_prices)

Box.test(Gld_oct_prices$residuals, lag = 1)                        

# 6.2 ARMA model for GLD ETF prices for November 
Gld_nov_prices <- arima(Nov_Gld_Prices, order = c(1, 0,0))
summary(Gld_nov_prices)

Box.test(Gld_nov_prices$residuals, lag = 1)

# ACF and PACF for UKOG Equity ETF October & November 
acf(Oct_Ukog_prices)
pacf(Oct_Ukog_prices)

acf(Nov_Ukog_prices)
pacf(Nov_Ukog_prices)

# stationary tests
adf.test(Oct_Ukog_prices)
adf.test(Nov_Ukog_prices)

# 6.3 ARMA model for UKOG ETF Prices for October 
UKog_oct_prices <- arima(Oct_Ukog_prices, order = c(1,0,0))
summary(UKog_oct_prices)

Box.test(UKog_oct_prices$residuals, lag = 1)

# 6.4 ARMA model for UKOG ETF prices for November 
UKog_nov_prices <- arima(Nov_Ukog_prices, order = c(1,0,0))
summary(UKog_nov_prices)

Box.test(UKog_nov_prices$residuals, lag = 1)

# 6.5 Which model performs best 
# The results indicates that none of the model shows to be statistically significant implying that 
# the model is not parsimonious. However, ARMA model estimation for the month of 
# October appears to perform better than the ARMA models for the month of November 
# for both GLD and UKOG prices.The coefficient for the month of October appears to be generally lower 
# the coefficient of November models. Also the AIC for the month of October are generally lower than
# AIC in November. So comparing both the coefficient and AIC, it suggests that UKOG ETF ARMA October model
# appears to perform better with coefficient of 0.5435 and aic = -83.41.

# 6.6 What are the significant changes? if any
# The significant changes observed in the models includes changes in the coefficient and aic. GLD October ARMA mode
# indicate a coefficient of 0.4101 and aic of 52.54, while the GLD ARMA model for November indicated
# a coeficient of 0.8936 and aic of 59.3. Similarly, the ARMA model of UKOG October prices indicates a coefficient of 0.5435
# and aic of 83.41 while the ARMA model of November UKOG prices indicated coefficient of 0.8851 and aic of 93.28.
# ACF test indicates UKOG October prices autocorrelation of about 0.5 that rapidly decayed.

# 7.0 Modelling Volatility 
# 7.1 GLD ETF Daily high minus low for October
Gld_Oct_high <- max(Oct_Gld_Prices)
print(Gld_Oct_high)
Gld_Oct_low <- min(Oct_Gld_Prices)
print(Gld_Oct_low)

# October Dialy high minus low 
Gld_oct_diff <- Gld_Oct_high - Gld_Oct_low
print(Gld_oct_diff)

# GLD ETF Daily high minus low for November
Gld_nov_high <- max(Nov_Gld_Prices)
print(Gld_nov_high)

Gld_nov_low <- min(Nov_Gld_Prices)
print(Gld_nov_low)

# November Daily high minus low
Gld_nov_diff <- Gld_nov_high - Gld_nov_low
print(Gld_nov_diff)

# GLD October average 
Gld_Oct_avg <- mean(Oct_Gld_Prices)
print(Gld_Oct_avg)

# GLD November average 
Gld_Nov_avg <- mean(Nov_Gld_Prices)
print(Gld_Nov_avg)

# 7.2 STD for GLD ETF Returns for October and November 
# Log Returns for GLD October prices
gldlog_oct <- diff(log(Oct_Gld_Prices))
gldlog2_oct <- gldlog_oct[-1]
print(gldlog2_oct)

# Log Returns for GLD November prices 
gldlog_nov <- diff(log(Nov_Gld_Prices))
gldlog2_nov <- gldlog_nov[-1]
print(gldlog2_nov)

# STD for GLD October and November log returns 
gld_oct_sd = sd(gldlog2_oct)
print(gld_oct_sd)
gld_nov_sd = sd(gldlog2_nov)
print(gld_nov_sd)

# 7.3 Volatility GARCH (1,1) model for October GLD Return
library(rugarch)
gld_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = 'norm')

gurcg_gld1 <- ugarchfit(spec =gld_gurch, data = gldlog2_oct)
print(gurcg_gld1)

# Volatility GARCH model for November GLD Return
gurcg_gld2 <- ugarchfit(spec =gld_gurch, data = gldlog2_nov)
print(gurcg_gld2)

# 7.4 What are the significant changes? if any
# GLD ETF GARCH model for October prices indicated a mean value of 0.000407
# and a p-value of 0.737075 while the GLD ETF GARCH model for November prices 
# indicated a negative mean value of -0.001188 and a smaller p-value of 0.386699.

# 7.5 UKOG Daily prices high minus low for October and November, average 
UKog_oct_high <- max(Oct_Ukog_prices)
print(UKog_oct_high)

Ukog_oct_low <- min(Oct_Ukog_prices)
print(Ukog_oct_low)

# October UKOG Daily High minus Low 
UKog_oct_diff <- UKog_oct_high - Ukog_oct_low
print(UKog_oct_diff)

#Daily average October UKOG prices 
Ukog_oct_avg <- mean(Oct_Ukog_prices)
print(Ukog_oct_avg)

# November UKOG Daily high minus low
Ukog_nov_high <- max(Nov_Ukog_prices )
print(Ukog_nov_high)

Ukog_nov_low <- min(Nov_Ukog_prices )
print(Ukog_nov_low)

# Daily high minus  low UKOG Nov prices
Ukog_nov_diff <- Ukog_nov_high - Ukog_nov_low
print(Ukog_nov_diff)

# Daily average November UKOG prices
Ukog_nov_avg <- mean(Nov_Ukog_prices )
print(Ukog_nov_avg)

# 7.6 returns and STD for UKOG ETF October and November Prices 
# log returns for UKOG ETF October prices 
ukoglog_oct <- diff( log(Oct_Ukog_prices) )
print(ukoglog_oct)
#ukoglog2 = na.omit(ukoglog )
ukoglog2_oct = ukoglog_oct[-1]
print(ukoglog2_oct)

# log returns for UKOG ETF November prices 
ukoglog_nov <- diff( log(Nov_Ukog_prices ) )
print(ukoglog_nov)
#ukoglog2 = na.omit(ukoglog )
ukoglog2_nov = ukoglog_nov[-1]
print(ukoglog2_nov)

# STD for UKOG October and November log returns 
ukog_oct_sd = sd(ukoglog2_oct)
print(ukog_oct_sd)
ukog_nov_sd = sd(ukoglog2_nov)
print(ukog_nov_sd)

# 7.7  Volatility GARCH (1,1) model for October UKOG.L Return
ukog_gurch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = 'norm')

gurcg_ukog1 <- ugarchfit(spec =ukog_gurch , data = ukoglog2_oct)
print(gurcg_ukog1)

# Volatility GARCH model for November GLD Return
gurcg_ukog2 <- ugarchfit(spec =ukog_gurch, data = ukoglog2_nov)
print(gurcg_ukog2)

# 7.8 What are the significant changes? if any 
# The UKOG GARCH model for October prices indicated a mean value of 0.001098 and
# p-value of 0.86797 (suggesting that is not is not statistically significant) while 
# the GARCH model for November prices indicated a much better model with a mean value 
# of -0.005280 and p-value of 0.25379.

# 8.0 Correlation 

