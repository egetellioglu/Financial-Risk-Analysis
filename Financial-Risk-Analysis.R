### METHODS OF FINANCIAL RISK ANALYSIS ###  

getwd()
load("C:\\Users\\user\\Documents\\GitHub\\Financial-Risk-Analysis-\\apple_prices.Rdata")
load("C:\\Users\\user\\Documents\\GitHub\\Financial-Risk-Analysis-\\sp500_prices.Rdata")


install.packages("RPostgres")
install.packages("PerformanceAnalytics")
install.packages("MASS")
install.packages("sn")
install.packages("xts")

library("RPostgres")
library("PerformanceAnalytics")
library("MASS")
library("sn")
library("xts")

head(apple_prices)
head(sp500_prices)

tail(apple_prices)

class(apple_prices)

apple_xts <- xts(apple_prices$prc, apple_prices$date)
sp500_xts <- xts(sp500_prices$spindx, sp500_prices$caldt)

class(apple_xts)

na.omit(apple_xts)
is.na(apple_xts)

which(is.na(sp500_xts))


### Calculating Returns: 

Return.calculate(apple_xts)
Return.calculate(sp500_xts)

plot(apple_prices[,1],apple_prices[,3], type="l", lwd = 1, main= "Apple Prices")

install.packages("rugarch")
install.packages("zoo")
install.packages("qrmdata")
install.packages("qrmtools")

library(rugarch)
library(zoo)
library(qrmdata)
library(qrmtools)


library(xts)
library(qrmdata)
library(qrmtools)


### Stock prices ###############################################################

## Dow Jones stock price data
data("DJ_const")
str(DJ_const)

## We extract a time period and take the first 10 stocks
DJdata <- DJ_const['2006-12-29/2015-12-31', 1:10]

## Use plot for zoo objects to get multiple plots
plot.zoo(DJdata, xlab = "Time", main = "DJ (10 component series)")
X <- returns(DJdata) # or diff(log(DJdata))[-1,]
head(X)
plot.zoo(X, xlab = "Time", main = "Log-returns of 10 DJ component series")

## Aggregating log returns by summation for each column
## Weekly
X.w <- apply.weekly(X, FUN = colSums)
dim(X.w)
plot.zoo(X.w, type = "h", xlab = "Time", main = "Weekly log-returns") # 'h' = histogram = vertical lines only
## Monthly
X.m <- apply.monthly(X, FUN = colSums)
dim(X.m)
plot.zoo(X.m, type = "h", xlab = "Time", main = "Monthly log-returns")
## Quarterly
X.q <- apply.quarterly(X, FUN = colSums)
dim(X.q)
plot.zoo(X.q, type = "h", xlab = "Time", main = "Quarterly log-returns")


### Stock indexes ##############################################################

## Load stock index data
data("SP500") # S&P 500
data("FTSE") # FTSE
data("SMI") # SMI
plot.zoo(SP500, xlab = "Time", ylab = "S&P 500")

## Merge the three time series
all <- merge(SP500, FTSE, SMI)
nms <- c("S&P 500", "FTSE", "SMI")
colnames(all) <- nms
plot.zoo(all, xlab = "Time", main = "All")

## Merge and retain only days where all three time series are available
all.avail <- merge(SP500, FTSE, SMI, all = FALSE)
colnames(all.avail) <- nms
plot.zoo(all.avail, xlab = "Time", main = "All available")

## Compute returns
SP500.X <- returns(SP500)
FTSE.X  <- returns(FTSE)
SMI.X   <- returns(SMI)
X <- merge(SP500.X, FTSE.X, SMI.X, all = FALSE)
colnames(X) <- nms
plot.zoo(X, xlab = "Time", main = "Log-returns")
pairs(as.zoo(X), gap = 0, cex = 0.4)

## Aggregating
## By week
X.w <- apply.weekly(X, FUN = colSums)
plot.zoo(X.w, xlab = "Time", main = "Weekly log-returns", type = "h")
## By month
X.m <- apply.monthly(X, FUN = colSums)
plot.zoo(X.m, xlab = "Time", main = "Monthly log-returns", type = "h")


### Exchange rates #############################################################

## Load exchange rate data
data("GBP_USD") # 1 GBP in USD
data("EUR_USD") # 1 EUR in USD
data("JPY_USD") # 1 JPY in USD
data("CHF_USD") # 1 CHF in USD
FX <- merge(GBP_USD, EUR_USD, JPY_USD, CHF_USD)
head(FX)
plot.zoo(FX, xlab = "Time", main = "Exchange rates to USD")
X <- returns(FX)
plot.zoo(X, xlab = "Time", main = "Log-returns of exchange rates to USD",
         col = c("black", "royalblue3", "maroon3", "darkorange2"))


### Zero-coupon bond yields ####################################################

## Load zero-coupon bond yield data (in USD)
## Note: Typically, yield = ((face value / current bond price)^(1/maturity) - 1) * 100%
##       (as face value = current price * (1 + yield)^maturity
data("ZCB_US")
dim(ZCB_US) # => 30-dimensional; each dimension is a maturity
head(ZCB_US)
ZCB <- ZCB_US['2002-01-02/2011-12-30']
plot.zoo(ZCB, xlab = "Time", main = "Percentage yields")

## Compute differences (first row is removed)
X <- returns(ZCB, method = "diff")

## Pick 3 maturities
X3 <- X[, c(1, 5, 30)]
plot.zoo(X3, xlab = "Time", main = "Differences (3 maturities)")
pairs(as.zoo(X3), gap = 0, cex = 0.4)

## Plot the corresponding "pseudo-observations" (componentwise scaled ranks)
U3 <- apply(X3, 2, rank) / (ncol(X3) + 1)
pairs(as.zoo(U3), gap = 0, cex = 0.4)


### 2.2 Calculate returns

return_sp500 = na.omit(Return.calculate(sp500_xts, method = "discrete"))
return_apple = na.omit(Return.calculate(apple_xts, method = "discrete"))

returnln_sp500 = na.omit(Return.calculate(sp500_xts, method = "log"))
returnln_apple = na.omit(Return.calculate(apple_xts, method = "log"))

retln_sp500 = na.omit(diff(log(sp500_xts), 1))
retln_apple = na.omit(diff(log(apple_xts), 1))

### Plot price and return series

par(mfrow = c(1, 2))
plot(sp500_xts, type = "l", col = "darkgoldenrod1")
plot(apple_xts, col = "firebrick", type = "l")

figure = plot(return_apple, type = "l", lty = 2, xlab = "Date", ylab = "Return", 
              grid.col = NA, main = "Return series for Apple and S&P 500")
figure = lines(return_sp500, col = "deepskyblue4", lty = 1)
addLegend("bottom", legend.names = c("Apple", "S&P 500"), col = c("black", "deepskyblue4"), 
          lty = c(2, 1), bg = "white", bty = "o")
figure


## 4 How is the SP500 distributed?

hist(returnln_sp500, breaks = 50, main = "Histogram of SP500 log returns", xlab = "", 
     ylab = "")

hist(returnln_sp500, breaks = 50, main = "Histogram with normal distribution", 
     prob = T, xlab = "", ylab = "")
curve(dnorm(x, mean = mean(returnln_sp500), sd = sd(returnln_sp500)), add = TRUE, 
      col = "red")


#### 5 Estimate one day Value at Risk

# Estimate one day VaR0.95 and VaR0.99 with T = 1 (2018), use the apple data

# 5.1 Variance-covariance
## Use the Variance-covariance method : VaR??=??L+??????????1(??). 
## Assume that the returns are i.i.d. and normally distributed.


apple_2018 = as.xts(returnln_apple)["2015-01/2018"]  #shorten time series


alpha = c(0.05, 0.01)

(VaR_varcov = qnorm(alpha, mean(apple_2018, na.rm = T), sd(apple_2018, na.rm = T), 
                    lower.tail = T))

# alternative approaches

mean(apple_2018, na.rm = T) + sd(apple_2018, na.rm = T) * qnorm(alpha)

VaR(apple_2018, method = "gaussian", p = 0.95)

VaR(apple_2018, method = "gaussian", p = 0.99)

#### 5.2 Historical Simulation

q_HS = as.numeric(quantile(apple_2018, probs = alpha))  #mirrored loss
hist(apple_2018, breaks = 50, freq = F, main = "Histogram with VaR")
abline(v = q_HS, col = "red")

q_HS

# 5.3 Monte-Carlo Method

set.seed(150)
z = as.numeric(sample(apple_2018, size = 10000, replace = T))

q_MC = as.numeric(quantile(z, probs = alpha))  #mirrored loss
hist(z, breaks = 50)
abline(v = q_MC, col = "red")


q_MC


### 6 Estimate ten day Value at Risk

### 6.1 Square-root-of-time (???t) rule

(varcov_sqr10 = VaR_varcov * sqrt(10))

VaR_varcov

(q_HS_sqr10 = q_HS * sqrt(10))

(q_MC_sqr10 = q_MC * sqrt(10))

#6.2 Actual 10 day returns

# calculate 10 day returns for 2018

apple10 = as.xts(na.omit(diff(log(apple_xts), 10)))["2018-01/2018"]

