library(tidyverse)
library(lubridate)
library(quantmod)
library(urca)
library(stats)
library(vars)

#Assignment 1 
#Exercise 1

#sorry for the absolute path
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Wirtschaftsuniversität/MASTER/Advanced Macroeconometrics")

#reading in the data 
current <- read.csv("current.csv")

#removing the first row
current_1 <- current %>% filter(!sasdate == "Transform:")

#Create a function that takes a vector containing observations of a time series as input and returns a dataframe
#with the following transformed series in its columns as output:
#– the original time series in its raw form.
#– the log-transformed time series.
#– month-on-month growth rates in percent.
#– year-on-year growth rates in percent.
#– the first lag of the year-on-year growth rates of the time series.

ts_transform <- function(x) {
  y <- data.frame(matrix(NA,    # Create empty data frame
                                 nrow = NROW(x),
                                 ncol = 0))
  y$date<- current_1$sasdate
  y$raw <- x
  y$log <- log(x)
  y$mom <- ((x-lag(x))/lag(x))*100
  y$yoy <- ((x-lag(x, 12))/lag(x, 12))*100
  y$yoy_1stlag <- lag((x-lag(x, 12))/lag(x, 12))*100
  return(y)
}


#Use the created function to create a dataframe with the various transformation for US industrial production (mnemonic INDPRO), plot the logged time series and the yearly changes produced by the function. Briefly
#describe the properties of the time series.

#appyling the function
USINDPRO <-ts_transform(current_1$INDPRO)

#filtering for missing value in log time series to create proper dates
USINDPRO_1 <- USINDPRO %>% filter(!is.na(log))

#creating an xts object for log and year on year growth rates
USINDPRO_log <- xts(USINDPRO_1$log, mdy(USINDPRO_1$date))
USINDPRO_yoy <- xts(USINDPRO_1$yoy, mdy(USINDPRO_1$date))

par(mfrow = c(1, 2))

plot(as.zoo(USINDPRO_log), 
     col = "darkred",
     lwd = 2,
     ylab = "log",
     xlab = "date",
     main = "US INDPRO",
     cex.main = 1)

plot(as.zoo(USINDPRO_yoy), 
     col = "darkred",
     lwd = 2,
     ylab = "log",
     xlab = "date",
     main = "US INDPRO",
     cex.main = 1)


#Some comments: The INDPRO is clearly on a strong upward trend. 
#Somewhat concave as industrial production seeems to grow less fast over time. 
#One can clearly see the crisis in 70s, 80s, and downturns induced by 2008 financial crisis as well as COVID.    

#The year on year growth rates exhibit a very high fluctuation ranging between +15 and -15 percent. 
#Downward trend somewhat observable, which is in line with decreasing slope of INDPRO plot. 


dev.off()


#Using suitable functions from the stats and urca package, assess the properties of both logged industrial production and its yearly growth rate. 
#Plot the autocorrelation function and perform Dickey-Fuller tests to test for a unit root (note the different specifications, i.e. including a drift or a trend), interpret the results.
acf(USINDPRO_1$log)
acf(na.omit(USINDPRO_1$yoy))

summary(ur.df(USINDPRO_1$log, 
              type = "none", 
              lags = 0, 
              selectlags = "Fixed"))

#evidence for stationarity :()
summary(ur.df(USINDPRO_1$log, 
              type = "drift", 
              lags = 0,
              selectlags = "Fixed"))

summary(ur.df(USINDPRO_1$log, 
              type = "drift", 
              lags = 1, 
              selectlags = "Fixed"))

summary(ur.df(USINDPRO_1$log, 
              type = "trend", 
              lags = 0, 
              selectlags = "Fixed"))

summary(ur.df(USINDPRO_1$log, 
              type = "trend", 
              lags = 1, 
              selectlags = "Fixed"))

summary(ur.df(na.omit(USINDPRO_1$yoy), 
              type = "none", 
              lags = 0, 
              selectlags = "Fixed"))



#We cannot reject that the log of INDPRO in the US is stochastic. Hence we cannot find evidence that the time series is stationary, with the exception for the specification of the drift term. Around a drift the time series seems to be stationary.   
#There is strong evidence that the year on year growth rates are stationary. See all specifications of DF-test.


mod_1 <- ar.ols(USINDPRO_1$yoy, aic = TRUE, order.max = NULL, na.action = na.omit, 
                demean = FALSE, intercept = FALSE)

mod_2 <- ar.ols(USINDPRO_1$log, aic = TRUE, order.max = NULL, na.action = na.omit,
                demean = TRUE, intercept = TRUE)

summary(mod_2)

predict_mod_yoy <- predict(mod_1, USINDPRO_1$yoy, n.ahead = 12)

predict_mod_log <- predict(mod_2, USINDPRO_1$log, n.ahead = 12)


#plotting the predicted values for the next years
plot.ts(USINDPRO_1$log)

points(predict_mod_log$pred, type = "l", col = 2)

#plotting the predicted values for the next years
plot.ts(USINDPRO_1$yoy)

points(predict_mod_yoy$pred, type = "l", col = 2)


#out of sample forecasts
subset <- USINDPRO_1 %>% subset(date < "2010-01-01")

mod_subset_log <- ar.ols(subset$log, aic = TRUE, order.max = NULL, na.action = na.omit,
                demean = TRUE, intercept = TRUE)

rmse <- function(ts,lag,holdout){}








#2nd exercise
#Using the the packages vars in R (or an equivalent one in another language), estimate the VAR described in section 2.2 using the variables in the same order as specified by Kilian & Park (2009)

df <- read.table('data_kilian_park_2009.txt',sep='')

df <- tibble(df)

df_1 <- df %>% 
  rename("PercChangeinCrudeProd" = "V1") %>%
  rename("GlobalRealActivity" = "V2") %>%
  rename("RealPriceofCrudeOil" = "V3") %>%
  rename("USdividend" = "V4")

myts <- ts(df_1, start=c(1973, 1), end=c(2006, 12), frequency=12)
myts <- myts[,c(1,2,3,4)]


#estimating the VAR models of the authors
var <- VAR(myts, p = 24, type = "const")


#replicating figure 3
irf_oil_demand_specific_shocks <- irf(var, impulse = "RealPriceofCrudeOil", response = "USdividend", boot = TRUE, cumulative = TRUE)

plot(irf_oil_demand_specific_shocks)


irf_agg_demand <- irf(var, impulse = "GlobalRealActivity", response = "USdividend", boot = TRUE, cumulative = TRUE, n.ahead = 15)

plot(irf_agg_demand)


irf_oil_supply <- irf(var, impulse = "PercChangeinCrudeProd", response = "USdividend", boot = TRUE, cumulative = TRUE, n.ahead = 15)

for(x in 1:length(irf_oil_supply$irf$PercChangeinCrudeProd)){
  irf_oil_supply$irf$PercChangeinCrudeProd[x,] <- irf_oil_supply$irf$PercChangeinCrudeProd[x,]*(-1)
  irf_oil_supply$Upper$PercChangeinCrudeProd[x,] <- irf_oil_supply$Upper$PercChangeinCrudeProd[x,]*(-1)
  irf_oil_supply$Lower$PercChangeinCrudeProd[x,] <- irf_oil_supply$Lower$PercChangeinCrudeProd[x,]*(-1)
}

plot(irf_oil_supply)




















#TEMP

diag <- diag(x = 4)

diag[2,1] <- NA
diag[3,1] <- NA
diag[3,2] <- NA
diag[4,1] <- NA
diag[4,2] <- NA
diag[4,3] <- NA

svar_1 <- SVAR(var, Amat = NULL, Bmat = diag, hessian = TRUE, method = c("scoring", "direct"))
amat <- diag(4)
diag(diag) <- NA

#Impulse Response Functions
SVARog <- irf(svar_1, Amat = diag)

SVARog
plot(SVARog)

VArirf <- irf(var, impulse = "PercChangeinCrudeProd", response = "GlobalRealActivity")
VArirf <- irf(var, impulse = "PercChangeinCrudeProd", response = "")
VArirf <- irf(var, impulse = "PercChangeinCrudeProd", response = "GlobalRealActivity")

SVARinf
plot(SVARinf)
SVARrrp <- irf(svar_1, impulse = "Inflation", response = "RRP")
SVARrrp
plot(SVARrrp)





#temp
# compute logarithms, annual growth rates and 1st lag of growth rates
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
  )
}
quants(x)

xts(x = INDPRO,
    order.by = INDPRO,
    frequency = NULL,
    unique = TRUE,
    tzone = Sys.getenv("TZ"))

is.xts(x)
# define series as xts objects
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]

DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]

JPIndProd <- xts(log(USMacroSWQ$JAPAN_IP), USMacroSWQ$Date)["1960::2013"]

# attach NYSESW data
data("NYSESW")  
NYSESW <- xts(Delt(NYSESW))
# divide plotting area into 2x2 matrix
par(mfrow = c(2, 2))

# plot the series
plot(as.zoo(USUnemp),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     xlab = "Date",
     main = "US Unemployment Rate",
     cex.main = 1)

plot(as.zoo(DollarPoundFX),
     col = "steelblue",
     lwd = 2,
     ylab = "Dollar per pound",
     xlab = "Date",
     main = "U.S. Dollar / B. Pound Exchange Rate",
     cex.main = 1)

plot(as.zoo(JPIndProd),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Japanese Industrial Production",
     cex.main = 1)

plot(as.zoo(NYSESW),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent per Day",
     xlab = "Date",
     main = "New York Stock Exchange Composite Index",
     cex.main = 1)

