#package zoo, tseries
install.packages("tseries")
install.packages("zoo")
library("tseries")
library("zoo")
library("gridExtra")
library("ggplot2")
library("forecast")

# Getting Microsoft stock prices form 2004 up to 2018-10-01.
MSFT.prices = get.hist.quote(instrument="msft", start="2004-01-01",end="2019-10-02", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")

plot(MSFT.prices, main="Adjusted Closing Prices", type="l", col="blue")

Msubset=window(MSFT.prices, end=as.Date("2019/10/01"))
p1 <- meanf(ts(Msubset), h=12)
p2 <- rwf(ts(Msubset), h=12)
p3 <- rwf(ts(Msubset), drift=TRUE, h=12)
autoplot(ts(MSFT.prices)) +
  forecast::autolayer(p1, PI=FALSE, series="Mean") +
  forecast::autolayer(p2, PI=FALSE, series="Naïve") +
  forecast::autolayer(p3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("MS stock price") +
  guides(colour=guide_legend(title="Forecast"))

AAPL.prices = get.hist.quote(instrument="aapl", start="2004-01-01",end="2019-10-02", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")
plot(AAPL.prices, main="AAPL Adjusted Closing Prices")

#MSFT
plot(MSFT.prices, main="Adjusted Closing Prices", lwd=2, col="blue")
#Monthly returns
MSFT.adjusted=(lag(MSFT.prices, k=1)-MSFT.prices)/MSFT.prices
plot(MSFT.adjusted)
# Are monthly returns gaussian noise? 
gn = rnorm(length(MSFT.adjusted),mean=mean(MSFT.adjusted[-1]),sd=sd(MSFT.adjusted[-1]))
gn=ts(gn)
plot1=autoplot(MSFT.adjusted)
plot2=autoplot(gn)
grid.arrange(plot1, plot2, ncol= 1)

plot3=ggAcf(coredata(MSFT.adjusted),lag.max=30)
plot4=ggAcf(gn, lag.max=30) #comparing with gaussian noise
grid.arrange(plot3,plot4,ncol=1)

#SP500
SP500.prices = get.hist.quote(instrument="^gspc", start="2004-01-01",end="2019-10-02", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")
plot(SP500.prices, main="Adjusted Closing Prices", lwd=2, col="blue")
SP500.adjusted=(lag(SP500.prices, k=1)-SP500.prices)/SP500.prices

plot(SP500.adjusted, ylim=c(-0.2,0.2))
lines(MSFT.adjusted, col="red") #comparing MSFT and SP500 monthly returns

ggAcf(coredata(SP500.adjusted))

#monthly returns tend to move together but SP500 has much lower volatility than MSFT, 
#individual asset returns have higher SD 
#than diversified portfolios
#Assets are approximately uncorrelated 
#over time (no serial correlation)

#The same for AAPL. Monthly returns from AAPL
AAPL.adjusted=(lag(AAPL.prices, k=1)-AAPL.prices)/AAPL.prices
plot(AAPL.adjusted)
ggAcf(coredata(AAPL.adjusted), lag.max=30)


# (Time series regression) Using Regression Models: Pollution, Temperature and Mortality in LA.
library(astsa)
library(ggfortify)
library(gridExtra) # to build grid of plots
data(lap) # Cardiovascular mortality from the Los Angeles Pollution study
head(lap)
str(lap)
# scatterplot matrix for Mortality, Temperature and Particulates
ggpairs(data.frame(lap[,c(3,4,11)]))
autoplot(lap[,c(3,4,11)])
p1=autoplot(lap[,3], ylab="Card. Mortality")
p2=autoplot(lap[,4], ts.colour="blue", ylab="Temperature")
p3=autoplot(lap[,11], ts.colour="green", ylab="Particulates")
grid.arrange(p1,p2,p3,nrow=3)

# 4 Models. Y=cmort, X=temp, temp2 and part
trend=time(lap)
temp=lap[,4]-mean(lap[,4])
temp2=temp^2
fit_1=lm(lap[,3]~trend, na.action=NULL)
fit_2=lm(lap[,3]~trend+temp, na.action=NULL)
fit_3=lm(lap[,3]~trend+temp+temp2, na.action=NULL)  
fit_4=lm(lap[,3]~trend+temp+temp2+lap[,11], na.action=NULL)
  
# Summary statistics for different models
AIC(fit_1, fit_2, fit_3, fit_4)
# residual sum of squares
deviance(fit_1); deviance(fit_2); deviance(fit_3); deviance(fit_4)
# AdjR^2
summary(fit_1)$adj.r.squared; summary(fit_2)$adj.r.squared; summary(fit_3)$adj.r.squared;summary(fit_4)$adj.r.squared
# comparing models
anova(fit_1,fit_4)

# We have still to check the residuals, for instance, for autocorrelation,
# of which there is a substantial amount ... 