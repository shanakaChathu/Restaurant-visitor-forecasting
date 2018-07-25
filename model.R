df=data[data$air_store_id=='air_24e8414b9b07decb',]
df=df[,c(2,3)]
str(df)

series=xts(df$visitors, as.Date(df$visit_date))
class(series)
series
plot(series)

#Filling missing values 

tseries = merge(series, zoo(,seq(start(series),end(series),by="day")), all=TRUE)
tseries = na.approx(tseries)
tseries

#Plotting 
plot(tseries)

#Stationarity checking
adf.test(tseries)

#ACF and PACF
acf(tseries, lag.max=20) # plot correlogram
acf(tseries,lag.max=20, plot=FALSE) #to get the partial autocorrelation values

pacf(tseries, lag.max=20) # plot correlogram
pacf(tseries,lag.max=20, plot=FALSE) #to get the partial autocorrelation values

start(tseries)

train= tseries["/2017-02"] 
train

test=tseries["2017-03"]
test

train

library(TTR)

trainSMA3= SMA(train,n=12)
plot.ts(trainSMA3)


trend_train = ma(train, order = 24, centre = T)
plot(train)
lines(trend_train)
plot(as.ts(trend_train))

train

a=boxplot(df$visitors)
library(tsoutliers)

w=tso(ts(train),types=c("AO","LS","TC"))
w

plot(w)


decompose(train,"additive")

plot(train)


ts_train = ts(train, frequency = 7)
decompose_train= decompose(ts_train, "additive")

decompose_train$seasonal

plot(as.ts(decompose_train$seasonal))
plot(as.ts(decompose_train$trend))

plot(decompose_train)


#seasonality detection

# Install and import TSA package
install.packages("TSA")
library(TSA)

# compute the Fourier Transform
p = periodogram(train)
p


dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 5)

# display the 2 highest "power" frequencies
top2

# convert frequency to time periods
time = 1/top2$f
time


plot(train)
