data=read.csv('air_visit_data.csv')
air=read.csv("air_store_id.csv")

head(data)
str(data)
data[,2]=as.Date(data[,2])

tail(data)

ts=data[data$air_store_id=='air_24e8414b9b07decb',]
attach(data)

plot(ts$visit_date,ts$visitors)
lines(ts$visit_date,ts$visitors) 

library(tseries)
l=length(air$air_store_id)

vec1=air$air_store_id

vec2=c()
for( i in 1:l)
{
    
  df=data[data$air_store_id==air[i,],]
  z=adf.test(df$visitors)
  if(z<=0.05)
  {
       val="stationary"
       vec2=c(vec2,val)
  }
  else
  {
      val="not stationary"
      vec2=c(vec2,val)
  }
  
}

stat=data.frame(vec1,vec2)



ts=data[data$air_store_id=='air_24e8414b9b07decb',]
head(ts)
adf.test(ts$visitors)

plot(ts$visit_date,ts$visitors)
lines(ts$visit_date,ts$visitors)

acf(ts$visitors, lag.max=20) # plot correlogram
acf(ts$visitors,lag.max=20, plot=FALSE) #to get the partial autocorrelation values

pacf(ts$visitors, lag.max=20) # plot correlogram
pacf(ts$visitors,lag.max=20, plot=FALSE) #to get the partial autocorrelation values

ts=ts[ts$visit_date<='2017-02-28',]


dataArima=auto.arima(ts$visitors)
dataArima
auto.arima()

dataForecast =forecast(dataArima, h=31)

plot(dataForecast)

library(forecast)


ts=data[data$air_store_id=='air_24e8414b9b07decb',]
fv=ts[(ts$visit_date>='2017-03-01') , ]
va=fv[fv$visit_date<='2017-03-31',]

a=dataForecast$mean
a
b=va$visitors
b

error=((a-b)^2)/31
