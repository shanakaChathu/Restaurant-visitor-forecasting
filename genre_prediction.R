library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(knitr)
library(tidyverse)
library(forecast)
library(xts)


df_air = read_csv(file = 'air_visit_data.csv')
df_air_store = read_csv('air_store_info.csv')
air=df_air %>% dplyr::left_join(df_air_store, by='air_store_id', how='left')

genre_unique =air%>% select(air_genre_name) %>% unique %>% unlist  
genre_unique
table(df_air_store$air_genre_name)

#Asian 
asian=air[air$air_genre_name=='Asian',]
asian_sum = asian %>% group_by(visit_date) %>% summarize(visitors = mean(visitors)) 

series=xts(asian_sum$visitors, as.Date(asian_sum$visit_date))
tseries = merge(series, zoo(,seq(start(series),end(series),by="day")), all=TRUE)
tseries = na.approx(tseries)

df=as.data.frame(tseries)
visit_date=as.Date(row.names(df))
visitors=df$series
asian_air=data.frame(visit_date,visitors)

plot(ts(asian_air$visitors), main="ASIAN")


asian_subset=asian_air[asian_air$visit_date>='2016-07-01',]
asian_subset=asian_air[,]

asian_train = asian_subset %>% filter(visit_date <='2017-03-01')
asian_test = asian_subset %>% filter(visit_date >'2017-03-01')


m = arima(asian_train$visitors, order=c(2,1,2), seasonal= list(order=c(1,1,1), period=7))
#m=auto.arima(asian_train$visitors,seasonal=T)
n=nrow(asian_test)
y_pred = forecast::forecast(m, h=n)
lines(y_pred$mean, col='red')
predict=as.numeric(y_pred$mean)
p=log(predict+1)
a=log(asian_test$visitors+1)


RMSE=sqrt(sum((p-a)^2)/n)
RMSE



w1=asian[(asian$visit_date>='2016-07-01') ,]
w2=w1[w1$visit_date<='2016-12-31',]
prop=w2%>%group_by(air_store_id) %>% summarize(visitors = sum(visitors)) 
prop

(6685/(6685+5870))*100
