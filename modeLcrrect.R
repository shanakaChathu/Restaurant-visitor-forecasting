library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(knitr)
library(tidyverse)
library(forecast)
library(xts)


df_air = read_csv(file = 'air_visit_data.csv')
df_air_store = read_csv('air_store_info.csv')

#i=air_unique[454]


air_sum = df_air%>%
  group_by(air_store_id,visit_date) %>% 
  summarize(visitors=sum(visitors))

air_unique =air_sum %>% select(air_store_id) %>% unique %>% unlist  
len=length(air_unique)

test=function(i,p)
{
  
  air_specific_sum = air_sum %>% filter(air_store_id==i)
  
  a=air_specific_sum[,c(2,3)]
  series=xts(a$visitors, as.Date(a$visit_date))
  tseries = merge(series, zoo(,seq(start(series),end(series),by="day")), all=TRUE)
  tseries = na.approx(tseries)
  
  df=as.data.frame(tseries)
  visit_date=as.Date(row.names(df))
  
  visitors=df$series
  air=data.frame(visit_date,visitors)
  
  
  fin=nrow(air)
  sp=round((nrow(air)*80)/100)
  air_train=air[1:sp,]
  air_test=air[(sp+1):52,]
  
  m = arima(air_train$visitors, order=c(2,1,2), seasonal= list(order=c(1,1,1), period=p),method="CSS")
  #m=auto.arima(air_train$visitors,seasonal=TRUE,method='CSS')
  h=nrow(air_test)
  y_pred = forecast::forecast(m,h )
  n=length(y_pred$mean)
  
  predict=as.numeric(y_pred$mean)
  #predict[predict<0]=2
  avg=median(predict[predict>0])
  predict[is.na(predict)]=avg
  predict[predict<0]=avg
  p=log(predict+1)
  #avg=median(p,na.rm = TRUE)
  #p[is.na(p)]=avg
  
  a=log(air_test$visitors+1)
  
  RMSE=sqrt(sum((p-a)^2)/n)
  RMSE
  
}


fun=function()
{
   vec2=c()  
   for(p in 1:7)
   {   
    vec2=c()
    vec=c()
    for( i in 1:len)
    {
    
     RMSE=test(air_unique[i],30)
     vec=c(vec,RMSE)
  
    }
    m=mean(vec,na.rm=T)
    vec2=c(vec2,m)
    vec2
   }
  vec2
}

fun()


