library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(knitr)
library(tidyverse)
library(forecast)
library(xts)


df_air = read_csv(file = 'air_visit_data.csv')
df_air_store = read_csv('air_store_info.csv')
sample_sub=read.csv('sample_submission.csv')
#i=air_unique[1]


air_sum = df_air%>%
  group_by(air_store_id,visit_date) %>% 
  summarize(visitors=sum(visitors))

air_sum=air_sum[air_sum$visit_date>'2016-07-01',]

air_unique =air_sum %>% select(air_store_id) %>% unique %>% unlist  
len=length(air_unique)

test=function(i)
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
  air_train=air[air$visit_date<='2017-03-31',]
  
  
  m = arima(air_train$visitors, order=c(2,1,2), seasonal= list(order=c(1,1,1), period=7),method="CSS")
 
  y_pred = forecast::forecast(m,61 )
  n=length(y_pred$mean)
  
  predict=as.numeric(y_pred$mean)
  avg=median(predict[predict>0])
  predict[is.na(predict)]=avg
  predict[predict<0]=avg
  #predict=abs(predict)
  predict
  
}


main=function()
{
  d=data.frame()  
  len=length(air_unique)
  pred_dates= seq.Date(as.Date('2017-04-01'), as.Date('2017-05-31'),by=1) 
  for(i in 1:len)
  {
         pred=test(air_unique[i])
         str=air_unique[i]
         rstr=replicate(61,str)
         df=data.frame(rstr,pred_dates,pred)
         d=d%>%bind_rows(df)
  }
 d 
 #write.csv(d,"submission.csv")
 save(d,file='submission.RData')
}

main()

sub=load("submission.RData")
df=d[d$pred_dates>'2017-04-22',]

df$id=paste0(df$rstr,'_',df$pred_dates)
df$id=as.factor(df$id)

df_pred =df %>%inner_join(sample_sub, by="id") %>% select(id,pred)
colnames(df_pred)=c('id','visitors')

write.csv(df_pred,'submission6.csv')




