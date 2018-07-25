library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(knitr)
library(tidyverse)

df_air <- read_csv(file = 'air_visit_data.csv')
df_air_store <- read_csv('air_store_info.csv')

par(mfrow=c(2,1), cex=0.7)
df_air %>% 
  group_by(visit_date) %>% 
  summarize(visitors = sum(visitors)) %>% 
  plot(type='l', main='Overall Visitors')

merged <- df_air %>% 
  filter(visit_date > '2016-07-01') %>% 
  dplyr::left_join(df_air_store, by='air_store_id', how='left')

merged_sum <- merged %>% 
  group_by(visit_date) %>% 
  summarize(visitors = sum(visitors)) 

merged_sum %>% 
  plot(type='l', xlab='Year', main='Cut-off at July 2016')


#Model

merged_train <- merged_sum %>% filter(visit_date <='2017-02-01')
merged_test <- merged_sum %>% filter(visit_date >'2017-02-01')

#print(paste(nrow(merged_sum),nrow(merged_train),nrow(merged_test)))

m <- arima(merged_train$visitors, order=c(2,1,2), seasonal= list(order=c(1,1,1), period=7))
y_pred <- forecast::forecast(m, h=80)

par(mfrow=c(1,1), cex=0.7)
plot(ts(merged_sum$visitors), main="ARIMA model predictions, cut off at Feb 2017")
lines(y_pred$mean, col='red')


#Genre wise 

genre_sum <- merged %>%
  group_by(visit_date, air_genre_name) %>% 
  summarize(visitors=sum(visitors))

genre_unique = merged %>% select(air_genre_name) %>% unique %>% unlist  
genre_unique


graph_list=  list()

plot_genre <- function(i){
  genre_specific_sum <- genre_sum %>% filter(air_genre_name==i)
  genre_train <- genre_specific_sum %>% filter(visit_date <='2017-02-01')
  genre_test <- genre_specific_sum %>% filter(visit_date >'2017-02-01')
  
  m <- arima(genre_train$visitors, order=c(2,1,2), seasonal= list(order=c(1,1,1), period=7))
  y_pred <- forecast::forecast(m, h=80)
  
  plot(ts(genre_specific_sum$visitors), main=i, xlab='Year', ylab='visitors')
  lines(y_pred$mean, col='red')
}

par(mfrow=c(3,1), cex=0.7)
plot_genre(genre_unique[1])
plot_genre(genre_unique[2])
plot_genre(genre_unique[3])



sampsub <- read_csv('sample_submission.csv') %>% 
  separate(id, c('prefix','store','Date'),'_') %>%
  mutate(store_id = paste0(prefix,'_',store)) %>% 
  mutate(id = paste0(store_id,'_',Date))

read_csv('sample_submission.csv') %>% head(4)




