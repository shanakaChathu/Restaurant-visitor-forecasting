library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(knitr)
library(tidyverse)
library(forecast)
library(xts)


df_air = read_csv(file = 'air_visit_data.csv')
df_air_store = read_csv('air_store_info.csv')
sample_sub=read.csv('sample_submission.csv')

merged=df_air %>% dplyr::left_join(df_air_store, by='air_store_id', how='left')

merged_sum = merged %>% 
  group_by(visit_date) %>% 
  summarize(visitors = sum(visitors)) 

genre_sum = merged %>%
  group_by(visit_date, air_genre_name) %>% 
  summarize(visitors=sum(visitors))

genre_unique <- merged %>% select(air_genre_name) %>% unique %>% unlist  
genre_unique


plot_genre <- function(i){
  genre_specific_sum = genre_sum %>% filter(air_genre_name==i)
  genre_train = genre_specific_sum %>% filter(visit_date <='2017-02-01')
  genre_test = genre_specific_sum %>% filter(visit_date >'2017-02-01')
  
  m = arima(genre_train$visitors, order=c(2,1,2), seasonal= list(order=c(1,1,1), period=7))
  y_pred = forecast::forecast(m, h=80)
  
  plot(ts(genre_specific_sum$visitors), main=i, xlab='Year', ylab='visitors')
  lines(y_pred$mean, col='red')
}

plot_genre(genre_unique[14])









