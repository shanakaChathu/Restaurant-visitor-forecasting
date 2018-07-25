library('ggplot2')
library('forecast')
library('tseries')

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)
head(daily_data)
str(daily_data)
class(daily_data)

#Plotting the original time series 
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')
     + ylab("Daily Bike Checkouts") + xlab("")

#Cleaning the time series 

count_ts = ts(daily_data[, c('cnt')])
daily_data$clean_cnt = tsclean(count_ts)
ggplot() +geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')


#Smooting the tie series to obtain trend
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
plot(daily_data$cnt_ma)
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)
plot(daily_data$cnt_ma30)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')


#deseasonalizing the time series 

count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
plot(count_ma)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
decomp
plot(deseasonal_cnt)

#Test for stationarity 

adf.test(count_ma, alternative = "stationary")
?ts

#correlagrams 

acf(count_ma)
pacf(count_ma)


#Differenced series 

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Corregram

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#Fitting the model 
auto.arima(deseasonal_cnt, seasonal=FALSE)

#Evaluete the model 

fit=auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')


fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')


hold = window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout = forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

#Fit with seasonal

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

