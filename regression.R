
df_air = read_csv(file = 'air_visit_data.csv')
df_air_store = read_csv('air_store_info.csv')
sample_sub=read.csv('sample_submission.csv')

merged=df_air %>% dplyr::left_join(df_air_store, by='air_store_id', how='left')

table(df_air_store$air_area_name)
table(df_air_store$latitude)

head(merged)
df=as.data.frame(merged)


df[,2]=as.character(df[,2])
df$month=substr(df$visit_date,6,7)
df$day=substr(df$visit_date,9,10)


df$month=month
head(df)
data=df[,c(2,4,5,6,7,8,9,3)]

train=data[data$visit_date<='2017-03-31',]
test=data[data$visit_date>'2017-03-31',]

train=train[,c(2,3,4,5,6,7,8)]
test=test[,c(2,3,4,5,6,7,8)]
library(rpart)


model=rpart(visitors~.,data=train,control = rpart.control(minsplit=10))
model$cptable
opt=which.min(model$cptable[,"xerror"])
cp=model$cptable[opt,"CP"]
mprune=prune(model,cp=cp)

pred=predict(model,newdata=test)
pred

pred_df=data.frame(test$visitors,pred)

RMSE=sqrt((sum(test$visitors-pred)^2)/length(pred))

