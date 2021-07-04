#Final Project - Big Data - Tornadoes project


#install.packages("forecast")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("tseries)
library(data.table)
library(dplyr)
library(plot3D)
library(e1071)
library(caret)
library(party)
library(kernlab)
library(FactoMineR)
library(forecast)
library(tidyverse)
library(tseries)
#section 1 
dt = fread("tornadoes.csv")
tornadoes.dt = as.data.table(dt)
tornadoes.dt = tornadoes.dt[loss != 0,] #remove the rows with unknown data
#from 2016, convert it to scale from 1-9
for(i in 38132:nrow(tornadoes.dt)){
  if(tornadoes.dt$loss[i] < 50) tornadoes.dt$loss[i] = 1
  else if(tornadoes.dt$loss[i] >=50 & tornadoes.dt$loss[i] < 500) tornadoes.dt$loss[i] = 2
  else if(tornadoes.dt$loss[i] >=500 & tornadoes.dt$loss[i] < 5000) tornadoes.dt$loss[i] = 3
  else if(tornadoes.dt$loss[i] >=5000 & tornadoes.dt$loss[i] < 50000) tornadoes.dt$loss[i] = 4
  else if(tornadoes.dt$loss[i] >=50000 & tornadoes.dt$loss[i] < 500000) tornadoes.dt$loss[i] = 5
  else if(tornadoes.dt$loss[i] >=500000 & tornadoes.dt$loss[i] < 5000000) tornadoes.dt$loss[i] = 6
  else if(tornadoes.dt$loss[i] >=5000000 & tornadoes.dt$loss[i] < 50000000) tornadoes.dt$loss[i] = 7
  else if(tornadoes.dt$loss[i] >=50000000 & tornadoes.dt$loss[i] < 500000000) tornadoes.dt$loss[i] = 8
  else tornadoes.dt$loss[i] = 9
}
#from 1996-2016, convert it from miliions to scale of 1-9
for(i in 25569:38132){
  if(tornadoes.dt$loss[i]*1000000 < 50) tornadoes.dt$loss[i] = 1
  else if(tornadoes.dt$loss[i]*1000000 >=50 & tornadoes.dt$loss[i]*1000000 < 500) tornadoes.dt$loss[i] = 2
  else if(tornadoes.dt$loss[i]*1000000 >=500 & tornadoes.dt$loss[i]*1000000 < 5000) tornadoes.dt$loss[i] = 3
  else if(tornadoes.dt$loss[i]*1000000 >=5000 & tornadoes.dt$loss[i]*1000000 < 50000) tornadoes.dt$loss[i] = 4
  else if(tornadoes.dt$loss[i]*1000000 >=50000 & tornadoes.dt$loss[i]*1000000 < 500000) tornadoes.dt$loss[i] = 5
  else if(tornadoes.dt$loss[i]*1000000 >=500000 & tornadoes.dt$loss[i]*1000000 < 5000000) tornadoes.dt$loss[i] = 6
  else if(tornadoes.dt$loss[i]*1000000 >=5000000 & tornadoes.dt$loss[i]*1000000 < 50000000) tornadoes.dt$loss[i] = 7
  else if(tornadoes.dt$loss[i]*1000000 >=50000000 & tornadoes.dt$loss[i]*1000000 < 500000000) tornadoes.dt$loss[i] = 8
  else tornadoes.dt$loss[i] = 9
}
par(mfrow=c(1,1))
scatter3D(tornadoes.dt$loss,tornadoes.dt$len,tornadoes.dt$wid,ylab = "len",zlab = "width",xlab = "loss", phi = 13, bty = "g",  type = "h", theta = 30,
          ticktype = "detailed", pch = 19, cex = 0.5,main = "does length & width of the tornado effect the loss?")
#we can see that the p-value is 0, meaning there is high segnification between thos.
fit = lm(tornadoes.dt$loss~tornadoes.dt$len+tornadoes.dt$wid)
print(summary(fit)$coefficients)
#the correlations of them is 0.3661, what indicates of non-linear relationship.
print(cor(tornadoes.dt$loss,tornadoes.dt$wid+tornadoes.dt$len))
plot(tornadoes.dt$loss,tornadoes.dt$wid,pch=20,main = "does len of the tornado path effect loss in $? ")
plot(tornadoes.dt$loss,tornadoes.dt$wid,pch=20,main = "does width of the tornado path effect loss in $? ")

#section 2
# we can see that the amount of the fatalities effected by the fujita scale and the coorelation
# is 0.9194422, this pointing on a linear relationship.
scatter3D(tornadoes.dt$mag,tornadoes.dt$inj,tornadoes.dt$fat,xlab = "mag",ylab = "inj",zlab = "fat", phi = 13, bty = "g",  type = "h", theta = 30,
          ticktype = "detailed", pch = 19, cex = 0.5,main = "does inj & fat effected by tornado scale?")
tornadoes.dt[,total_fatalities:=sum(fat),by=paste0(mag)]
tornadoes.dt[,total_injuries:=sum(inj),by=paste0(mag)]
#check if fatalities effected by fujita scale. 
temp = as.data.table(unique(tornadoes.dt$mag),unique(tornadoes.dt$total_fatalities))
temp = data.table("mag" = unique(tornadoes.dt$mag),"total_fat" = unique(tornadoes.dt$total_fatalities))
temp = arrange(temp,temp$mag)
plot(temp$total_fat,temp$mag,pch=20,xlab = "total fatalities",ylab = "fujita scale",main="does fatalities effected by the fujita scale?")
lines(temp$total_fat,temp$mag,type="l")
fit2 = lm(tornadoes.dt$mag~tornadoes.dt$total_fatalities)
print(summary(fit2)$coefficients)
print(cor(tornadoes.dt$mag,tornadoes.dt$total_fatalities))
abline(fit2,col="blue",lwd = 3)
#check if injuries effected by fujita scale.
# we can see that the amount of the injuries effected by the fujita scale and the coorelation
# is 0.9786665, this pointing on a strong linear relationship.
temp = as.data.table(unique(tornadoes.dt$mag),unique(tornadoes.dt$total_injuries))
temp = data.table("mag" = unique(tornadoes.dt$mag),"total_inj" = unique(tornadoes.dt$total_injuries))
temp = arrange(temp,temp$mag)
plot(temp$total_inj,temp$mag,pch=8,col="red",xlab = "total injuries",ylab = "fujita scale",main="does injuries effected by the fujita scale?")
lines(temp$total_inj,temp$mag,type="l",col="orange")
fit2 = lm(tornadoes.dt$mag~tornadoes.dt$total_injuries)
print(summary(fit2)$coefficients)
print(cor(tornadoes.dt$mag,tornadoes.dt$total_injuries))
abline(fit2,col="black",lwd = 3)
#number of cases by fujita scale

tornadoes.dt[,total_mag:=sum(mag>=0),by=paste0(mag)]
t =  data.table("fujita_scale" = unique(tornadoes.dt$mag),"total_mag_per_mag " = unique(tornadoes.dt$total_mag))
t = arrange(t,t$fujita_scale)
plot(t$total_mag_per_mag,t$fujita_scale,pch=16,col="blue",xlab = "total cases per fujita scale",ylab = "fujita scale",main="number of tornado cases by fujita scale")
lines(t$total_mag_per_mag,t$fujita_scale,type="l",col="pink")

#section 3
#the coorelation is pretty low (0.38703), indicate that there isnt much linear relationship
#and we can see increasing in the frequency of the tornado cases from 1950-1980 and after that
#its stay stable.
tornadoes.dt[,tornado_frequency:=sum(om>0),by=paste0(yr)]
temp.tornado.dt.freq = tornadoes.dt%>%group_by(yr)%>%summarise(tornado_frequency = unique(tornado_frequency))
print(cor(temp.tornado.dt.freq$yr,temp.tornado.dt.freq$tornado_frequency))
plot(temp.tornado.dt.freq$yr,temp.tornado.dt.freq$tornado_frequency,pch=20,xlab = "year",ylab = "tornado frequency",main="number of tornado cases by year")
lines(temp.tornado.dt.freq$yr,temp.tornado.dt.freq$tornado_frequency,type="l",col="green",lwd=2)


#section 4
# the common month with the highest cases of tornadoes is in May, with 7645 cases.
# the less common month with the lowest cases of tornadoes is in December, with 1245 cases.
tornadoes.dt[,sum_of_cases_per_month := sum(om>0) , by = paste0(mo,yr)]
monthes = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

max_cases = 0
min_cases = .Machine$integer.max
#finding, which month is with the max cases and which month with less cases.
for(i in 1:12){
  temp.tornado = tornadoes.dt%>%filter(tornadoes.dt$mo == i)%>%group_by(yr)%>%summarise(cases_per_month = unique(mean(sum_of_cases_per_month)))
  ret = sum(temp.tornado$cases_per_month)
  if(ret > max_cases){
    max_cases = ret
    m = as.integer(i)
  }
  if(ret < min_cases){
    min_cases = ret
    n = as.integer(i)
  }
}
cat("max cases : ",max_cases, ", at month: ", monthes[m],"\n")
cat("min cases : ",min_cases, ", at month: ", monthes[n],"\n")

#section 5
#the state with the most cases of tornadoes is texas, with 4397 cases.
tornadoes.dt[,common_cases_by_state := sum(mo>0),by = paste0(st)]
temp.tornado.freq.by.state = tornadoes.dt%>%group_by(st)%>%summarise(common_cases_by_state = unique(common_cases_by_state))
temp.tornado.freq.by.state = arrange(temp.tornado.freq.by.state,temp.tornado.freq.by.state$common_cases_by_state)
cat("the state with the most cases of tornadoes is : ",as.character(tail(temp.tornado.freq.by.state,1)[1]), ", with ", as.integer(tail(temp.tornado.freq.by.state,1)[2])," cases\n")

ggplot(as.data.frame(temp.tornado.freq.by.state$common_cases_by_state), aes(x = temp.tornado.freq.by.state$st, y = temp.tornado.freq.by.state$common_cases_by_state)) +
  geom_point(col = "black") + xlab("state")+ ylab("cases")

#section 6
tornadoes.dt = tornadoes.dt[st == "TX",]
tornadoes.dt[,strong_storm := ifelse(mag > 2,1,2)]
tornadoes.dt[,sum_of_strongs_storms_per_year := sum(strong_storm==1),by = paste0(yr)]
tornadoes.dt[,sum_of_lights_storms_per_year := sum(strong_storm==2),by = paste0(yr)]
tornadoes.dt[,sum_of_fatalities_per_year := sum(fat),by = paste0(strong_storm,yr)]
tornadoes.dt[,sfata := ifelse(strong_storm==1,sum(fat),0),by = paste0(strong_storm,yr)]
temp = as.data.table(tornadoes.dt%>%group_by(yr)%>%summarise(strong = unique(sum_of_strongs_storms_per_year)))
#temp2 = as.data.table(tornadoes.dt%>%group_by(yr)%>%summarise(light = unique(sum_of_lights_storms_per_year)))
temp3 = as.data.table(tornadoes.dt%>% group_by(yr)%>%summarise(fatalities = unique(sfata),strong = unique(sum_of_strongs_storms_per_year)))
temp3[,fatalities:=sum(fatalities),by=paste0(yr)]
temp3 = temp3[!duplicated(temp3$yr)]
tornadoes.dt[,st :=NULL]
tornadoes.dt[,time :=NULL]
tornadoes.dt[,date :=NULL]
tornadoes.dt[,Class:=as.factor(strong_storm)]

#with random forest
random.forest = train(Class~., data=tornadoes.dt, method="rf")
random.forest.predictions = predict(random.forest, newdata=tornadoes.dt,interval="prediction")
random.forest.conf = confusionMatrix(random.forest.predictions,tornadoes.dt$Class)
print(random.forest.conf)


ggplot(as.data.frame(temp$strong), aes(x = temp$yr, y = temp$strong)) +
  geom_line(col = "black") + xlab("years")+ ylab("strong storms")+
  theme_bw() + theme(legend.title = element_blank(),
                     axis.text.x  = element_text(angle=45, vjust=0.5))


#one year prediction
#with linear regression
mod = lm(strong~yr, data = temp)
p = predict(mod, data.frame(yr = 2019))
cat("with lm and predicted function, we predicted to see",as.integer(p),"strong storms in 2019\n")
plot(temp$yr,temp$strong,main="predicting with linear regrassion")
abline(mod,col="blue")
#i adjust the arima model by looking at the residuals of the forcast and adjust the lag
#to be between the range.

#with arima model
#alternative hypothesis = stationary
#choose the best model
print(adf.test(temp$strong,alternative = "stationary"))
ts_data = ts(temp$strong, start = 1950,frequency = 1)
print(auto.arima(ts_data, trace= TRUE, ic ="aicc", approximation = FALSE, stepwise = FALSE,seasonal = FALSE))

acf(ts_data)
pacf(ts_data)
#ar = arima(ts_data,order = c(0,1,1),optim.control = list(maxit = 10000))
ar = arima(ts_data,order = c(0,1,12))
forecast.ts.data = forecast(ar,h=1)
cat("with arima and forecast function, we predicted to see",as.integer(forecast.ts.data$mean),"strong storms in 2019\n")
plot(ts_data,main="original data vs fitted data")
lines(forecast.ts.data$fitted,col="red")
legend("topright", lwd=1,col=c("black", "red","blue"),legend=c("data", "fitted","predicted"))
plot(forecast.ts.data)
print(accuracy(forecast.ts.data))

#5 years prediction
#with linear regression 
p = predict(mod, data.frame(yr = c(2019:2023)))
cat("with lm and predicted function, we predicted to see between the years 2019-2023:\n")
for(i in 1:5){
  cat(2018+i,":",as.integer(p[i]),"strong storms\n")
}
#with arima model
forecast.ts.data = forecast(ar,h=5)
cat("with arima and forecast function, we predicted to see between the years 2019-2023:\n")
for(i in 1:5){
  cat(2018+i,":",as.integer(forecast.ts.data$mean)[i],"strong storms\n")
}
acf(forecast.ts.data$residuals)
pacf(forecast.ts.data$residuals)
plot(forecast.ts.data)
lines(forecast.ts.data$fitted,col="red")
legend("topright", lwd=1,col=c("black", "red","blue"),legend=c("data", "fitted","predicted"))
print(accuracy(forecast.ts.data))

#10 years prediction
#with linear regression
p = predict(mod, data.frame(yr = c(2019:2028)))
cat("with lm and predicted function, we predicted to see between the years 2019-2028:\n")
for(i in 1:10){
  cat(2018+i,":",as.integer(p[i]),"strong storms\n")
}

#with arima model
forecast.ts.data = forecast(ar,h=10)
cat("with arima and forecast function, we predicted to see between the years 2019-2028:\n")
for(i in 1:10){
  cat(2018+i,":",as.integer(forecast.ts.data$mean)[i],"strong storms\n")
}

acf(forecast.ts.data$residuals)
pacf(forecast.ts.data$residuals)
plot(forecast.ts.data)
lines(forecast.ts.data$fitted,col="red")
legend("topright", lwd=1,col=c("black", "red","blue"),legend=c("data", "fitted","predicted"))
print(accuracy(forecast.ts.data))


#how many fatalities we supposed to see next year, in 5 years and in 10 years?

#next year by linear regression
mod3 = lm(fatalities~yr, data = temp3)
p3 = predict(mod3, data.frame(yr = 2019))
cat("with lm and predicted function, we predicted to see",as.integer(p3),"fatalities in 2019\n")
plot(temp3$yr,temp3$fatalities,main="predicting with linear regrassion")
abline(mod3,col="blue")
#next year by arima model
ts_data_fat = ts(temp3$fatalities, start = 1950,frequency = 1)
print(auto.arima(ts_data_fat, trace= TRUE, ic ="aicc", approximation = FALSE, stepwise = FALSE))
acf(ts_data_fat)
pacf(ts_data_fat)
#ar.fat = arima(ts_data_fat,order = c(0,0,0))
ar.fat = arima(ts_data_fat,order = c(0,1,12))
forecast.ts.data.fat = forecast(ar.fat,h=1)
cat("with arima and forecast function, we predicted to see",as.integer(forecast.ts.data.fat$mean),"fatalities in 2019\n")

plot(forecast.ts.data.fat)
lines(forecast.ts.data.fat$fitted,col="green")
legend("topright", lwd=1,col=c("black", "green","blue"),legend=c("data", "fitted","predicted"))
print(accuracy(forecast.ts.data.fat))

#in 5 years by linear regression
p3 = predict(mod3, data.frame(yr = c(2019:2023)))
cat("with lm and predicted function, we predicted to see between the years 2019-2023:\n")
for(i in 1:5){
  cat(2018+i,":",as.integer(p3[i]),"fatalities\n")
}

#in 5 years by arima model
forecast.ts.data.fat = forecast(ar.fat,h=5)
cat("with arima and forecast function, we predicted to see between the years 2019-2023:\n")
for(i in 1:5){
  cat(2018+i,":",as.integer(forecast.ts.data.fat$mean)[i],"fatalities\n")
}
acf(forecast.ts.data.fat$residuals)
pacf(forecast.ts.data.fat$residuals)
plot(forecast.ts.data.fat)
lines(forecast.ts.data.fat$fitted,col="green")
legend("topright", lwd=1,col=c("black", "green","blue"),legend=c("data", "fitted","predicted"))
print(accuracy(forecast.ts.data.fat))

#in 10 years by linear regression
p3 = predict(mod3, data.frame(yr = c(2019:2028)))
cat("with lm and predicted function, we predicted to see between the years 2019-2028:\n")
for(i in 1:10){
  cat(2018+i,":",as.integer(p3[i]),"fatalities\n")
}

#in 10 years by arima model
forecast.ts.data.fat = forecast(ar.fat,h=10)
cat("with arima and forecast function, we predicted to see between the years 2019-2028:\n")
for(i in 1:10){
  cat(2018+i,":",as.integer(forecast.ts.data.fat$mean)[i],"fatalities\n")
}
acf(forecast.ts.data.fat$residuals)
pacf(forecast.ts.data.fat$residuals)
plot(forecast.ts.data.fat)
lines(forecast.ts.data.fat$fitted,col="green")
legend("topright", lwd=1,col=c("black", "green","blue"),legend=c("data", "fitted","predicted"))
print(accuracy(forecast.ts.data.fat))

