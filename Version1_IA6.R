library(Metrics)
#Question 1 (25 points)
attach(airline)
#The following data contains the monthly number of airlines tickets sold by a travel agency for four years.
#Our goal is to build a regression model to predict the demand for the following 12 months.

#a) (2 pts) Does a linear trend appear to fit these data well? Explain why or why not. Reference any tables/figures that you need to make your point.


plot(airline$T, airline$Tickets, type = 'l')

#b) (5+2+2 = 9 pts) Build a linear trend model or nonlinear trend regression model (depending on your answer in part a). 
#Do not add a seasonality factor to this model. To validate your model, use the last 12 months as a validation data set.

train <- head(airline,36) #diving the train data  

test <- tail(airline,12)#dividing the test data

train.lm.trend<- lm(Tickets ~ T, data = train) #building the model with the test data

summary(train.lm.trend) #getting the summary of the model which was built by the test data

observed<-test$Tickets # saving the test data dependent variables as observed data

predicted<-predict(train.lm.trend, test) # saving the predicted values from the model as the predicted values

#plot data and forecasts
plot(airline$T, airline$Tickets, type ='l')

#plot fitted values in the training period
lines(train.lm.trend$fitted, lwd=2)
lines(test$T, predicted, lwd= 2 , col= 'blue')

#residual plot
plot(fitted(train.lm.trend), resid(train.lm.trend))
abline(0,0)


#Copy and paste your R code and display the regression output.
#What are the RMSE and MAPE of the trend model based on the validation data? Discuss the overall performance of you model.


#computing rmse and mape values
rmse.lm.trend<-rmse(observed,predicted)
mape.lm.trend<-mape(observed,predicted)*100
print(c(rmse.lm.trend,mape.lm.trend))

#Fill in the table with your predictions for the following 12 months.
predicted<-predict(train.lm.trend, airline_topred)
predicted

#c) (2 pts) Is there evidence of some seasonal pattern in the sales data? If so, characterize the seasonal pattern (monthly, quarterly, or yearly).

#with QuarterIndex
train.lm.trend<- lm(Tickets ~ T+ Tsqrd +factor(QuarterIndex), data = train)
summary(train.lm.trend)
observed<-test$Tickets
predicted<-predict(train.lm.trend, test)

plot(airline$T, airline$Tickets, type ='l')
lines(train.lm.trendq$fitted, lwd=2)
lines(test$T, predicted, lwd= 2 , col= 'blue')

#residual
plot(fitted(train.lm.trend), resid(train.lm.trend))
abline(0,0)

rmse.lm.trend<-rmse(observed,predicted)
mape.lm.trend<-mape(observed,predicted)*100
print(c(rmse.lm.trend,mape.lm.trend))



#with MonthIndex ---------- best model
train.lm.trend<- lm(Tickets ~ T+factor(MonthIndex), data = train) 
summary(train.lm.trend)
observed<-test$Tickets
predicted<-predict(train.lm.trend, test)

plot(airline$T, airline$Tickets, type ='l')
lines(train.lm.trend$fitted, lwd=2)
lines(test$T, predicted, lwd= 2 , col= 'blue')

#residual
plot(fitted(train.lm.trend), resid(train.lm.trend))
abline(0,0)

rmse.lm.trend<-rmse(observed,predicted)
mape.lm.trend<-mape(observed,predicted)*100
print(c(rmse.lm.trend,mape.lm.trend))

#with Year Index

train.lm.trend<- lm(Tickets ~ T+ Tsqrd +Year, data = train)
summary(train.lm.trend)
observed<-test$Tickets
predicted<-predict(train.lm.trend, test)
plot(airline$ T, airline$Tickets, type ='l')
lines(train.lm.trend$fitted, lwd=2)
lines(test $ T, predicted, lwd= 2 , col= 'blue')

#residual
plot(fitted(train.lm.trend), resid(train.lm.trend))
abline(0,0)


rmse.lm.trend<-rmse(observed,predicted)
mape.lm.trend<-mape(observed,predicted)*100
print(c(rmse.lm.trend,mape.lm.trend))


#with MonthIndex with emponential
train.lm.trend<- lm(formula = log(Tickets) ~ T+ Tsqrd +factor(MonthIndex), data = train)
summary(train.lm.trend)
observed<-test$Tickets

predicted<-predict(train.lm.trend, test)



plot(airline$T, airline$Tickets, type ='l')
lines(exp(train.lm.trend$fitted), lwd=2)
lines(test$T, exp(predicted), lwd= 2 , col= 'blue')

#residual
plot(fitted(train.lm.trend), resid(train.lm.trend))
abline(0,0)

rmse.lm.trend<-rmse(observed,exp(predicted))
mape.lm.trend<-mape(observed,exp(predicted))*100
print(c(rmse.lm.trend,mape.lm.trend))


#d) (5+2+2= 9 pts) Build a regression model with trend and seasonality. To validate your model, use the last 12 months as a validation data set.



#Copy and paste your R code and display the regression output. (final_model)
attach(airline)
train.lm.trend<- lm(Tickets ~ T+factor(MonthIndex), data = train)

summary(train.lm.trend)
observed<-test$Tickets
predicted<-predict(train.lm.trend, test)



plot(airline$T, airline$Tickets, type ='l')
lines(train.lm.trend$fitted, lwd=2)
lines(test$T, predicted, lwd= 2 , col= 'blue')

#residual
plot(fitted(train.lm.trend), resid(train.lm.trend))
abline(0,0)

#What are the RMSE and MAPE of the trend model based on the validation data? Discuss the overall performance of you model.
rmse.lm.trend<-rmse(observed,predicted)
mape.lm.trend<-mape(observed,predicted)*100
print(c(rmse.lm.trend,mape.lm.trend))

#Fill in the table with your predictions for the following 12 months.

attach(airline_topred)
predicted2<-predict(train.lm.trend, airline_topred)
print(predicted2)

#Question 2 (25 points)

#The following data contains the annual revenue of a convenient store in thousand dollars.

#Our goal is to predict the revenue for the following 4 years.

#Model A:
  
#  a) (2 pts) Which exponential smoothing method would be the best (select one)?

#  Double Exponential smoothing ( level+ Trend)

#b) (5+2+2=9 pts) Build an appropriate exponential smoothing model (depending on your answer in part a). To validate your model, use the last 4 years as a validation data set.
attach(revenue)
revenue.ts <- ts(revenue$Revenue , start=c(1990), frequency = 1 ) # creating the timeseries dataset from the existing data
revenue.ts

plot(revenue.ts,col='blue') # plotting the time series data

train <- head(revenue.ts,19) #taking the top 19 rows/years as the train data 
test <- tail(revenue.ts,4) # taking the bottom 4 rows as the test dataset



train.Holt<- HoltWinters(train,   alpha= 0.4, beta = 0.03,gamma = FALSE) #with alpha and beta values. Tried series of values, and found these are the best values.
train.Holt

simple.pred <-forecast(train.Holt, h=8, level=1 )
simple.pred


plot(simple.pred, ylab="Revenue", bty='l', xlab="Year")
lines(simple.pred$fitted, col= 'red')
lines(test)

print(c(rmse(test,simple.pred$mean),mape(test,simple.pred$mean)))

revenue.Holt<- HoltWinters(revenue.ts,  alpha= 0.4, beta = 0.03,gamma = FALSE) # using the whole known data for the prediction rather than just the train data
revenue.Holt
prediction <-forecast(revenue.Holt, h=4, level= 0)


prediction #predicted values for the unknown 4 years

#Copy and paste your R code and display the regression output.
#What are the RMSE and MAPE of the trend model based on the validation data? Discuss the overall performance of you model.
#Fill in the table with your predictions for the following 12 months.

#Copy and paste your R code and display the regression output.


revenue$logrevenue<- log(revenue$Revenue)
revenue$logrevenue


revenue.ts <- ts((revenue$logrevenue) , start=c(1990), frequency = 1 )
revenue.ts
plot(revenue.ts,col='blue')

train <- head(revenue.ts,19)
test <- tail(revenue.ts,4)

train.Holt<- HoltWinters(train, gamma = FALSE)
train.Holt

simple.pred <-forecast(train.Holt, alpha= 0.4, beta = 0.03, h=8, level=1 )
simple.pred

plot(simple.pred, ylab="Revenue", bty='l', xlab="Year")
lines(test)
lines(simple.pred$fitted, col= 'red')

print(c(rmse(exp(test),exp(simple.pred$mean)),mape(exp(test),exp(simple.pred$mean))))


#What are the RMSE and MAPE of the trend model based on the validation data? Discuss the overall performance of you model.
#Fill in the table with your predictions for the following 12 months.
revenue.Holt<- HoltWinters(train,  alpha= 0.4, beta = 0.03,gamma = FALSE)
revenue.Holt
prediction <-forecast(revenue.Holt, h=4, level= 0)
exp(prediction$mean)

    