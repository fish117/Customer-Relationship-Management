
rm(list = ls())
library(data.table)

retention<-read.csv("retention_high.csv")

YEAR <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)
ACTUAL <- c(1000,869,743,653,593,551,517,491,468,445,427,409,394)

forecast <- as.data.table(cbind(YEAR,ACTUAL))

#FORECASTING USING LINEAR REGRESSION
linear <- lm(SURV_HIGH ~ YEAR, data=retention)
summary(linear)
forecast$linear <- predict(linear, forecast, type="response")

quadratic <- lm(SURV_HIGH ~ YEAR + I(YEAR^2), data=retention)
summary(quadratic)
forecast$quadratic <- predict(quadratic, forecast, type="response")

logmodel <- lm(SURV_HIGH ~ I(log(YEAR+1)), data=retention)
summary(logmodel)
forecast$logmodel <- predict(logmodel, forecast, type="response")

log.dv <- lm(log(SURV_HIGH) ~ YEAR, data=retention)
summary(log.dv)
forecast$log.dv <- exp(predict(log.dv, forecast, type="response"))



#HOW WELL DID WE DO?
plot(1,type='n',xlim=c(0,12),ylim=c(0,1000),xlab='YEAR', ylab='CUSTOMERS SURVIVED')
lines(forecast$YEAR,forecast$ACTUAL, col="black", lwd=2)
lines(forecast$YEAR,forecast$linear, col="blue", lwd=2)
lines(forecast$YEAR,forecast$quadratic, col="red", lwd=2)
lines(forecast$YEAR,forecast$logmodel, col="orange", lwd=2)
lines(forecast$YEAR,forecast$log.dv, col="green", lwd=2)

#lines(forecast$YEAR,forecast$log.dv, col="green", lwd=2)

abline(v=7, col="black")




#FORECASTING USING LOGISTIC REGRESSION

#DATA RESTRUCTURING
current.id<-1
id.nums.all<-c()
YEAR<-c()
defect.all<-c()

for (i in 1:7){

#i<-1  
  
number.died<-retention[i,2]-retention[i+1,2]

id.nums<-sort(rep(seq(from=current.id, to=current.id+number.died-1),i))

year<-rep(seq(from=1, to=i),number.died)

defect<-rep(c(rep(0,length=i-1), 1),number.died)

id.nums.all<-c(id.nums.all, id.nums)
YEAR<-c(YEAR, year)
defect.all<-c(defect.all, defect)

current.id<-current.id+number.died

}

num.censored<-491
id.nums.censored<-sort(rep(current.id:1000,7))
year.censored<-rep(1:7,num.censored)
defect.censored<-rep(rep(0,7),num.censored)

id.nums.all<-c(id.nums.all, id.nums.censored)
YEAR<-c(YEAR, year.censored)
defect.all<-c(defect.all, defect.censored)


  
  
retentionDiscrete<-as.data.frame(cbind(id.nums.all,YEAR,defect.all))



#Estimate discrete-time model 1 with intercept only

logit1 <- glm(defect.all ~ 1,  data=retentionDiscrete, family=binomial(link = logit))
summary(logit1)

forecast$logit1<-predict(logit1,forecast,type="response")
sum(retentionDiscrete$defect.all==1)/length(retentionDiscrete$defect.all)

forecast$logit1pred[1] <- 1000

for (i in 2:13){
  forecast$logit1pred[i] <- forecast$logit1pred[i-1] - (forecast$logit1pred[i-1]*forecast$logit1[i])
}

plot(1,type='n',xlim=c(0,12),ylim=c(0,1000),xlab='YEAR', ylab='CUSTOMERS SURVIVED')
lines(forecast$YEAR,forecast$ACTUAL, col="black", lwd=2)
lines(forecast$YEAR,forecast$logmodel, col="orange", lwd=2)
lines(forecast$YEAR,forecast$logit1pred, col="grey", lwd=2)
abline(v=7, col="black")



# Estimate discrete-time model 2 with time as a covariate

logit2 <- glm(defect.all ~ YEAR,  data=retentionDiscrete, family=binomial(link = logit))
summary(logit2)
forecast$logit2 <- predict(logit2, forecast, type="response")

forecast$logit2pred[1] <- 1000

for (i in 2:13){
  forecast$logit2pred[i] <- forecast$logit2pred[i-1] - (forecast$logit2pred[i-1]*forecast$logit2[i])
}


plot(1,type='n',xlim=c(0,12),ylim=c(0,1000),xlab='YEAR', ylab='CUSTOMERS SURVIVED')
lines(forecast$YEAR,forecast$ACTUAL, col="black", lwd=2)
lines(forecast$YEAR,forecast$logmodel, col="orange", lwd=2)
lines(forecast$YEAR,forecast$logit2pred, col="green", lwd=3)
abline(v=7, col="black")




