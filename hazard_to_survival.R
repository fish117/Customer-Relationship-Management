
rm(list = ls())

library(data.table)
library(ggplot2)

retention<-fread("HighRegDiscrete.csv")


#From discrete-time data to survival function
data = retention[,list(defect=sum(defect),n=length(defect)),by=c("subscription","year")]

data[, hazardrate:=defect/n]
data[, cumdefect:=cumsum(defect),by=c("subscription")]
data[, survivalrate:=(max(data$n)-cumdefect)/max(data$n),by=c("subscription")]



# plot out the survival function
ggplot(data=data, aes(x=year, y=survivalrate, group=subscription)) +
  geom_line(aes(col=subscription))+
  geom_point()

# plot out the hazard rate (per period defection) function 
ggplot(data=data, aes(x=year, y=hazardrate, group=subscription)) +
  geom_line(aes(col=subscription))+
  geom_point()




#From discrete-time to model-based hazard function

mod1 <- glm(defect ~ as.factor(year) + as.factor(subscription),  data=retention, family=binomial(link = logit))
summary(mod1)

data$hazardrate_pred <- predict(mod1, data, type="response")
ggplot(data=data, aes(x=year, y=hazardrate_pred, group=subscription)) +
  geom_line(aes(col=subscription))+
  geom_point()


plot(1,type='n',xlim=c(0,8),ylim=c(0,0.5),xlab='YEAR', ylab='DEFECTION LIKELIHOOD (hazard rate)')
lines(data$year[1:7], data$hazardrate[1:7], type="o", col="blue", lwd=1)
lines(data$year[8:14], data$hazardrate[8:14], type="o", col="red", lwd=1)

lines(data$year[1:7], data$hazardrate_pred[1:7], type="o", col="blue", lwd=3)
lines(data$year[8:14], data$hazardrate_pred[8:14], type="o", col="red", lwd=3)




#From model-based hazard function to model-based survival function
data$survivalrate_pred[1]<- 1 - data$hazardrate_pred[1]
for (i in 2:7){
  data$survivalrate_pred[i] <- data$survivalrate_pred[i-1] - (data$hazardrate_pred[i]*data$survivalrate_pred[i-1])
}

data$survivalrate_pred[8]<- 1 - data$hazardrate_pred[8]
for (i in 9:14){
  data$survivalrate_pred[i] <- data$survivalrate_pred[i-1] - (data$hazardrate_pred[i]*data$survivalrate_pred[i-1])
}


plot(1,type='n',xlim=c(0,8),ylim=c(0,1),xlab='YEAR', ylab='PERCENT SURVIVORS')
lines(data$year[1:7], data$survivalrate[1:7], type="o", col="blue", lwd=1)
lines(data$year[8:14], data$survivalrate[8:14], type="o", col="red", lwd=1)

lines(data$year[1:7], data$survivalrate_pred[1:7], type="o", col="blue", lwd=3)
lines(data$year[8:14], data$survivalrate_pred[8:14], type="o", col="red", lwd=3)


