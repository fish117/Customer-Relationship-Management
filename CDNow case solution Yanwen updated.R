
#set working directory
library(data.table)
#install.packages("dplyr")
library(dplyr)

#load transaction data and retention data
CDNow<-fread("cdnow_students_transaction.csv")

str(CDNow)

#specify date format for dat column
CDNow$DATE<-as.Date(CDNow$DATE,format="%m/%d/%y")
CDNow[, DATE:=as.Date(DATE,format="%m/%d/%y")]
str(CDNow)
table(is.na(CDNow))

#split the data into calibration and validation samples at 9/30/97
date.cutoff<-as.Date(c("1997-09-30"))
CDNow.calibration<-CDNow[CDNow$DATE<=date.cutoff,]
CDNow.validation<-CDNow[CDNow$DATE>date.cutoff,]

# create new data frames that aggregate info at per ID level
# monetary = average spend
# frequency = number of separate orders
# lastpurchase = date of most recent purchase
# numcds = average number of CDs ordered
new.calibration <- CDNow.calibration[,list(monetary=mean(DOLLARS),
                                           frequency=length(DOLLARS),
                                           lastpurchase=as.Date(max(DATE)),
                                           recency=as.numeric(max(CDNow.calibration$DATE)-max(DATE)),
                                           numcds=sum(CDS)),
                                     by=c("ID")]
new.validation <- CDNow.validation[,list(monetary=mean(DOLLARS),
                                           frequency=length(DOLLARS),
                                           lastpurchase=as.Date(max(DATE)),
                                           recency=as.numeric(max(CDNow.validation$DATE)-max(DATE)),
                                           numcds=sum(CDS)),
                                     by=c("ID")]


#Merge calibration and validation samples into wide format where NAs in the validation indicate not being retained
new<-merge(new.calibration, new.validation, by = c("ID"), all.x = TRUE)
                                
#creates another column which returns a 1 if customer purchases in the validation period, 0 otherwise
new[,retained:=as.numeric(!is.na(new$monetary.y))]



# decile analysis
new[, monetaryquantile:=ntile(monetary.x,10)]
new[, recencyquantile:=ntile(recency.x,10)]
new[, frequencyquantile:=ntile(frequency.x,10)]
new[, table(monetaryquantile)]
new[, table(recencyquantile)]
new[, table(frequencyquantile)]

decileplot_monetary <- new[, list(monetaryquantile,monetaryretentionrate=mean(retained)),by=monetaryquantile]
decileplot_recency <- new[, list(recencyquantile,recencyretentionrate=mean(retained)),by=recencyquantile]
decileplot_frequency <- new[, list(frequencyquantile,frequencyretentionrate=mean(retained)),by=frequencyquantile]


decileplot_monetary[order(monetaryquantile),plot(monetaryquantile,monetaryretentionrate,type="b")]
decileplot_recency[order(recencyquantile),plot(recencyquantile,recencyretentionrate,type="b")]
decileplot_frequency[order(frequencyquantile),plot(frequencyquantile,frequencyretentionrate,type="b")]



#Linear regression
new.linearfit <-lm(retained ~ recency.x + frequency.x + monetary.x, data=new)
summary(new.linearfit)
new[, linearfit:=predict(new.linearfit, type="response")]

par(mfrow=c(4,1))
hist(new$linearfit)
plot(new$monetary.x,new$linearfit,col=3);#abline(lm(linearfit~monetary.x,data=new),col=2);
plot(new$frequency.x,new$linearfit,col=3);#abline(lm(linearfit~frequency.x,data=new),col=2);
plot(new$recency.x,new$linearfit,col=3);#abline(lm(linearfit~recency.x,data=new),col=2);


#monetaryprediction = expand.grid(recency.x=mean(new$recency.x),
#                                 frequency.x=mean(new$frequency.x),
#                                 monetary.x=seq(min(new$monetary.x),max(new$monetary.x)))
#monetaryprediction$linearfit <- predict(new.linearfit, monetaryprediction,type="response")
#hist(monetaryprediction$linearfit)
#plot(monetaryprediction$monetary.x,monetaryprediction$linearfit,col=3);#abline(lm(linearfit~monetary.x,data=new),col=2);


#logistic regression
new.logisticfit <- glm(retained ~ recency.x + frequency.x + monetary.x, data=new, family =binomial(link = "logit"))
summary(new.logisticfit)
new[, logisticfit:=predict(new.logisticfit, type="response")]

par(mfrow=c(4,1))
hist(new$logisticfit)
plot(new$monetary.x,new$logisticfit,col=3)
plot(new$frequency.x,new$logisticfit,col=3)
plot(new$recency.x,new$logisticfit,col=3)

monetaryprediction = expand.grid(recency.x=mean(new$recency.x),
                                 frequency.x=mean(new$frequency.x),
                                 monetary.x=seq(min(new$monetary.x),max(new$monetary.x)))
monetaryprediction$logisticfit <- predict(new.logisticfit, monetaryprediction,type="response")
hist(monetaryprediction$logisticfit)
plot(monetaryprediction$monetary.x,monetaryprediction$logisticfit,col=3,ylim=c(0,1))


recencyprediction = expand.grid(recency.x=seq(min(new$recency.x),max(new$recency.x)),
                                 frequency.x=mean(new$frequency.x),
                                 monetary.x=mean(new$monetary.x))
recencyprediction$logisticfit <- predict(new.logisticfit, recencyprediction,type="response")
hist(recencyprediction$logisticfit)
plot(recencyprediction$recency.x,recencyprediction$logisticfit,col=3,ylim=c(0,1))


frequencyprediction = expand.grid(recency.x=mean(new$recency.x),
                                frequency.x=seq(min(new$frequency.x),max(new$frequency.x)),
                                monetary.x=mean(new$monetary.x))
frequencyprediction$logisticfit <- predict(new.logisticfit, frequencyprediction,type="response")
hist(frequencyprediction$logisticfit)
plot(frequencyprediction$frequency.x,frequencyprediction$logisticfit,col=3,ylim=c(0,1))



# cumulative lift chart 
new[, monetaryquantile:=ntile(monetary.x,10)]
new[, recencyquantile:=ntile(recency.x,10)]
new[, frequencyquantile:=ntile(frequency.x,10)]
new[, table(monetaryquantile)]
new[, table(recencyquantile)]
new[, table(frequencyquantile)]

# by monetary dimension
cumlift_monetary <- new[, list(monetaryquantile,retained=sum(retained),n=length(retained)),by=monetaryquantile]
setorder(cumlift_monetary,-monetaryquantile)
cumlift_monetary[,cumlift:=(cumsum(retained)/cumsum(n))/(sum(retained)/sum(n))]
cumlift_monetary[,cumcustomerpt:=cumsum(n)/sum(n)]
cumlift_monetary[,plot(cumcustomerpt,cumlift,type="b")]
abline(h=1,col=2)

# by recency dimension
cumlift_recency <- new[, list(recencyquantile,retained=sum(retained),n=length(retained)),by=recencyquantile]
setorder(cumlift_recency,recencyquantile)
cumlift_recency[,cumlift:=(cumsum(retained)/cumsum(n))/(sum(retained)/sum(n))]
cumlift_recency[,cumcustomerpt:=cumsum(n)/sum(n)]
cumlift_recency[,plot(cumcustomerpt,cumlift,type="b")]
abline(h=1,col=2)

# by frequency dimension
cumlift_frequency <- new[, list(frequencyquantile,retained=sum(retained),n=length(retained)),by=frequencyquantile]
setorder(cumlift_frequency,-frequencyquantile)
cumlift_frequency[,cumlift:=(cumsum(retained)/cumsum(n))/(sum(retained)/sum(n))]
cumlift_frequency[,cumcustomerpt:=cumsum(n)/sum(n)]
cumlift_frequency[,plot(cumcustomerpt,cumlift,type="b")]
abline(h=1,col=2)


# gain chart
# by monetary dimension
gains_monetary <- new[, list(monetaryquantile,retained=sum(retained),n=length(retained)),by=monetaryquantile]
setorder(gains_monetary,-monetaryquantile)
gains_monetary[,gains:=cumsum(retained)/sum(retained)]
gains_monetary[,cumcustomerpt:=cumsum(n)/sum(n)]
gains_monetary[,plot(cumcustomerpt,gains,type="b",ylim=c(0,1),xlim=c(0,1))]
abline(0,1,col=2)

# by recency dimension
gains_recency <- new[, list(recencyquantile,retained=sum(retained),n=length(retained)),by=recencyquantile]
setorder(gains_recency,recencyquantile)
gains_recency[,gains:=cumsum(retained)/sum(retained)]
gains_recency[,cumcustomerpt:=cumsum(n)/sum(n)]
gains_recency[,plot(cumcustomerpt,gains,type="b",ylim=c(0,1),xlim=c(0,1))]
abline(0,1,col=2)

# by frequency dimension
gains_frequency <- new[, list(frequencyquantile,retained=sum(retained),n=length(retained)),by=frequencyquantile]
setorder(gains_frequency,-frequencyquantile)
gains_frequency[,gains:=cumsum(retained)/sum(retained)]
gains_frequency[,cumcustomerpt:=cumsum(n)/sum(n)]
gains_frequency[,plot(cumcustomerpt,gains,type="b",ylim=c(0,1),xlim=c(0,1))]
abline(0,1,col=2)
