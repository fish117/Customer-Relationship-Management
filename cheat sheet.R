library(data.table)
library(dplyr)
library(visreg)
set.seed(12345)
library(MASS)
library(rgl)
library('proxy')

CDnow<-read.csv(file.choose())

str(CDnow)
CDNow<-CDnow
mydata<-CDnow
## convert to Date type

mydata$DATE<-as.Date(CDnow$DATE,format="%m/%d/%Y")
## filter by season 

datecutoff<-as.Date(c("0097-09-30"))
calibaration<-mydata[mydata$DATE<=datecutoff,]
validation<-mydata[mydata$DATE>datecutoff,]

#restructure
library(data.table)
library(dplyr)
ferequency<-aggregate(calibaration$DOLLARS,by=list(calibaration$ID),FUN=length)
colnames(ferequency)<-c("ID","frequency")
monetery<-aggregate(calibaration$DOLLARS,by=list(calibaration$ID),FUN=mean)
colnames(monetery)<-c("ID","monetery")
last_purchase<-aggregate(calibaration$DATE,by=list(calibaration$ID),FUN=max)
colnames(last_purchase)<-c("ID","last_purchase")
newdata<-merge(ferequency,monetery,by=c("ID"))
newdata<-merge(newdata,last_purchase,by=c("ID"))

enddate<-calibaration$DATE[which.max(calibaration$DATE)]
recency<-as.numeric(enddate-newdata$last_purchase)
newdata<-cbind(newdata,recency)
newdata<-merge(calibaration,newdata,by=c("ID"))
#
ferequency_val<-aggregate(validation$DOLLARS,by=list(validation$ID),FUN=length)
colnames(ferequency_val)<-c("ID","frequency")
monetery_val<-aggregate(validation$DOLLARS,by=list(validation$ID),FUN=mean)
colnames(monetery_val)<-c("ID","monetery")
last_purchase_val<-aggregate(validation$DATE,by=list(validation$ID),FUN=max)

colnames(last_purchase_val)<-c("ID","last_purchase")
newdata_val<-merge(ferequency_val,monetery_val,by=c("ID"))
newdata_val<-merge(newdata_val,last_purchase_val,by=c("ID"))

enddate_val<-validation$DATE[which.max(validation$DATE)]
recency<-as.numeric(enddate_val-newdata_val$last_purchase)
newdata_val<-cbind(newdata_val,recency)
newdata_val<-merge(validation,newdata_val,by=c("ID"))
newdata_val$recency_val<-NULL

##merge two samples
dt<-merge(newdata,newdata_val,by=c("ID"),all=TRUE)
str(dt)
dt$retained=as.numeric(!is.na(dt$frequency.y))


#ntile
dt$monetery.x.quantile<-ntile(dt$monetery.x,10)
dt$frequency.x.quantile<-ntile(dt$frequency.x,10)
dt$recency.x.quantile<-ntile(dt$recency.x,10)
table(dt$monetery.x.quantile)
table(dt$recency.x.quantile)
table(dt$frequency.x.quantile)
# by monetary dimension
cumlift_monetary <- new[, list(monetaryquantile,retained=sum(retained),n=length(retained)),by=monetaryquantile]
setorder(cumlift_monetary,-monetaryquantile)
cumlift_monetary[,cumlift:=(cumsum(retained)/cumsum(n))/(sum(retained)/sum(n))]
cumlift_monetary[,cumcustomerpt:=cumsum(n)/sum(n)]
cumlift_monetary[,plot(cumcustomerpt,cumlift,type="b")]
abline(h=1,col=2)
str(dt)
#monetary
library(data.table)
library(dplyr)
cumlift_monetary<-dt[,list(dt$monetery.x.quantile,retained=sum(dt$retained),n=length(retained)),by=monetery.x.quantile]
cumlift_monetary<-aggregate(dt$monetery.x.quantile,by=list(dt$monetery.x.quantile),FUN=sum)
n<-aggregate(dt$retained,by=list(dt$monetery.x.quantile),FUN=sum)
colnames(n)<-c("monetaryquantile","retained")
colnames(cumlift_monetary)<-c("monetaryquantile","cumlift_monetary")
l<-aggregate(dt$retained,by=list(dt$monetery.x.quantile),FUN=length)
colnames(l)<-c("monetaryquantile","n")
cumlift_monetary<-merge(cumlift_monetary,n,by=c("monetaryquantile"))
cumlift_monetary<-merge(cumlift_monetary,l,by=c("monetaryquantile"))
setorder(cumlift_monetary,monetaryquantile)
cumlift<-(cumsum(cumlift_monetary$retained)/cumsum(cumlift_monetary$n))/(sum(cumlift_monetary$retained)/sum(cumlift_monetary$n))
cumlift_per<-cumsum(cumlift_monetary$n)/sum(cumlift_monetary$n)

plot(cumlift_per,cumlift,type="b")
abline(h=1,col=2)

gains<-cumsum(cumlift_monetary$retained)/sum(cumlift_monetary$retained)
plot(cumlift_per,gains,type="b")
abline(h=1,col=2)

#recency 
#create a column called "recency" which is a numeric variable that represents how many days since the most recent purchase.
#A good way to do this is to use the last date in the whole calibration sample as an end date. 
#So a purchase on that date would have a recency score of 0.
#when merge two datasets,all=TRUE, so they will keep NA

#Cumulative Lifts: How much of a bump in my response rate do I get by targeting fewer people?
#Cumulative Gains:  (aka Banana Chart)As I target more people how rapidly do I get most of the responders?

################ Linear Regression Model############

#####################################################


#the total distance from any point to Y(mean) is the sum of the point  to regression line and the regression line to the y(mean)
#Variability in Y not accounted for by your model per remaining parameter
#i.e., MSE = SSR / (n - p - 1), with n = # observations, and p = # additional parameters
SST<- sum(( mydata2$sales - mean( mydata2$sales))^2)

SSE <- sum(( mydata2$sales - mydata2$predict1)^2)

SSR<- sum(( mydata2$predict1 - mean( mydata2$sales))^2)

#RSE=SQRT(MSE)

###weekday
baseball$weekday <- weekdays(baseball$event_date)
summary(baseball$weekday)
table(baseball$weekday)

baseball$weekday <-as.factor(baseball$weekday)

## convert to Date type
baseball$event_date <- as.Date(baseball$event_date,"%Y-%m-%d")
#date before 2016
data=baseball[baseball$season<2016,]

#density plot
plot(density(baseball$attendance))

#df=number of observations minus the number of parameters in your model
#Examples:
 # For a model with only an intercept: n - 1
#For a model with an intercept and a slope: n - 2
#For a model with an intercept and two slopes: n - 3

#R^2=1-SSR/SST
#adjusted R^2= 1-(SSR/N-P-1)/(SST/N-1)

#f statistics is comparing this model with the baby model(with only interception)
# if p-value<0.05 means this model is better than the baby model
#y=a+b1*x
#linear model: 1 unit incerase in X will lead to b1 unit increase in Y
#log-linear model: 1 unit incerase in X will lead to b1 percent increase in Y
#log-log model:1 % incerase in X will lead to b1 % increase in Y

###############missing value###########

################################################

#Casewise deletion
#data[complete.cases(data),]

#Discard observation if any one of the variables in the observation is missing
#Undesirable if entire sample size is small and when each observation has a lot of variables
#Biased results if characteristics of discarded observations are different from those of the remaining observations


#Single imputation
#ifelse(is.na(variable),mean(variable,na.rm=T),variable)

#Simplest form is mean substitution, but can also use patterns in the non-missing data to impute a suitable value for the missing response (e.g., using regression)
#Ignores uncertainty on the predictions of the unknown missing values

#Missing variable dummies
#create a missing variable dummy to signify that the variable is missing for a given customer
#This technique can also be used after single or multiple imputation. 
#The extra dummy takes on the value of 1 if the observation is imputed and 0 if the observation is complete.
#If the imputation procedure is unbiased, the coefficient associated with the dummy variable will be estimated to be 0. 
#If the coefficient is not zero, the imputation method does not capture the pattern of missing data appropriately.

############CLV calculation################

#################################################


#CLV Definition: the net present value of the future cash flows attributed to the customer relationship
#if start from 0 and discount rate=1/(1+d)
#CLV =m*[1+r/(1+d)+(r/(1+d))^2+.....]
 # =M*(1+d)/(1+d-R) - AC

#if the customer does not appear on the firm's trader until after first payment .
#CLV=mr/(1+d)*[1+(r/(1+d)+....]

#there is no difference with scenario 1, just received the CF at the end of period 1
#CLV=m/(1+d-r)-AC

# let's try to get the CLV according to Case 1
M = 100
R = .8
AC = 200
d = .1
CLV=M*(1+d)/(1+d-R) - AC
CF=rep(0,100)
NPV=rep(0,100)
NPV[0]=0
# Let's verify by listing the infinite series
for (i in 1:100){
  CF[i]=M*(R/(1+d))^(i-1)
  NPV[i]=sum(CF[1:i])-AC
}
# print out the discounted cash flow matrix 
plot(CF,type="b")

# print out npv to see since which period the npv>=0
plot(NPV,type="b")
# an alternative to figure out since which period the npv>=0
which.max(NPV>0)


#limitation of CLV: if retention rate=1 and discount rate=0, we cannot assume the infinite time horizon since the 
#CF will not become 0.

#Average retention rates are almost always increasing because loyal customers 
#(i.e., those with higher retention rates) are more likely to stick around. 





########### Monte Carol Simulation#############

#############################################

ac=seq(40000,60000,5000)
am <- seq(from = 20000, to = 40000, by = 5000)
rr <- seq(from = 0.30, to = 0.90, by = 0.12)
values<-expand.grid(ac,am,rr)
values$ir=0.1
#=M*(1+d)/(1+d-R) - AC
values$CF=values$am*(1+values$ir)/(1+values$ir-values$rr)-values$ac

#some descriptives
negativepercent<-sum(values$CF<0)/length(values$CF)
scatter.smooth(values$ac,values$CF)
scatter.smooth(values$am,values$CF)
scatter.smooth(values$rr,values$CF)

mydata<-read.csv(file.choose())
maru.data<-mydata
maru.data$clv=rep(0,5)
maru.data$clv = (maru.data$annual.margin* ((1+maru.data$interest.rate) / (1 + maru.data$interest.rate - maru.data$retention.rate))) - maru.data$acquisition.cost
elite.ballplayers<-maru.data[4 ,]

hist(customers,breaks=100)
margin.mean<-mean(customers)
margin.sd<-sd(customers)
d<-.1
ac<-50000
m<-30000
r<-0.6

# scenario 1:
num.samples=10000

d.vec<-rep(d, num.samples)
ac.vec<-rep(ac, num.samples)
m.vec<-rnorm(num.samples,margin.mean,margin.sd)
a<-5
b<-a*(1-r)/r
b
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate a=5 b=3.3")
#CLV = (M * ( (1+d) / (1 + d - r))) - AC
clv.vec<-(m.vec*(1+d.vec)/(1+d.vec-r.vec))-ac.vec
hist(clv.vec)

#########whale plot###################

whale.data<-as.data.frame(-sort(clv.vec))
colnames(whale.data)<-"clv.ordered"
whale.data$clvpercent<-(whale.data$clv.ordered)/sum(whale.data$clv.ordered)
whale.data$cumsumpercent<-cumsum(whale.data$clvpercent)
whale.data$ncustomers<-seq(1,10000,1)
whale.data$percentcustomers<-whale.data$ncustomers/10000
plot(whale.data$clvpercent,whale.data$percentcustomers)
plot(whale.data$percentcustomers,whale.data$clv.ordered, type="l",
     main="Whale Curve for Maru Batting",
     xlab = "Percent of Customers", ylab = "Percent of Aggregate CLV")
abline(h=0,col=2)


#############profitability skew##########

###########################################
#SET WORKING DIRECTORY
library(data.table)
pilgrim<-read.csv(file.choose())
pilgrim_whaleplot=setorder(pilgrim,-Profit99)
pilgrim_whaleplot$profitpt=cumsum(pilgrim_whaleplot$Profit99)/sum(pilgrim_whaleplot$Profit99)
cus=seq(1,31634,1)
pilgrim_whaleplot$customerpercentil=cus/31634
plot(pilgrim_whaleplot$customerpercentil,pilgrim_whaleplot$profitpt,type="l")
abline(h=1,col="red")
pilgrim_whaleplot$customerpercentil[which.max(pilgrim_whaleplot$profitpt)]
abline(h=pilgrim_whaleplot$profitpt[which.max(pilgrim_whaleplot$profitpt)],col="red")
abline(v=pilgrim_whaleplot$customerpercentil[which.max(pilgrim_whaleplot$profitpt)],col="red")
pilgrim_whaleplot$customerpercentil[which.max(pilgrim_whaleplot$profitpt>=1)]
pilgrim_whaleplot$profitpt[which.max(pilgrim_whaleplot$customerpt>=.1)]  # 69.18 percent


#linear regression
mod1 <- lm(Profit99~1, data=pilgrim)
#interpreation of interception: The average customer profitability for 1999 was $111.50
confint(mod1,level=0.95)

#interpretation:It can be estimated with 95% confidence level that 95% of average customer profitability was
#between $108.49 and $114.50 in 1999

pilgrim$Age99<-ifelse(is.na(pilgrim$Age99),mean(pilgrim$Age99,na.rm=TRUE),pilgrim$Age99)
pilgrim$Inc99<-ifelse(is.na(pilgrim$Inc99),0,pilgrim$Inc99)
pilgrim$miss_Profit00<-as.numeric(is.na(pilgrim$Profit00))
pilgrim$Profit00<-ifelse(pilgrim$miss_Profit00==0,pilgrim$Profit00,mean(pilgrim$Profit00,na.rm=TRUE))

model5<-lm(Profit99 ~Online99+as.factor(Age99)+as.factor(Inc99)+as.factor(District99)+Tenure99+Billpay99, data=pilgrim)
summary(model5)
library(visreg)
visreg(model5,"Age99")
visreg(maineffects,"minutes.spent",by="frequentflyerprogram.numeric",overlay=TRUE)

scatter.smooth(pilgrim$Age99,pilgrim$Profit99,)
boxplot(Profit99~Age99,data=pilgrim)
mosaicplot(Profit99~Age99,data=pilgrim)


# to test the significance of the interaction term

#interaction model

interaction <- lm(spend ~ frequentflyerprogram.numeric + minutes.spent + frequentflyerprogram.numeric:minutes.spent, data=all.data)
summary(interaction)
visreg(interaction, "minutes.spent", by="frequentflyerprogram.numeric", overlay ="TRUE")



#Interaction model with frequent flyer coded the other way

all.data$frequentflyerprogram.recoded<-abs(all.data$frequentflyerprogram.numeric-1)
interaction.recoded <- lm(spend ~ frequentflyerprogram.recoded + minutes.spent + frequentflyerprogram.recoded:minutes.spent, data=all.data)
summary(interaction.recoded)
visreg(interaction.recoded, "minutes.spent", by="frequentflyerprogram.recoded", overlay ="TRUE")


#Interaction model with mean-centered time spent

all.data$minutes.spent.centered<-all.data$minutes.spent-mean(all.data$minutes.spent)
interaction.centered <- lm(spend ~ frequentflyerprogram.numeric + minutes.spent.centered + frequentflyerprogram.numeric:minutes.spent.centered, data=all.data)
summary(interaction.centered)
visreg(interaction.centered, "minutes.spent.centered", by="frequentflyerprogram.numeric", overlay ="TRUE")
mean(all.data$minutes.spent)


#############quadratice model
quadratic <- lm(SURV_HIGH ~ YEAR + I(YEAR^2), data=retention)
summary(quadratic)
#logmodel
logmodel <- lm(SURV_HIGH ~ I(log(YEAR+1)), data=retention)
summary(logmodel)
forecast$logmodel <- predict(logmodel, forecast, type="response")

log.dv <- lm(log(SURV_HIGH) ~ YEAR, data=retention)
summary(log.dv)
forecast$log.dv <- exp(predict(log.dv, forecast, type="response"))
#############logit model and hit rate#############
##########################################


## now a logit model
modb2 <- glm(win~braves_payroll+opponent_payroll, family=binomial(link="logit"), 
             data=baseball)
summary(modb2)


pred_win2016 <- predict(modb2, baseball[baseball$season==2016,], type="response")

## now use 0.5 as the cut off for predicting win or loss
pred_win2016_binary <- as.numeric(pred_win2016>0.5)
real_win2016_binary <- baseball$win[baseball$season==2016]

## create a table to display the fit
table(real_win2016_binary, pred_win2016_binary)

############survival and hazard###############
###############################################


library(data.table)
education<-read.csv(file.choose())
education<-data.table(education)
education_agg<-education[,list(defect=sum(cancelnow),n=length(custid)),by=c("startlen","time")]

education_agg[,cumdefect:=cumsum(defect),by=c("startlen")]
education_agg[,initial:=max(n),by=c("startlen")]
education_agg[,hazard.rate:=defect/n]
education_agg[,survival.rate:=1-cumdefect/initial,by=c("startlen")]


# plot out the survival function
ggplot(data=education_agg, aes(x=time, y=survival.rate, group=as.factor(startlen))) +
  geom_line(aes(col=as.factor(startlen)))+
  geom_point()

#From discrete-time to model-based hazard function

mod1 <- glm(defect ~ as.factor(year) + as.factor(subscription),  data=retention, family=binomial(link = logit))
summary(mod1)


#From model-based hazard function to model-based survival function
data$survivalrate_pred[1]<- 1 - data$hazardrate_pred[1]
for (i in 2:7){
  data$survivalrate_pred[i] <- data$survivalrate_pred[i-1] - (data$hazardrate_pred[i]*data$survivalrate_pred[i-1])
}



retention<-data.table(retention)
is.data.table(retention)
data=retention[,list(defect:=sum(defect),n:=length(defect)),by=c("id","subscription")]
data[,cumdefect:=cumsum(defect),by=c("subscription")]
data[,hazard:=defect/n]
data[,survival:=(max(n)-cumdefect)/max(n),by=c("subscription")]
#avergae survival rate for each group
# summary how much should the company spend on incentive to convert one-month subscription to twelve-month contracts

education_agg[,list(survival.rate=mean(survival.rate)),by=c("startlen")]
education_agg[,list(hazard.rate=mean(hazard.rate)),by=c("startlen")]

education[,cumlag:=cumsum(lagtest),by=c("startlen")]
education[,contractup:=ifelse(payleft==0,1,0)]


#show the requirement for factor variable:I(lagtest > 0)TRUE
mod1 <- glm(cancelnow ~ time + I(lagtest>0) + lagtest + cumlagtest + payleft + contractup + as.factor(startlen)  + time*as.factor(startlen) ,  data=education, family=binomial(link = logit))
summary(mod1)
education$hazard_pre<-predict(mod2,education,type="response")
education[,cancelnow:=as.numeric(hazard_pre>0.5)]
education[,prop.table(table(cancelnow,cancelnow_pred))]

education_pre<-education[,list(hazard_pre=mean(hazard_pre)),by=c("startlen","time")]

#use hazard to predict survival
#From model-based hazard function to model-based survival function
education_agg$survivalrate_pred=0

education_agg[startlen==1,]$survivalrate_pred[1] <- 1- education_agg[startlen==1,]$hazardrate_pred[1]
education_agg[startlen==1,]$survivalrate_pred[1]<- 1 - education_agg[startlen==1,]$hazardrate_pred[1]
for (i in 2:11){
  education_agg[startlen==1,]$survivalrate_pred[i] <- education_agg[startlen==1,]$survivalrate_pred[i-1] - (education_agg[startlen==1,]$hazardrate_pred[i]*education_agg[startlen==1,]$survivalrate_pred[i-1])
}


#######loyalty program############

############################################

#Incremental Revenue
#500,000 customers opt into the program in year 1
#An increase in concession RPG by 15 per cent per visit
#Incremental attendance of one visit per member
#Cannibalization of 25 per cent
#Assume concession RPG $3.44 and ticket RPG $7.73 (Exhibit 2)

#RPG increase = $3.44 concession RPG * 15% * (7.5 visits +1 visit)  + $11.17 total RPG * 1 visit 

#Revenue increase= RPG increase * 500,000 members * 75% non-cannibalization= $5,833,500
#Given that the average visit per year is 7.5, even after 1 increase in visit, each member will earn about 850 points per year
#Reward cost: Of 200,000 members redeeming (500,000 members * 40%redeemed), 50 percent will redeem level 1 ($8.50 face value with $4.39 margin cost), another 50 percent will redeem level 2 ($12.37 face value with $4.33 margin cost)  =  $872,000 

#Incremental cost of loyalty program = $4,372,000

#Percent Retail Value
#Reward value/(Customer spend per points transaction *number of visits required to obtain reward level)*100%

#Example
#Reward structure No.1 requires five transaction of 100 points each to reach the 500-point reward level, which has a retail value of $8.50. Each 100-point transaction will cost the customer $10.95. Therefore
#$8.5/($10.95*5)*100% = 15.5%
  


##############linear regression#############

############################################


#probability function:mapping an event of Pr in (0,1)
#logit model will model the nonlinear transformation of Pr 
#odds ratio
exp(cbind(Odds_and_OR=coef(modb1), confint(modb1)))
#odds=Pr(y=1)/Pr(y=0)

#log(pi/(1-pi))=a+b1*x1+b2*x2+.....
#Pr=exp(a+b1*x1+b2*x2+.....)/(1+exp(a+b1*x1+b2*x2+...3..))
#the probability that y=1 is a nonlinear function of predictor variables(x1,x2.....)
#logit(p) or log-odds is a linear function of predictor variables(y1,y2....)
#a 1 unit change in x produces a change of b in log-odds
exp(0.678194)/(1+exp(0.678194))
###############cluster################

#######################################

library(data.table)
all<-read.csv(file.choose())
all<-data.table(all)
all[,missing:=ifelse(complete.cases(all),1,0)]
all[,table(missing)]
all[,movie:=rowMeans(all[,2:51],na.rm=T)]
t.test(movie~missing,data=all)


proportion<-sort(colMeans(movies),decreasing = T)
barplot(proportion,names.arg = names(proportion),las=2)
hist(rowSums(movies),breaks=100)
barplot(sort(rowSums(movies),decreasing=T))
abline(h=median(rowSums(movies)),col="red")
abline(v=which.max(sort(rowSums(movies))>mean(rowSums(movies))))
which.max(sort(rowMeans(movies))>mean(rowMeans(movies)))
summary(rowMeans(movies))
#99 out of 178 indivdiuals have watched more than 37.5% movies.
#the data is skewed on the heavy movie watching side  



# Determine initial number of clusters based on Within-cluster sum of squares
wss<-c()
bss <- c()
wssbss <- c()
pseudoF <- c()

max.clusters = 15
for (i in 2:max.clusters){ 
  kmeansi <- kmeans(movies,centers=i,nstart=15, iter.max=50)
  wss[i] <- sum(kmeansi$withinss)
  bss[i] <- kmeansi$betweenss
  wssbss[i] <- wss[i]/bss[i]
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(movies)-i)) 
}
wss
bss
wssbss
pseudoF


plot(2:max.clusters, wss[2:max.clusters], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:max.clusters, bss[2:max.clusters], type="b", xlab="Number of Clusters",ylab="between groups sum of squares") 
plot(2:max.clusters, wssbss[2:max.clusters], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(2:max.clusters, pseudoF[2:max.clusters], type="b", xlab="Number of Clusters",ylab="Pseudo F") 


#Ordinary K-means
num.clusters<-3
set.seed (12345)
movies.k3<-kmeans(movies, num.clusters,nstart=50)

# plot center
points(X,pch=X.km$cluster+1, col=X.km$cluster+1)
points(X.km$centers, col=2:3, pch=2:3, cex=1.5)


k3.means <- t(movies.k3$centers)
colMeans(k3.means) 

k3.means[order(-k3.means[,1]),] #Cluster 1 has seen 40% of movies with 65 individuals. Classic movies.
k3.means[order(-k3.means[,2]),] #Cluster 2 has seen 26% of movies with 80 individuals, Mostly fantasy/sci-fi
k3.means[order(-k3.means[,3]),] #Cluster 3 has seen 62% of movies with 33 individuals, Heavey watchers. Smaller cluster.
demos<-data.table(demo)
demos[,cluster:=movies.k3$cluster]
aggregate(demos[,1:6],by=demos[,7],FUN=summary)
# center 3 care more about the price, center 2 their parents like drama,center 2 will consider the distance


theater.k3$size           #displays size of clusters,small ize difference is better.

pie(theater.k3$size, radius=1, main="Cluster Size Pie", clockwise=TRUE, col=rainbow(theater.k3$cluster+1))

theater.k3$totss          #displays total sum of squares
theater.k3$withinss       #displays within-cluster sum of squares
theater.k3$betweenss      #displays between-cluster sum of squares
sum(theater.k3$withinss)+theater.k3$betweenss






demos$cluster1 <- ifelse(movies.k3$cluster==1,1,0)
demos$cluster2 <- ifelse(movies.k3$cluster==2,1,0)
demos$cluster3 <- ifelse(movies.k3$cluster==3,1,0)

# DD CLUSTER COLUMN TO DATA
theater$cluster <- theater.k3$cluster


logit1 <- glm(cluster1 ~ age + edunew + log(incnew) + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit1)
demos$cluster1prob <- predict(logit1, demos, type="response") 
#cluster 1 tends to be older,because the efficient is positive


### BONUS: SPHERICAL K-MEANS (COSINE DISTANCE) ###
library(skmeans)#if you don't have it installed use install.packages("skmeans")
library(cluster)
library(clue)

remaining.disimilarity <- numeric()
#Add within sum of squares for other clusters
max.clusters<-15
for (i in 2:max.clusters) remaining.disimilarity[i-1] <- 1 - as.numeric(cl_validity(skmeans(as.matrix(movies),i))["Dissimilarity accounted for"])
plot(2:15, remaining.disimilarity, type="b", xlab="Number of Clusters", ylab="Remaining Disimilarity")


#try 4 clusters?
movies.sk4<-skmeans(as.matrix(movies), 4)

table(movies.sk4$cluster)

sk4.means<-t(aggregate(movies,by=list(movies.sk4$cluster),mean))
sk4.means<-sk4.means[-c(1), ]              

colMeans(sk4.means) #Get % movies watched by cluster; more balanced than ordinary K-means
#[1] 0.3462963 0.4164516 0.5096296 0.2422857

#RESULTS ARE UNSTABLE?
sk4.means[order(-sk4.means[,1]),] #Cluster 1 has seen 35% of movies, fantasy on the top
sk4.means[order(-sk4.means[,3]),] #Cluster 3 has seen 51% of movies, sci-fi on the top
sk4.means[order(-sk4.means[,2]),] #Cluster 2 has seen 42% of movies, classic older movies, heavy watchers 
sk4.means[order(-sk4.means[,4]),] #Cluster 4 has seen 24% of movies, classic older movies, light watchers

mosaicplot(age~cluster,data=theater,col=c("limegreen","black","yellow2","steelblue","indianred"), xlab="Age",ylab="Cluster",main="Cluster vs. Age Mosaic")

install.packages("doBy")   
library(doBy)

summaryBy(age+educ~cluster,FUN=list(mean,sd),na.rm=TRUE,data=theater)

boxplot(attitude~cluster,data=theater, col=clr1, xlab="Cluster", ylab="Attitude", main="Clustertude Box Plot")

# can we use demographics to predict the identity of the clusters?
# try logistic regression
logit1 <- glm(I(cluster==1) ~ age + educ + income + cnty,  data=theater, family=binomial(link = logit))
summary(logit1)
theater$cluster1prob <- predict(logit1, theater, type="response")

plot(theater$educ, theater$cluster1prob)
plot(theater$age, theater$cluster1prob)
plot(theater$income, theater$cluster1prob)

## try multinomial regression
library(nnet) ## neural net library needed
##full model multinomial regression
mod1 <- multinom(cluster ~ age + educ + income + cnty, data=theater)
summary(mod1)


k4.clustermeans[order(-k4.clustermeans[,8]),]

library(skmeans)
wine.cos5<-skmeans(wine.transposed, 5,method="genetic")
#######removel effect################

######################################
#Probability of conversion after removing C1 
#= P(C2 -> C3 -> Convert) = .33*1*.5 =.165

#Probability of conversion before removing C1 
#= P(C1 -> C2 -> C3 -> Conversion) + P(C2->C3->Conversion) 
#= .66*.5*1*.5 + .33*1*.5
#= .33

#Removal effect 50%

#########last click matrix#########

###################################

#Applying the last-click metric, the firm would attribute 50% of the conversions to direct channel and 25% each to display and search advertising. 

#However, the last-click metric ignores the prior channel contribution
#only consider the current effect throgh last channel

#First-click metric assigns the credit to the first touch

#Linear (uniform) metric assigns the credit to all the channels equally
#Markov chain based matrix
#State space
#Transition matrix
#Current state probability distribution



###########distance###################

######################################

#Minkowski distance metrics grow with number of dimensions and are unbounded.

#Correlation and Cosine are bounded.

#Cosine is usually used for sparse data with lots of zeros. The cosine ignores 0-0 matches. 

#Cosine should only be used when there is a true and meaningful zero due to scale sensitivity. 

#Correlation is invariant to the absolute distances between sets of data.

#if different people use different scale,it's better to use Pearson, it will compare the data after scaling
#if almost all attributes in the data has non zero values, and the magnititude is important,it's better to use Euclidean or Manhattan distance
#if the data is sparse, we should use consine distance


#User-Based Filtering Algorithm

#Calculate similarity of ratings for target customer to all other customers.

#Sort customers based on similarity to target customer. 

#Choose a subset of "k nearest neighbors."

#Calculate predicted ratings using weighted averaging.

#Recommend items with high predicted scores that have not been rated/purchased yet.

# the drawback of User-Based Filtering Algorithm
#Computational demands as user-base grows. Typically the data have more customers than items. 

#Sparse data. Little overlap between any two customers. 

#Segments should:
#be internally homogenous and distinctive in a strategically meaningful way
#be operational: measurable and accessible 
#have effective demand


#K means: to minimize the within cluster distance which is to maximize the distance between cluster
#Manhattan distance (city block)

#Distance between a customer and a cluster center is calculated by summing the difference between the two points for each dimension

#(Squared) Euclidean distance (as-the-crow-flies)
#Distance between a customer and a cluster center is calculated by taking the difference between the two points for each dimension, squaring the differences, and summing them up.


######inverse order####
index.list=c()
for(i in (1:11104)){
  for (m in (12:2)){
    if (donation[i,m]==1){
      index.list=append(index.list,12-m)
      
      break}
  }
}



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

index.list[1:10]
median(index.list)
