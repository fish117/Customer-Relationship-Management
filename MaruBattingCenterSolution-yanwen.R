
# Assignment solultion to Maru Batting prepared by Yanwen WANG


# SET WORKING DIRECTORY
# READ DATA
maru.data<-read.csv("maru_data.csv")



# Answers to Part I
# CALCULATE ANNUAL MARGIN
maru.data$total.cost.per.hr<-maru.data$instructor.labor.cost.per.hr* maru.data$instructors.needed + maru.data$worker.labor.cost.per.hr* maru.data$workers.needed
maru.data$margin.hr<-maru.data$price.per.hr-maru.data$total.cost.per.hr
maru.data$annual.margin<-maru.data$margin.hr*maru.data$annual.hours


# answer to question 1 on page 5
# CALCULATE ACQUISITION COST
maru.data$acquisition.cost<-maru.data$contact.cost/maru.data$response.rate
maru.data$acquisition.cost


# answer to question 2
# BREAKEVEN WITHOUT DISCOUNTING
maxperiod = 6
period = seq(1,maxperiod)

cf = matrix(0,nrow=5,ncol=maxperiod)
npv = matrix(0,nrow=5,ncol=maxperiod)

for (t in 1:maxperiod){
  cf[,t] = maru.data$annual.margin*maru.data$retention.rate^(t-1)
  npv[,t] = rowSums(as.matrix(cf[,1:t])) - maru.data$acquisition.cost
}

# print out cash flow matrix 
cf
# print out npv to see since which period the npv>=0
npv
# you can try the following instead of eyeballing
apply(npv,1, function(x) which.max(x>=0))



# PROBLEM 3: COMPUTE CLV (ASSUMING INFINITE TIME HORIZON)
#CLV = (M * ( (1+D) / (1 + D - R))) - AC Using formula #1
#Create a new column with CLV assuming numbers from case
maru.data$clv <- (maru.data$annual.margin* ((1+maru.data$interest.rate) / (1 + maru.data$interest.rate - maru.data$retention.rate))) - maru.data$acquisition.cost

# if you use formula 3...
# maru.data$clv <- (maru.data$annual.margin / (1 + maru.data$interest.rate - maru.data$retention.rate)) - maru.data$acquisition.cost

maxperiod = 6
period = seq(1,maxperiod)

cf = matrix(0,nrow=5,ncol=maxperiod)
npv = matrix(0,nrow=5,ncol=maxperiod)

for (t in 1:maxperiod){
  cf[,t] = maru.data$annual.margin*maru.data$retention.rate^(t-1)/(1+.1)^(t-1)
  npv[,t] = rowSums(as.matrix(cf[,1:t])) - maru.data$acquisition.cost
}

# print out cash flow matrix 
cf
# print out npv to see since which period the npv>=0
npv


# PROBLEM 5: CHIYODA AWARD
clv.littleleaguers.now<-maru.data[maru.data$X=="little leaguers",]$clv
clv.littleleaguers.chiyoda<-5000*(1+.1)/(1+.1-.65)-(600/.08)
clv.littleleaguers.now
clv.littleleaguers.chiyoda



# PROBLEM 6: ELITE BALLPLAYERS DISCOUNT
clv.eliteballplayers.now<-maru.data$clv[which(maru.data$X=="elite ballplayers (party)")]
clv.eliteballplayers.discount<-((7000-6000)*20)*(1+.1)/(1+.1-.75)-(50000)+500*20


# PROBLEM 7: ELITE BALLPLAYERS BAT
clv.eliteballplayers.bat<-((7500-6000)*20)*(1+.1)/(1+.1-.6)-(12500/.29+10000)



# Answers to Part II
# SENSITIVITY ANALYSIS

#creates scenario values
ac <- seq(from = 40000, to = 60000, by = 5000)
am <- seq(from = 20000, to = 40000, by = 5000)
rr <- seq(from = 0.30, to = 0.90, by = 0.12)

#Matrix of all pairwise comparisons

values <- expand.grid(ac=ac,am=am,rr=rr)

#adds constant interest rate column
values$ir <- 0.10

#computes CLV for all scenarios
values$clv <- values$am * ( (1+values$ir) / (1 + values$ir - values$rr)) - values$ac

#write.csv(values,"sensitivity.csv",row.names=F)


#some descriptives
negativeclvpercent = sum(values$clv < 0)/ length(values$clv)
negativeclvpercent

worsethanlittleleaguerspercent=sum(values$clv < maru.data$clv[1])/length(values$clv)
worsethanlittleleaguerspercent

#visualization using scatter plot
par(mfrow=c(1,3))
scatter.smooth(x=values$ac, y=values$clv)
scatter.smooth(x=values$am, y=values$clv)
scatter.smooth(x=values$rr, y=values$clv)



# Answers to Part III

elite.ballplayers<-maru.data[4 ,]
elite.ballplayers.subset<-subset(elite.ballplayers, select = c("acquisition.cost","annual.margin","retention.rate"))
elite.ballplayers.subset<-data.matrix(elite.ballplayers.subset)


#CLV for elite ballplayers calcuated using aggregate values
elite.ballplayers.clvaggregate<-maru.data$clv[4]




#simulate CLV for a bunch of elite ballplayers

# Set Seed for Random Number Generation
set.seed(123456)


#load in customer margin data and analyze it
load("customers.rdata")
hist(customers,breaks = 100)
abline(v=mean(customers),col=2)
margin.mean<-mean(customers)
margin.sd<-sd(customers)


d<-.1
ac<-elite.ballplayers.subset[1]
m<-elite.ballplayers.subset[2]
r<-elite.ballplayers.subset[3]

num.samples=10000

d.vec<-rep(d, num.samples)
ac.vec<-rep(ac, num.samples)
m.vec<-rnorm(num.samples,margin.mean,margin.sd)

#Use Beta distribution to determine retention rate value


#Beta with slight skew 
#par(mfrow=c(3,2))
a<-5
b<-a*(1-r)/r
b
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate a=5 b=3.3")

#CLV = (M * ( (1+d) / (1 + d - r))) - AC
clv.vec=(m.vec* ((1+d.vec)/(1+d.vec-r.vec)))-ac.vec

mean(clv.vec)
median(clv.vec)
hist(clv.vec, main = "CLV")


#Beta with "Bathtub Shape"
a<-.5
b<-a*(1-r)/r
b
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate a=.5 b=.3")

#CLV = (M * ( (1+d) / (1 + d - r))) - AC
clv.vec=(m.vec* ((1+d.vec)/(1+d.vec-r.vec)))-ac.vec

mean(clv.vec)
median(clv.vec)
hist(clv.vec, main = "CLV")


#Beta with sharp peak
a<-100
b<-a*(1-r)/r
b
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate a=100 b=67")


#CLV = (M * ( (1+d) / (1 + d - r))) - AC
clv.vec=(m.vec* ((1+d.vec)/(1+d.vec-r.vec)))-ac.vec

mean(clv.vec)
median(clv.vec)
hist(clv.vec, main = "CLV")





#Create "Whale Plot" to depict value concentration

whale.data <- as.data.frame(clv.vec[order(-clv.vec)]) #sort customers decreasing order of CLV
colnames(whale.data)<-"clv.ordered"

whale.data$clvpercent <- 100*(whale.data$clv.ordered/sum(whale.data$clv.ordered))
whale.data$clvcumpercent<-cumsum(whale.data$clvpercent)

whale.data$ncustomer<-seq(1,num.samples)
whale.data$percentcustomers<-100*whale.data$ncustomer/num.samples

par(mfrow=c(1,2))
plot(whale.data$percentcustomers,whale.data$clvcumpercent, type="l",
     ylim=c(0,150),main="Whale Curve for Maru Batting"
     ,xlab = "Percent of Customers", ylab = "Percent of Aggregate CLV")
abline(100,0,col=3)
abline(v=whale.data$percentcustomers[which.max(whale.data$clv.ordered<=0)],col=2)
abline(h=whale.data$clvcumpercent[which.max(whale.data$clv.ordered<=0)],col=2)

plot(whale.data$percentcustomers,whale.data$clv.ordered, type="l",
     main="Whale Curve for Maru Batting",
     xlab = "Percent of Customers", ylab = "Percent of Aggregate CLV")
abline(h=0,col=2)




