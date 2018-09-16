
#SET WORKING DIRECTORY
library(data.table)

#IMPORT DATASET
pilgrim <- fread("pilgrimBdata.csv")


# CREATE PROFITABILITY SKEW (WHALE PLOT) IN EXHIBIT 3
pilgrim_whaleplot <- setorder(pilgrim, -Profit99)
pilgrim_whaleplot[,profitpt:=cumsum(Profit99)/sum(Profit99)]
pilgrim_whaleplot[,customerpt:=.I/.N]

pilgrim_whaleplot[,plot(customerpt,profitpt,type="l",xlab="Percent of Customers",ylab="Percent of Profit")]

abline(h=1,col="red")
pilgrim_whaleplot$customerpt[which.max(pilgrim_whaleplot$profitpt>=1)]  # 21.53 percent


abline(v=pilgrim_whaleplot$customerpt[which.max(pilgrim_whaleplot$profitpt)],col=3)
abline(h=pilgrim_whaleplot$profitpt[which.max(pilgrim_whaleplot$profitpt)],col=3)
pilgrim_whaleplot$customerpt[which.max(pilgrim_whaleplot$profitpt)]     # 53.21%

pilgrim_whaleplot$profitpt[which.max(pilgrim_whaleplot$customerpt>=.1)]  # 69.18 percent



# summary info on profit99
summary(pilgrim)

pilgrim[,hist(Profit99,breaks=100)]

pilgrim[,prop.table(table(Profit99>=0))]
pilgrim[,prop.table(table(Profit99>=0))]



# use online channel or not
pilgrim[,table(Online99)]
pilgrim[,prop.table(table(Online99))]

pilgrim[Online99==1,summary(Profit99)]
pilgrim[Online99==0,summary(Profit99)]


#Various ways to visualize heterogeneity in profit
#install.packages("ggplot2")
library(ggplot2)

ggplot(pilgrim, aes(x = Profit99)) + geom_histogram(binwidth = 50, fill = "blue") #Histogram using ggplot2 package, bin width of 50, and blue fill
ggplot(pilgrim, aes(x = Profit99)) + geom_density(fill = "grey50") #Density plot instead of histogram, and grey fill
ggplot(pilgrim, aes(x=Tenure99, y=Profit99)) + geom_point() + geom_smooth() #smoothed fit curve
ggplot(pilgrim, aes(x=Tenure99, y=Profit99)) + geom_point() + geom_smooth() #smoothed fit curve
ggplot(pilgrim, aes(x=Tenure99, y=Profit99)) + geom_point() + geom_smooth(method="lm") #linear regression line
ggplot(pilgrim, aes(x=as.factor(Online99), y=Profit99)) + geom_boxplot() #Boxplot split out by online/offline 
ggplot(pilgrim, aes(x=as.factor(Age99), y=Profit99)) + geom_boxplot() #Boxplot split out by age groups
ggplot(pilgrim, aes(x=as.factor(Age99), y=Profit99)) + geom_violin() #Violin plot split out by age groups
ggplot(pilgrim, aes(x=as.factor(Inc99), y=Profit99)) + geom_boxplot() #Boxplot split out by income groups
ggplot(pilgrim, aes(x=as.factor(District99), y=Profit99)) + geom_boxplot() #Boxplot split out by district groups


# Profit99 by age and income groups
tapply(pilgrim$Profit99,pilgrim$Age99,mean)
tapply(pilgrim$Profit99,pilgrim$Inc99,mean)


# Online channel usage by age and income groups
pilgrim[, prop.table(table(Online99, Age99),2)]
pilgrim[, prop.table(table(Online99, Inc99),2)]


# impact of online channel on profitability

#INTERCEPT-ONLY MODEL
interceptOnly <-lm(Profit99 ~ 1 , data=pilgrim) #Estimate intercept-only regression model
summary(interceptOnly) #Analyze regression output
confint(interceptOnly, level=0.95) #Determine 95% confidence interval for parameters of regression model

# ADD ONLINE99
mod0 <-lm(Profit99 ~ Online99 , data=pilgrim) 
summary(mod0) 


# after controlling for demogrpahics
pilgrim[,hist(Age99)]
pilgrim[,hist(Inc99)]
pilgrim[,hist(Tenure99)]
pilgrim[,hist(District99)]


# mising value approach for Age99 and Inc99
pilgrim[Age99==1,Age:=10]
pilgrim[Age99==2,Age:=20]
pilgrim[Age99==3,Age:=30]
pilgrim[Age99==4,Age:=40]
pilgrim[Age99==5,Age:=50]
pilgrim[Age99==6,Age:=60]
pilgrim[Age99==7,Age:=70]

pilgrim[,AgeExist:=ifelse(!is.na(Age99),1,0)]
pilgrim[,AgeMean:=ifelse(!is.na(Age99),Age,mean(Age,na.rm=T))]
pilgrim[,AgeZero:=ifelse(!is.na(Age99),Age,0)]
pilgrim[,hist(AgeMean)]
pilgrim[,hist(log(AgeMean))]

pilgrim[Inc99==1,Inc:=12500]
pilgrim[Inc99==2,Inc:=17500]
pilgrim[Inc99==3,Inc:=25000]
pilgrim[Inc99==4,Inc:=35000]
pilgrim[Inc99==5,Inc:=45000]
pilgrim[Inc99==6,Inc:=62500]
pilgrim[Inc99==7,Inc:=87500]
pilgrim[Inc99==8,Inc:=112500]
pilgrim[Inc99==9,Inc:=150000]

pilgrim[,IncExist:=ifelse(!is.na(Inc99),1,0)]
pilgrim[,IncMean:=ifelse(!is.na(Inc99),Inc,mean(Inc,na.rm = T))]
pilgrim[,IncZero:=ifelse(!is.na(Inc99),Inc,0)]
pilgrim[,table(IncExist)]
pilgrim[,hist(IncMean)]
pilgrim[,hist(log(IncMean))]



# controlling for age,income,tenure,and district
mod1 <- lm(Profit99 ~ Online99 + Age + Inc + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod1)

mod2 <- lm(Profit99 ~ Online99 + AgeMean + IncMean + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod2)

mod3 <- lm(Profit99 ~ Online99 + AgeZero + IncZero + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod3)

mod4 <- lm(Profit99 ~ Online99 + AgeExist + AgeMean + IncExist + IncMean + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod4)

mod5 <- lm(Profit99 ~ Online99 + AgeExist + AgeZero + IncExist + IncZero + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod5)

mod6 <- lm(Profit99 ~ Online99 + AgeExist + AgeZero + IncExist + I(log(IncZero+1)) + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod6)


pilgrim[,Inc99New:=ifelse(!is.na(Inc99),Inc99,max(Inc99)+1)]
pilgrim[,Age99New:=ifelse(!is.na(Age99),Age99,max(Age99)+1)]
mod7 <- lm(Profit99 ~ Online99 + as.factor(Age99New) + as.factor(Inc99New) + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod7)

#install.packages("visreg")
library(visreg)
visreg(mod5,"AgeZero",type="conditional")
visreg(mod5,"Tenure99")
visreg(mod5,"IncZero")



# future profit prediction
mod8 <- lm(Profit00 ~ Profit99 + Online99 + AgeExist + AgeMean + IncExist + IncMean + Tenure99 + as.factor(District99), data=pilgrim)
summary(mod8)

pilgrim[,Profit00_pred:=predict(mod8,pilgrim)]
pilgrim[,plot(Profit00_pred,Profit00)]
pilgrim[,summary(abs(Profit00_pred-Profit00))]


# retention rate
pilgrim[,retention:=ifelse(!is.na(Profit00),1,0)]
pilgrim[,table(retention)]
pilgrim[,prop.table(table(retention))]

mod9 <- glm(retention ~ Profit99 + Online99 + AgeExist + AgeMean + IncExist + IncMean + Tenure99 + as.factor(District99), data=pilgrim, family=binomial(link = "logit") )
summary(mod9)

pilgrim[,glmpredict := predict(mod9, pilgrim, type="response")]
pilgrim[,hist(glmpredict,breaks=100)]
pilgrim[,retentionpredict:=as.numeric(glmpredict>=.5)]
pilgrim[,table(retention,retentionpredict)]
pilgrim[,prop.table(table(retention,retentionpredict))]

pilgrim[Online99==1,prop.table(table(retention,retentionpredict))]
pilgrim[Online99==0,prop.table(table(retention,retentionpredict))]


# predictions on simulated data
tmp1 = predict(mod9, data.frame(Profit99=seq(-221,2071,by=10), Online99=1, AgeExist=1, AgeMean=mean(pilgrim$AgeMean),
                                IncExist=1, IncMean=mean(pilgrim$IncMean),
                                Tenure99=mean(pilgrim$Tenure99), 
                                District99="1200"), type="response")
tmp2 = predict(mod9, data.frame(Profit99=seq(-221,2071,by=10), Online99=0, AgeExist=1, AgeMean=mean(pilgrim$AgeMean),
                                IncExist=1, IncMean=mean(pilgrim$IncMean),
                                Tenure99=mean(pilgrim$Tenure99), 
                                District99="1200"), type="response")

plot(seq(-221,2071,by=10),tmp1,col=1,type="l",ylim = c(.9,1))
lines(seq(-221,2071,by=10),tmp2,col=2)





