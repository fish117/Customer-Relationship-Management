## load data
baseball <- read.csv("baseball.csv")
## look at the structure of the data
str(baseball)
## quick summary
summary(baseball)


## histogram on attendance 
hist(baseball$attendance, col="green",
     xlab="Attendance", main="Histogram")
plot(density(baseball$attendance))


## scatter plots
plot(baseball$tickets_resale, baseball$attendance,col=3);abline(lm(attendance~tickets_resale,data=baseball),col=2)
plot(log(baseball$tickets_resale), baseball$attendance,col=3);abline(lm(attendance~log(tickets_resale),data=baseball),col=2)
plot(baseball$tickets_resale, log(baseball$attendance),col=3);abline(lm(log(attendance)~tickets_resale,data=baseball),col=2)
plot(sqrt(baseball$tickets_resale), sqrt(baseball$attendance),col=3);abline(lm(sqrt(attendance)~sqrt(tickets_resale),data=baseball),col=2)


with(baseball, plot(tickets_sold, attendance,col=3))
abline(lm(attendance~tickets_sold,data=baseball),col=2)


## filter by season 
with(baseball[baseball$season==2013|baseball$season==2014,],
     plot(tickets_sold, attendance))


## event_date
summary(baseball$event_date)


## convert to Date type
baseball$event_date <- as.Date(baseball$event_date,"%Y-%m-%d")
summary(baseball$event_date)



## let us create a variable called "weekday"
baseball$weekday <- weekdays(baseball$event_date)
summary(baseball$weekday)
table(baseball$weekday)

baseball$weekday <-as.factor(baseball$weekday)
baseball$weekday <- factor(baseball$weekday, 
    c("Monday","Tuesday","Wednesday","Thursday","Friday",
      "Saturday","Sunday"))

table(baseball$weekday)



# Now regression model 
mod1 <- lm(attendance~weekday+tickets_sold, data=baseball)
# simple model output
mod1
# detailed model output
summary(mod1)

anova(mod1)



mod2 <- lm(attendance~weekday, data=baseball)
summary(mod2)



## now model 3 using 2011-2015 data
mod3 <- lm(attendance~weekday+tickets_sold, 
           data=baseball[baseball$season<2016, ])
summary(mod3)
library(data.table)
baseball<-data.table(baseball)
data=baseball[baseball$season<2016,]
## using 2016 data for prediction 
predict_2016 <- predict(mod3, baseball[baseball$season==2016,])
predict_2016



## how good are our predictions??
attendance_2016 <- baseball$attendance[baseball$season==2016]
attendance_2016
data.frame(attendance_2016, predict_2016)
plot(attendance_2016, predict_2016)
abline(0,1, col="green")
