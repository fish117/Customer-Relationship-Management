
baseball = read.csv("baseball.csv",header=T)
baseball=read.csv(file.choose())
# We can use the glm() function to fit a binary response model
# type ?glm to look for the help menu

## now a probit model
modb1 <- glm(win~braves_payroll+opponent_payroll, family=binomial(link="probit"), 
             data=baseball)
summary(modb1)
odds.ratio(modb1)
odds.ratio(modb2)
## now a logit model
modb2 <- glm(win~braves_payroll+opponent_payroll, family=binomial(link="logit"), 
             data=baseball)
summary(modb2)
install.packages("mgcv")
library(mgcv)

pred_win2016 <- predict(modb2, baseball[baseball$season==2016,], type="response")

## now use 0.5 as the cut off for predicting win or loss
pred_win2016_binary <- as.numeric(pred_win2016>0.5)
real_win2016_binary <- baseball$win[baseball$season==2016]

## create a table to display the fit
table(real_win2016_binary, pred_win2016_binary)
