#Monte Carlo Simulation of Wine Purchases in a Week
#Yanwen Wang CRM
#UBC
#Updated January 2018

rm(list=ls())
set.seed(123456)

#Number of purchases
#Let's try Poisson; Note: could also try negative binomial, similar properties but more flexible

num.purchases.poisson<-rpois(1000, 1)

hist(num.purchases.poisson)


#Price per bottle; Note: normal is not perfect. Why not? Demonstrate with large SD

price.per.bottle.normal<-rnorm(1000,15,5)

hist(price.per.bottle.normal)


#dollars spent; Note: Could also use for loop, not necessary in this case

dollars.spent<-num.purchases.poisson*price.per.bottle.normal
hist(dollars.spent)
mean(dollars.spent)
median(dollars.spent)
min(dollars.spent)
max(dollars.spent)
