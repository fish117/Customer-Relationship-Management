#Yanwen Wang
#University of British Columbia
#CRM
#Updated January 2018

#Generate PDFs/PMFs, CDFs and random numbers from common distributions
rm(list=ls())
set.seed(123456)


# Standard Normal Distribution

norm.values<-seq(from = -5, to = 5, by = .01)

norm.density<-dnorm(norm.values, mean=0, sd=1)

plot(norm.values, norm.density)

norm.cum<-pnorm(norm.values, mean=0, sd=1)

plot(norm.values, norm.cum)

norm.rand<-rnorm(10000, mean=0, sd=1)

hist(norm.rand)

norm.prob<-qnorm(.975, mean=0, sd=1)



#Uniform Distribution on 0:1

unif.values<-seq(from = 0, to = 1, by = .01)

unif.density<-dunif(unif.values, min=0, max=1)

plot(unif.values,unif.density)

unif.cum<-punif(unif.values, min=0, max=1)

plot(unif.values,unif.cum)

unif.rand<-runif(1000, min=0, max=1)

hist(unif.rand)





#Binomial Distribution, 20 flips, p=.5

binom.values<-seq(from = 0, to = 20, by = 1)

binom.mass<-dbinom(binom.values, size=20, prob=.5)

plot(binom.values, binom.mass)

binom.cum<-pbinom(binom.values, size=20, prob=.5)

plot(binom.values, binom.cum)

binom.rand<-rbinom(10000, size=20, prob=.5)

hist(binom.rand)


