
# in class exercise on monte carlo simulation

#Generate PDFs/PMFs, CDFs and random numbers from common distributions
rm(list=ls())
set.seed(123456)

# repeat nsize times for the monte carlo simulation
nsize = 10000

# nsize random draws from a standard normal distribution
norm.rand = rnorm(nsize, mean=0, sd=1)
hist(norm.rand)
mean(norm.rand)
sd(norm.rand)


# nsize random draws from a beta distribution
a = 2
b = 1
beta.rand = rbeta(nsize,a,b)
hist(beta.rand, main= "Retention Rate")
mean(beta.rand) # a/(a+b)
sd(beta.rand)  

# what if a=1, b=2 for a beta distribution
a = 1
b = 2
beta.rand = rbeta(nsize,a,b)
hist(beta.rand, main= "Retention Rate")
mean(beta.rand) # a/(a+b)
sd(beta.rand)  

# what if a=1, b=2 for a beta distribution
a = .5
b = .5
beta.rand = rbeta(nsize,a,b)
hist(beta.rand, main= "Retention Rate")
mean(beta.rand) # a/(a+b)
sd(beta.rand)  


#b<-a*(1-r)/r




# nsize random draws from a uniform distribution
uniform.rand = runif(nsize, min=0, max=1)
hist(uniform.rand)
mean(uniform.rand)
sd(uniform.rand)





# nsize random draws from a geometric distribution
# what does the parameter mean here?
geom.rand = rgeom(nsize, prob = 1/4)
hist(geom.rand)
mean(geom.rand)
sd(geom.rand)
