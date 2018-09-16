

# TO SHOW THE FLAW OF AVERAGES IN CLV CALCULATIONS
rm(list = ls())

T <- 5         #Time periods over which value is calculated
margin<-30000  #Expected margin in each time period
dr<-0.10       #Discount rate
ac<-50000
rr<-seq(from = .10, to = .90, by = .10) #Vector of retention rates


#Discount factor matrix
discountfactor <- matrix(0,nrow=length(rr),ncol=T)

for (i in 1:T){
  discountfactor[,i] <- 1/((1+dr)^(i-1))
  }
discountfactor


#Probability of being "alive" matrix
palive <- matrix(0,nrow=length(rr),ncol=T)

for (i in 1:T){
  palive[,i] <- rr^(i-1)
  }
palive


#Values matrix
dcf<-palive*margin*discountfactor
dcf


#Compute CLV for a customer with a retention rate of 0.50
sum(dcf[5,])- ac


#Compute CLV for 9 customers with retention rates of 0.10, 0.20, ..., and 0.90
(sum(dcf[1:9,])/9) - ac


