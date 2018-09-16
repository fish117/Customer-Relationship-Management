
# Lecture 2 - customer lifetime value calculation
# in class exercise

# Example
# M=100
# R=.8
# AC=200
# d=.1 

# let's try to get the CLV according to Case 1
M = 100
R = .8
AC = 200
d = .1

M*(1+d)/(1+d-R)
CLV = M*(1+d)/(1+d-R) - AC
CLV


# Let's verify by listing the infinite series
maxperiod = 100
period = seq(1,maxperiod)

# discounted cash flow matrix 
dcf = rep(0,maxperiod)
npv = rep(0,maxperiod)

for (t in 1:maxperiod){
  dcf[t] = M*R^(t-1)/(1+d)^(t-1)
  npv[t] = sum(dcf[1:t]) - AC
}
# print out the discounted cash flow matrix 
plot(dcf,type="b")

# print out npv to see since which period the npv>=0
plot(npv,type="b")

# an alternative to figure out since which period the npv>=0
apply(npv,1, function(x) which.max(x>=0))
which.max(npv>=0)
