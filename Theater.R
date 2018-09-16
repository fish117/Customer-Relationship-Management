
## Clustering of the Theater Data
## Yanwen Wang
## Sauder School of Business
## March 2018

rm(list = ls())

#setwd
theater<-read.csv("theater.csv")
head(theater)
summary(theater)
var(theater[,1:5])


#K-MEANS CLUSTERS FOR THEATER IN-CLASS EXERCISE
set.seed(12345)

#K-means can be subject to random starting conditions, so it is considered good practice
#to run it with a number of random starts. This is accomplished with the nstart argument.

#is there anything wrong the following code?
theater.k3 <- kmeans(theater[,1:5], centers=3, nstart=25)






#Transform the data such that each variable has a mean of 0 and a standard deviation of 1
theater.k3 <- kmeans(scale(theater[,1:5]), centers=3, nstart=25)# first scale the data 

theater.k3

theater.k3$cluster
theater.k3$centers
# center 3 care more about the price, center 2 their parents like drama,center 2 will consider the distance
heater.k3$size           #displays size of clusters,small ize difference is better.

pie(theater.k3$size, radius=1, main="Cluster Size Pie", clockwise=TRUE, col=rainbow(theater.k3$cluster+1))

theater.k3$totss          #displays total sum of squares
theater.k3$withinss       #displays within-cluster sum of squares
theater.k3$betweenss      #displays between-cluster sum of squares
sum(theater.k3$withinss)+theater.k3$betweenss


#PERHAPS A DIFFERENT NUMBER OF CLUSTERS?
#IS 3 THE RIGHT NUMBER OF CLUSTERS?
wss <- rep(0,15)
bss <- rep(0,15)
wssbss <- rep(0,15)
pseudoF <- rep(0,15)

# evaluation criteria for different cluster size
# try cluster n from 2:15
set.seed(12345)
for (i in 2:15){ 
  kmeansi <- kmeans(scale(theater[,1:5]),centers=i,nstart=25, iter.max=30)
  wss[i] <- sum(kmeansi$withinss)
  bss[i] <- kmeansi$betweenss
  wssbss[i] <- wss[i]/bss[i]
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(theater)-i)) 
}
wss
bss
wssbss
pseudoF
#cluster should be <=3 and each cluster should have similar size #. 
par(mfrow=c(2,2))
plot(2:15, wss[2:15], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:15, bss[2:15], type="b", xlab="Number of Clusters",ylab="between groups sum of squares") 
plot(2:15, wssbss[2:15], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(2:15, pseudoF[2:15], type="b", xlab="Number of Clusters",ylab="Pseudo F") 



# alternative way of writing a function to provide the evaluation criteria for different cluster size
set.seed(12345)
cluster_n <- function(X, k, ns, iter){
  for (i in k[1]:k[length(k)]){
    kmeansi <- kmeans(X, centers=i, nstart=ns, iter.max=iter)
    wss[i] <- sum(kmeansi$withinss)
    bss[i] <- kmeansi$betweenss
    wssbss[i] <- wss[i]/bss[i]
    pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(X)-i)) 
  }
  return(list(k=k, wss=wss, bss=bss, wssbss=wssbss, pseudoF=pseudoF))
  
}

clustersize = c(2:15)
output = cluster_n(scale(theater[,1:5]), clustersize, ns=25, iter=30)
output$wss
output$bss
output$wssbss
output$pseudoF

par(mfrow=c(2,2))
plot(clustersize, output$wss[clustersize], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(clustersize, output$bss[clustersize], type="b", xlab="Number of Clusters",ylab="between groups sum of squares") 
plot(clustersize, output$wssbss[clustersize], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(clustersize, output$pseudoF[clustersize], type="b", xlab="Number of Clusters",ylab="Pseudo F") 







# CLUSTER INTERPRETATION AND PROFILING
# NOW LETS USE CLUSTER SIZE 3 AND DESCRIBE THE DEMOGRAPHIC FEATURES OF EACH CLUSTER
# DD CLUSTER COLUMN TO DATA
theater$cluster <- theater.k3$cluster


#PCA(principal component analysis) and get component scores for each survey question
cor(theater[,1:5])

summary(lm(attitude ~ planning + parents + goodval + getto, data=theater))


# Plotting the results of K-means can be difficult because of the high-dimensional nature of the data
# To overcome this, the plot3d function projects the data into three dimensions
# And color codes the points according to cluster membership
library(rgl)
theater.pca <- prcomp(theater[,1:5], scale=T)
theater.pca

plot3d(theater.pca$x[,1:3],type="s",size=.25, col=theater.k3$cluster)



# Analyze demographics by cluster
theater$cluster1 <- ifelse(theater.k3$cluster==1,1,0)
theater$cluster2 <- ifelse(theater.k3$cluster==2,1,0)
theater$cluster3 <- ifelse(theater.k3$cluster==3,1,0)


## Color Spectrum Object
clr1 <- c("limegreen","darkorange","darkorchid","yellow2","steelblue","indianred") 
## Mosaic Plots 
par(mfrow=c(2,2))
mosaicplot(age~cluster,data=theater,col=c("limegreen","black","yellow2","steelblue","indianred"), xlab="Age",ylab="Cluster",main="Cluster vs. Age Mosaic")
mosaicplot(educ~cluster,data=theater, col=c("limegreen","black","yellow2","steelblue","indianred"), xlab="Education", ylab="Cluster", main="Cluster vs. Education Mosaic")
mosaicplot(income~cluster,data=theater, col=c("limegreen","black","yellow2","steelblue","indianred"), xlab="Income", ylab="Cluster", main="Cluster vs. Income Mosaic")
mosaicplot(cnty~cluster,data=theater, col=c("limegreen","black","yellow2","steelblue","indianred"), xlab="County", ylab="Cluster", main="Cluster vs. County Mosaic")


install.packages("doBy")   
library(doBy)
theater$cnty.Chicago <- ifelse(theater$cnty=="Chicago",1,0)
theater$cnty.Cook <- ifelse(theater$cnty=="Cook",1,0)
theater$cnty.DuPage <- ifelse(theater$cnty=="DuPage",1,0)
theater$cnty.Lake <- ifelse(theater$cnty=="Lake",1,0)
theater$cnty.Other <- ifelse(theater$cnty=="Other",1,0)
summaryBy(age + educ + income ~ cluster,data=theater, FUN = list(mean,sd), na.rm=TRUE)
summaryBy(cnty.Chicago + cnty.Cook + cnty.DuPage + cnty.Lake + cnty.Other ~ cluster,data=theater, FUN = list(mean), na.rm=TRUE)


## BoxPlots survey questions
par(mfrow=c(2,3))
boxplot(attitude~cluster,data=theater, col=clr1, xlab="Cluster", ylab="Attitude", main="Clustertude Box Plot")
boxplot(planning~cluster, data=theater, col=clr1, xlab="Cluster", ylab="Planning", main="Cluster vs. Planning Box Plot")
boxplot(parents~cluster, data=theater, col=clr1, xlab="Cluster", ylab="Parents", main="Cluster vs. Parents Box Plot")
boxplot(goodval~cluster, data=theater, col=clr1, xlab="Cluster", ylab="Good Value", main="Cluster vs. Good Value Box Plot")
boxplot(getto~cluster, data=theater, col=clr1, xlab="Cluster", ylab="Get To", main="Cluster vs. Get To Box Plot")


## BoxPlots demographics
par(mfrow=c(2,2))
boxplot(age~cluster,data=theater, col=clr1, xlab="Cluster", ylab="age", main="Clustertude Box Plot")
boxplot(educ~cluster, data=theater, col=clr1, xlab="Cluster", ylab="educ", main="Cluster vs. Planning Box Plot")
boxplot(income~cluster, data=theater, col=clr1, xlab="Cluster", ylab="income", main="Cluster vs. Parents Box Plot")



# can we use demographics to predict the identity of the clusters?
# try logistic regression
logit1 <- glm(I(cluster==1) ~ age + educ + income + cnty,  data=theater, family=binomial(link = logit))
summary(logit1)
theater$cluster1prob <- predict(logit1, theater, type="response")

plot(theater$educ, theater$cluster1prob)
plot(theater$age, theater$cluster1prob)
plot(theater$income, theater$cluster1prob)



logit2 <- glm(I(cluster==2) ~ age + educ + income + cnty,  data=theater, family=binomial(link = logit))
summary(logit2)
theater$cluster2prob <- predict(logit2, theater, type="response")

plot(theater$educ, theater$cluster2prob)
plot(theater$age, theater$cluster2prob)
plot(theater$income, theater$cluster2prob)



logit3 <- glm(I(cluster==3) ~ age + educ + income + cnty,  data=theater, family=binomial(link = logit))
summary(logit3)
theater$cluster3prob <- predict(logit3, theater, type="response")

plot(theater$educ, theater$cluster3prob)
plot(theater$age, theater$cluster3prob)
plot(theater$income, theater$cluster3prob)




## try multinomial regression
library(nnet) ## neural net library needed
##full model multinomial regression
mod1 <- multinom(cluster ~ age + educ + income + cnty, data=theater)
summary(mod1)
#str(mod1)



