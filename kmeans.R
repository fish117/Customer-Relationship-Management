
# Illustration of key points in K-MEANS clusteirng analysis
# March 2018
# Lecture 7 - Clustering 
# R-code Illustration of K-MEANS Clustering Algorithms

set.seed(12345)
library(MASS)
library(rgl)


# K Means Clustering
# the idea is to partition the space by K clusters
x1 <- c(1, 2, 3, 4, 7, 8, 10)
x2 <- c(8, 2, 3, 1, 11, 8, 10)
X <- cbind(x1, x2)
plot(X, pch=16)
#identify(X, labels=1:7)

X.km <- kmeans(X,2)
X.km
points(X,pch=X.km$cluster+1, col=X.km$cluster+1)
points(X.km$centers, col=2:3, pch=2:3, cex=1.5)



# random starts may give different solutions
mu3 <- c(4.5, 4.5)
Sigma2 <- matrix(c(2.25,1.5,1.5,2.25),nrow=2)
unimodal <- mvrnorm(50, mu=mu3, Sigma=Sigma2)

plot(unimodal, pch=16)
uni.3m1 <- kmeans(unimodal,3)	# Random starts - 
uni.3m2 <- kmeans(unimodal,3)	# different answers

plot(unimodal, pch=uni.3m1$cluster, col=uni.3m1$cluster)# if I do kmeans once
points(unimodal, pch=uni.3m2$cluster+3, col=uni.3m2$cluster)# if I do kmeans twice, some points will be reassinged.

table(data.frame(km1=uni.3m1$cluster,km2=uni.3m2$cluster))

uni.3m1$withinss
sum(uni.3m1$withinss)
uni.3m2$withinss
sum(uni.3m2$withinss)




# nstart argument tells kmeans to try that many random starts and keep thte best
# with 20 or 25 random starts, you'll generally find the overall best solution
# unless your sample size is really big
uni.3m3 <- kmeans(unimodal,3, nstart=25)# nstart will keep doing kmeans 25 points, it will try 25 paris center points.keep the optimal results.	 
uni.3m4 <- kmeans(unimodal,3, nstart=25)	

plot(unimodal, pch=uni.3m3$cluster, col=uni.3m3$cluster)
points(unimodal, pch=uni.3m4$cluster+3, col=uni.3m4$cluster)

table(data.frame(km1=uni.3m3$cluster,km2=uni.3m4$cluster))

uni.3m3$withinss
sum(uni.3m3$withinss)

#############the following block has optional content#############################
# Predictions
mu1 <- c(3,3)
mu2 <- c(6,6)
Sigma1 <- matrix(c(1,0,0,1),nrow=2)
bimodal <- rbind(mvrnorm(25,mu=mu1,Sigma=Sigma1), 
                 mvrnorm(25,mu=mu2,Sigma=Sigma1))
plot(bimodal, pch=16)

bim.2m <- kmeans(bimodal, 2)

plot(bimodal, pch=bim.2m$cluster, col = bim.2m$cluster)
points(bim.2m$centers, pch=3, col=1:2)

points(unimodal, pch=4, col=4)

#source("predict.kmeans.txt")
predict.kmeans <- function(km, data) {
  k <- nrow(km$centers)
  n <- nrow(data)
  d <- as.matrix(dist(rbind(km$centers, data)))[-(1:k),1:k]
  out <- apply(d, 1, which.min)
  return(out)
}


pred.uni.bim <- predict.kmeans(bim.2m, unimodal)
pred.uni.bim

points(unimodal, pch=pred.uni.bim+2, col=pred.uni.bim)




# Validation
iris0 <- iris[,1:4]
s <- sample(150, 75)
iris1 <- iris0[s,]	# Calibration Set
iris2 <- iris0[-s,]	# Validation Set


wss <-  rep(0,15)
bss <- rep(0,15)
pseudoF <- rep(0,15)
wssbss<- rep(0,15)
for (i in 2:10){ 
  kmeansi <- kmeans(scale(iris1),centers=i, nstart=5, iter.max = 30)
  wss[i] <- sum(kmeansi$withinss)# within cluster sum of square,as small as possible
  bss[i] <- kmeansi$betweenss# between cluster sum of square,as large as possible
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(iris1)-i)) # i-1, df,F statistics
  wssbss[i] <- wss[i]/bss[i]#ratio of WSS/BSS,as small as possible
}
par(mfrow=c(2,2))
plot(2:10, wss[2:10], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:10, bss[2:10], type="b", xlab="Number of Clusters",ylab="between groups sum of squares") 
plot(2:10, wssbss[2:10], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(2:10, pseudoF[2:10], type="b", xlab="Number of Clusters",ylab="Pseudo F") 
# for pseudoF, large is best


iris1.3m <- kmeans(iris1, 3, 25)
pairs(iris1, pch=iris1.3m$cluster, col=iris1.3m$cluster)

pred.iris2.iris1 <- predict.kmeans(iris1.3m, iris2)

iris2.3m <- kmeans(iris2, 3, 25)
pairs(iris2, pch=pred.iris2.iris1, col=iris2.3m$cluster)

table(data.frame(S1=pred.iris2.iris1,S2=iris2.3m$cluster))





iris1.4m <- kmeans(iris1, 4, 25)
pairs(iris1, pch=iris1.4m$cluster, col=iris1.4m$cluster)

pred.iris2.iris1 <- predict.kmeans(iris1.4m, iris2)

iris2.4m <- kmeans(iris2, 4, 25)
pairs(iris2, pch=pred.iris2.iris1, col=iris2.4m$cluster)

table(data.frame(S1=pred.iris2.iris1,S2=iris2.4m$cluster))



iris1.2m <- kmeans(iris1, 2, 25)
pred.iris2.iris1 <- predict.kmeans(iris1.2m, iris2)

iris2.2m <- kmeans(iris2, 2, 25)

pairs(iris2, pch=pred.iris2.iris1, col=iris2.2m$cluster)
table(data.frame(S1=pred.iris2.iris1,S2=iris2.2m$cluster))

#end



#####################Swiss Canton Data################################
swiss.4m <- kmeans(scale(swiss), 4, nstart=25)
pairs(swiss, pch=swiss.4m$cluster, col=swiss.4m$cluster)

library(rgl)
swiss.pca <- prcomp(swiss, scale=T)
plot3d(swiss.pca$x[,1:3],type="s",size=.25, col=swiss.4m$cluster)

swiss.4m$withinss
sum(swiss.4m$withinss)



#PERHAPS A DIFFERENT NUMBER OF CLUSTERS?
# Number of clusters?  "Pseudo-F" Statistic 
# The "Pseudo-F" statistic: large pseudo-F (adjsuted between group SS/ adjusted within group SS) 
# indicates the efficient of the partition
wss <-  rep(0,15)
bss <- rep(0,15)
pseudoF <- rep(0,15)
wssbss<- rep(0,15)

for (i in 2:10){ 
  kmeansi <- kmeans(scale(swiss),centers=i, nstart=5, iter.max = 30)
  wss[i] <- sum(kmeansi$withinss)
  bss[i] <- kmeansi$betweenss
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(swiss)-i)) 
  wssbss[i] <- wss[i]/bss[i]
}
par(mfrow=c(2,2))
plot(2:10, wss[2:10], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:10, bss[2:10], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:10, wssbss[2:10], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(2:10, pseudoF[2:10], type="b", xlab="Number of Clusters",ylab="Pseudo F") 




# let's use the 3 clusters
swiss.3m <- kmeans(scale(swiss), 3)
sum(swiss.3m$withinss)

pairs(swiss, pch=swiss.3m$cluster, col=swiss.3m$cluster)

plot3d(swiss.pca$x[,1:3],type="s",size=.25, col=swiss.3m$cluster)



########## the following block has optional content######################
# Comparisoins with hierarchical clustering and interpretation
swiss.hc.w <- hclust(dist(scale(swiss)), "ward.D2")

plot(swiss.hc.w)

table(data.frame(km3=swiss.3m$cluster,hc3=cutree(swiss.hc.w,3)))



# Interpretations
pairs(swiss.3m$centers, pch=1:3, col=1:3)
swiss.3m$centers




