
## Clustering for the Wine Data
## Yanwen Wang
## Sauder School of Business
## March 2018

rm(list = ls())
set.seed(12345)

wine<-read.csv("wine data.csv")

wine[is.na(wine)]<-0
wine.transposed<-t(wine[,-(1:7)])

#ORDINARY K-MEANS (EUCLIDEAN DISTANCE) WITH 4 CLUSTERS
wine.k4<-kmeans(scale(wine.transposed), centers=4,nstart=25)

wine.k4$centers

k4.clustermeans<-cbind(wine[,1:7], t(wine.k4$centers))

k4.clustermeans[order(-k4.clustermeans[,8]),]
k4.clustermeans[order(-k4.clustermeans[,9]),]
k4.clustermeans[order(-k4.clustermeans[,10]),]
k4.clustermeans[order(-k4.clustermeans[,11]),]

#PERHAPS A DIFFERENT NUMBER OF CLUSTERS?
pseudoF <- function(X, k, ns = 25){
  
  nk <- length(k)
  n <- nrow(X)
  T <- sum(scale(X,scale=F)^2)
  W <- rep(T, nk)
  
  for (i in 1:nk){
    cli <- kmeans(X, k[i], nstart=ns)
    W[i] <- sum(cli$withinss)
  }
  
  B <- T-W
  pF <- (B/(k-1))/(W/(n-k))
  
  return(list(k=k, W=W, pF=pF))
  
}
pseudoF(scale(wine.transposed),2:15)
plot(pseudoF(scale(wine.transposed),2:15)$W)
plot(pseudoF(scale(wine.transposed),2:15)$pF)








wss <-  c(wine.k4$totss, rep(0,14))
bss <- c(rep(0,15))
pseudoF <- c(rep(0,15))
wssbss<- c(rep(0,15))

for (i in 2:15){ 
  kmeansi <- kmeans(wine.transposed,centers=i,nstart=5)
  wss[i] <- sum(kmeansi$withinss)
  bss[i] <- kmeansi$betweenss
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(wine.transposed)-i)) 
  wssbss[i] <- wss[i]/bss[i]
}

plot(2:15, wss[2:15], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:15, wssbss[2:15], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(2:15, pseudoF[2:15], type="b", xlab="Number of Clusters",ylab="Pseudo F") 




#ORDINARY K-MEANS (EUCLIDEAN DISTANCE) WITH 3 CLUSTERS
wine.k3<-kmeans(wine.transposed, centers=3,nstart=25)

k3.clustermeans<-cbind(wine[,1:7], t(wine.k3$centers))
k3.clustermeans[order(-k3.clustermeans[,8]),]
k3.clustermeans[order(-k3.clustermeans[,9]),]
k3.clustermeans[order(-k3.clustermeans[,10]),]

#ORDINARY K-MEANS (EUCLIDEAN DISTANCE) WITH 5 CLUSTERS
wine.k5<-kmeans(wine.transposed, centers=5,nstart=25)

k5.clustermeans<-cbind(wine[,1:7], t(wine.k5$centers))
k5.clustermeans[order(-k5.clustermeans[,8]),]
k5.clustermeans[order(-k5.clustermeans[,9]),]
k5.clustermeans[order(-k5.clustermeans[,10]),] 
k5.clustermeans[order(-k5.clustermeans[,11]),] 
k5.clustermeans[order(-k5.clustermeans[,12]),] 

#SPHERICAL K-MEANS (COSINE DISTANCE)
#install.packages("skmeans")
library(skmeans)
wine.cos5<-skmeans(wine.transposed, 5,method="genetic")

cos5.clustermeans<-cbind(wine[,1:7],t(aggregate(wine.transposed,by=list(wine.cos5$cluster),mean)[,2:33]))

cos5.clustermeans[order(-cos5.clustermeans[,8]),] 
cos5.clustermeans[order(-cos5.clustermeans[,9]),] 
cos5.clustermeans[order(-cos5.clustermeans[,10]),] 
cos5.clustermeans[order(-cos5.clustermeans[,11]),] 
cos5.clustermeans[order(-cos5.clustermeans[,12]),] 


#EVALUATE THE NUMBER OF CLUSTERS FOR SKMEANS
#install.packages("clue")
library(clue)
cl_validity(wine.cos5)


## Create elbow plot

remaining.disimilarity <- numeric()

max.clusters<-25
for (i in 2:max.clusters) {
  remaining.disimilarity[i-1] <- 1 - as.numeric(cl_validity(skmeans(as.matrix(wine.transposed),i)))
}
plot(2:25, remaining.disimilarity, type="b", xlab="Number of Clusters", ylab="Remaining Disimilarity")


