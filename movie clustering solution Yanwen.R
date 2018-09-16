rm(list = ls())
library(data.table)
#setwd
all.raw<-fread("movie viewing data.csv")
all.raw<-read.csv(file.choose())
### PART 1 ###
summary(all.raw)

all.raw[,miss:=ifelse(complete.cases(all.raw),0,1)]
all.raw[,table(miss)]       #43 cases with missing values
all.raw[,moviewatching:=rowMeans(all.raw[,2:51],na.rm=T)]
t.test(moviewatching ~ miss, data=all.raw)


all.clean<-all.raw[complete.cases(all.raw),] #casewise deletion of missing data


movies<-all.clean[,2:51] #create dataset with clustering variables only (i.e., all seen_xxx variables)
demos<-all.clean[,52:57] #create dataset with demographics only



#Get overall viewing proportions by movie
overall.proportions<-sort(colMeans(movies),decreasing=TRUE)  #create matrix with proportion of views for each movie
barplot(overall.proportions, names.arg=names(overall.proportions),las=2, ylim=0:1)
#Make the plot window wide enough and it will be readable
#las=2 makes label text perpendicular to axis


#Get by-person viewing proportions
hist(rowMeans(movies),breaks=20,xlab="Proportion of Movies Watched", main="Histogram of % Movies Watched")

barplot(sort(rowMeans(movies),decreasing = T),las=2, ylim=0:1)
abline(h=median(rowMeans(movies)),col=2)
abline(v=which.max(sort(rowMeans(movies))>mean(rowMeans(movies))),col=2)
which.max(sort(rowMeans(movies))>mean(rowMeans(movies)))
summary(rowMeans(movies))
#99 out of 178 indivdiuals have watched more than 37.5% movies.
#the data is skewed on the heavy movie watching side

sort(rowMeans(movies)) #no one watched 0 movies, 3 watched 2 movies, 1 watched all movies
mean(rowMeans(movies)) #Average person watched 38% of movies
median(rowMeans(movies))#Median person watched 36% of movies

        


### PART 2 ###

#Transform the data such that each variable has a mean of 0 and a standard deviation of 1
#movies<-scale(movies,center=TRUE,scale=TRUE)

# Determine initial number of clusters based on Within-cluster sum of squares
wss<-c()
bss <- c()
wssbss <- c()
pseudoF <- c()

max.clusters = 15
for (i in 2:max.clusters){ 
  kmeansi <- kmeans(movies,centers=i,nstart=15, iter.max=50)
  wss[i] <- sum(kmeansi$withinss)
  bss[i] <- kmeansi$betweenss
  wssbss[i] <- wss[i]/bss[i]
  pseudoF[i] <- (bss[i]/(i-1)) / (wss[i]/(nrow(movies)-i)) 
}
wss
bss
wssbss
pseudoF

par(mfrow=c(2,2))
plot(2:max.clusters, wss[2:max.clusters], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares") 
plot(2:max.clusters, bss[2:max.clusters], type="b", xlab="Number of Clusters",ylab="between groups sum of squares") 
plot(2:max.clusters, wssbss[2:max.clusters], type="b", xlab="Number of Clusters",ylab="Within divided by between groups sum of squares") 
plot(2:max.clusters, pseudoF[2:max.clusters], type="b", xlab="Number of Clusters",ylab="Pseudo F") 

# We can try 3 or 2


### PART 3 ###

#Ordinary K-means
num.clusters<-3
set.seed (12345)
movies.k3<-kmeans(movies, num.clusters,nstart=50)

movies.k3$size

k3.means <- t(movies.k3$centers)
colMeans(k3.means) 

k3.means[order(-k3.means[,1]),] #Cluster 1 has seen 40% of movies with 65 individuals. Classic movies.
k3.means[order(-k3.means[,2]),] #Cluster 2 has seen 26% of movies with 80 individuals, Mostly fantasy/sci-fi
k3.means[order(-k3.means[,3]),] #Cluster 3 has seen 62% of movies with 33 individuals, Heavey watchers. Smaller cluster.




### PART 4 ###
demos[,cluster:=movies.k3$cluster]

demos$cluster1 <- ifelse(movies.k3$cluster==1,1,0)
demos$cluster2 <- ifelse(movies.k3$cluster==2,1,0)
demos$cluster3 <- ifelse(movies.k3$cluster==3,1,0)

#Summary statistics by cluster
aggregate(demos[,1:6],by=demos[,7],FUN=summary)


#Predicting cluster membership
demos[,edunew:=0]
demos[,edunew:=ifelse(edu==1,10,edunew)]
demos[,edunew:=ifelse(edu==2,12,edunew)]
demos[,edunew:=ifelse(edu==3,14,edunew)]
demos[,edunew:=ifelse(edu==4,15,edunew)]
demos[,edunew:=ifelse(edu==5,16,edunew)]
demos[,edunew:=ifelse(edu==6,17,edunew)]
demos[,edunew:=ifelse(edu==7,18,edunew)]
demos[,edunew:=ifelse(edu==8,21,edunew)]

demos[,incnew:=0]
demos[,incnew:=ifelse(income==1,15000,incnew)]
demos[,incnew:=ifelse(income==2,25000,incnew)]
demos[,incnew:=ifelse(income==3,35000,incnew)]
demos[,incnew:=ifelse(income==4,45000,incnew)]
demos[,incnew:=ifelse(income==5,55000,incnew)]
demos[,incnew:=ifelse(income==6,65000,incnew)]
demos[,incnew:=ifelse(income==7,75000,incnew)]
demos[,incnew:=ifelse(income==8,85000,incnew)]
demos[,incnew:=ifelse(income==9,95000,incnew)]

summary(demos)

logit1 <- glm(cluster1 ~ age + edunew + log(incnew) + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit1)
demos$cluster1prob <- predict(logit1, demos, type="response") #cluster 1 tends to be older

logit2 <- glm(cluster2 ~ age + edunew + log(incnew) + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit2)
demos$cluster2prob <- predict(logit2, demos, type="response") #cluster 2 tends to be younger non-white females

logit3 <- glm(cluster3 ~ age + edunew + log(incnew) + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit3)
demos$cluster3prob <- predict(logit3, demos, type="response") #cluster 3 tends to more male (marginally)




### BONUS: SPHERICAL K-MEANS (COSINE DISTANCE) ###
library(skmeans)#if you don't have it installed use install.packages("skmeans")
library(cluster)
library(clue)



#How many clusters?
#Get within sum of squares for 1 cluster
remaining.disimilarity <- numeric()
#Add within sum of squares for other clusters
max.clusters<-15
for (i in 2:max.clusters) remaining.disimilarity[i-1] <- 1 - as.numeric(cl_validity(skmeans(as.matrix(movies),i))["Dissimilarity accounted for"])
plot(2:15, remaining.disimilarity, type="b", xlab="Number of Clusters", ylab="Remaining Disimilarity")




#try 4 clusters?
movies.sk4<-skmeans(as.matrix(movies), 4)

table(movies.sk4$cluster)

sk4.means<-t(aggregate(movies,by=list(movies.sk4$cluster),mean))
sk4.means<-sk4.means[-c(1), ]              

colMeans(sk4.means) #Get % movies watched by cluster; more balanced than ordinary K-means
#[1] 0.3462963 0.4164516 0.5096296 0.2422857

#RESULTS ARE UNSTABLE?
sk4.means[order(-sk4.means[,1]),] #Cluster 1 has seen 35% of movies, fantasy on the top
sk4.means[order(-sk4.means[,3]),] #Cluster 3 has seen 51% of movies, sci-fi on the top
sk4.means[order(-sk4.means[,2]),] #Cluster 2 has seen 42% of movies, classic older movies, heavy watchers 
sk4.means[order(-sk4.means[,4]),] #Cluster 4 has seen 24% of movies, classic older movies, light watchers



#try 3 clusters?
movies.sk3<-skmeans(as.matrix(movies), 3)

table(movies.sk3$cluster)

sk3.means<-t(aggregate(movies,by=list(movies.sk3$cluster),mean))
sk3.means<-sk3.means[-c(1), ]              

colMeans(sk3.means) #Get % movies watched by cluster; more balanced than ordinary K-means
# 0.3815625 0.3041791 0.4672340

#RESULTS ARE UNSTABLE
sk3.means[order(-sk3.means[,1]),] #Cluster 2 has seen 38% of movies, classic older movies on the top
sk3.means[order(-sk3.means[,2]),] #Cluster 3 has seen 30% of movies, fantasy on the top
sk3.means[order(-sk3.means[,3]),] #Cluster 4 has seen 47% of movies, drama & science-fic, heavy watchers

