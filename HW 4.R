movie<-read.csv(file.choose())
cust_name<-movie$id
movie_name<-colnames(movie)

movie<-data.table(t(movie))
movie<-movie[-1,]
colnames(movie)<-as.character(cust_name)
movie<-cbind(movie_name[-1],movie)

#User-based filtering
customer<-"Heather"  #The name of the customer for whom we want recommendations
num.recommendations<-2 #the number of recommended movies to return
num.neighbors<-4#the number of nearest neighbors to use for the calculation

###########Correlaion###########

####################################
#compute pairwise similarity using chosen distance function. In this case, let's go with correlation.
movies.cor<-cor(data.matrix(movie[2:267,2:267]), use="pairwise.complete.obs")#you may get an error about Standard deviation = 0...
#This is telling you that for some customers there is no overlap in ratings, so correlation can't be computed and NA is entered.

#order matrix by correlation
nearest.neighbors<-movies.cor[order(-movies.cor[,customer]),customer,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)

#Remove self from neighbors
nearest.neighbors<-nearest.neighbors[rownames(nearest.neighbors)!=customer, , drop=FALSE]

#Remove NAs
nearest.neighbors<-nearest.neighbors[complete.cases(nearest.neighbors), , drop=FALSE]

#Get k nearest neighbors
nearest.neighbors<-nearest.neighbors[1:num.neighbors, ,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)

#Calculate weights -- weights are proportional to similarity
weights<-nearest.neighbors/sum(nearest.neighbors)
colnames(weights)<-c("weight")


#Get ratings for nearest neighbors
library(data.table)




ratings.neighbors<-t(movie[,rownames(nearest.neighbors),with=FALSE])
ratings.neighbors[is.na(ratings.neighbors)]<-0
#Use a for loop to calculate the weighted average for each movie, weighted by the weights calculated above
predicted.scores<-c()

for (i in 1:dim(ratings.neighbors)[2]){
  predicted.scores[i]<-weighted.mean(as.numeric(ratings.neighbors[,i]), t(weights), na.rm=TRUE)
}

predicted.scores<-as.data.frame(predicted.scores)
rownames(predicted.scores)<-movie.ratings[,1]



predicted.scores<-as.data.frame(predicted.scores)
rownames(predicted.scores)<-movie_name[-1]



# Cut down the predicted scores to only the movies that the customer has not seen and sort by score
predicted.scores.unseen<-predicted.scores[is.na(movie[,customer,with=FALSE]), , drop=FALSE]
predicted.scores.unseen<-predicted.scores.unseen[order(-predicted.scores.unseen), ,drop=FALSE]


#recommend the top n movies. 
recommendations<-predicted.scores.unseen[1:num.recommendations,,drop=FALSE]
recommendations



###########Manhatten distance###########

####################################


#Manhatten distance
movie_dis<-read.csv(file.choose())
#movie_dis<-movie_dis[movie_dis$id!="",]


movie_dis<-movie_dis[,-1]
rownames(movie_dis)<-as.character(cust_name)
similarity.1.manhattan<-as.matrix(dist(data.matrix(movie_dis), method="manhattan"))
colnames(similarity.1.manhattan)<-cust_name
rownames(similarity.1.manhattan)<-cust_name
#order matrix by correlation
nearest.neighbors<-similarity.1.manhattan[order(-similarity.1.manhattan[,customer]),customer,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)


#Remove self from neighbors
nearest.neighbors<-nearest.neighbors[rownames(nearest.neighbors)!=customer, , drop=FALSE]

#Remove NAs
nearest.neighbors<-nearest.neighbors[complete.cases(nearest.neighbors), , drop=FALSE]

#Get k nearest neighbors
nearest.neighbors<-nearest.neighbors[1:num.neighbors, ,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)
#Calculate weights -- weights are proportional to similarity
weights<-nearest.neighbors/sum(nearest.neighbors)
colnames(weights)<-c("weight")


#Get ratings for nearest neighbors


ratings.neighbors<-movie_dis[rownames(nearest.neighbors),]
ratings.neighbors[is.na(ratings.neighbors)]<-0
#Use a for loop to calculate the weighted average for each movie, weighted by the weights calculated above
predicted.scores<-c()

for (i in 1:dim(ratings.neighbors)[2]){
  predicted.scores[i]<-weighted.mean(as.numeric(ratings.neighbors[,i]),t( weights), na.rm=TRUE)
}

predicted.scores<-as.data.frame(t(predicted.scores))
rownames(predicted.scores)<-movie_name[-1]



# Cut down the predicted scores to only the movies that the customer has not seen and sort by score
predicted.scores.unseen<-predicted.scores[is.na(movie[,customer,with=FALSE]), , drop=FALSE]
predicted.scores.unseen<-predicted.scores.unseen[order(-predicted.scores.unseen), ,drop=FALSE]


#recommend the top n movies. 
recommendations<-predicted.scores.unseen[1:num.recommendations,,drop=FALSE]
recommendations


###########euclidean distance###########

####################################
movie_dis<-read.csv(file.choose())
#movie_dis<-movie_dis[movie_dis$id!="",]


movie_dis<-movie_dis[,-1]
rownames(movie_dis)<-as.character(cust_name)

similarity.2.Euclidean<-as.matrix(dist(data.matrix(movie_dis), method="euclidean"))
colnames(similarity.2.Euclidean)<-cust_name
rownames(similarity.2.Euclidean)<-cust_name
#order matrix by correlation
nearest.neighbors<-similarity.2.Euclidean[order(-similarity.2.Euclidean[,customer]),customer,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)


#Remove self from neighbors
nearest.neighbors<-nearest.neighbors[rownames(nearest.neighbors)!=customer, , drop=FALSE]

#Remove NAs
nearest.neighbors<-nearest.neighbors[complete.cases(nearest.neighbors), , drop=FALSE]

#Get k nearest neighbors
nearest.neighbors<-nearest.neighbors[1:num.neighbors, ,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)
#Calculate weights -- weights are proportional to similarity
weights<-nearest.neighbors/sum(nearest.neighbors)
colnames(weights)<-c("weight")


#Get ratings for nearest neighbors


ratings.neighbors<-movie_dis[rownames(nearest.neighbors),]
ratings.neighbors[is.na(ratings.neighbors)]<-0
#Use a for loop to calculate the weighted average for each movie, weighted by the weights calculated above
predicted.scores<-c()

for (i in 1:dim(ratings.neighbors)[2]){
  predicted.scores[i]<-weighted.mean(ratings.neighbors[,i],t( weights), na.rm=TRUE)
}

predicted.scores<-as.data.frame(t(predicted.scores))
rownames(predicted.scores)<-movie_name[-1]



# Cut down the predicted scores to only the movies that the customer has not seen and sort by score
predicted.scores.unseen<-predicted.scores[is.na(movie[,customer,with=FALSE]), , drop=FALSE]
predicted.scores.unseen<-predicted.scores.unseen[order(-predicted.scores.unseen), ,drop=FALSE]


#recommend the top n movies. 
recommendations<-predicted.scores.unseen[1:num.recommendations,,drop=FALSE]
recommendations



###########cosine distance###########

####################################
movie_dis<-read.csv(file.choose())
#movie_dis<-movie_dis[movie_dis$id!="",]


movie_dis<-movie_dis[,-1]
rownames(movie_dis)<-as.character(cust_name)

similarity.3.cosine<-as.matrix(dist(data.matrix(movie_dis), method="cosine"))
colnames(similarity.3.cosine)<-cust_name
rownames(similarity.3.cosine)<-cust_name
#order matrix by correlation
nearest.neighbors<-similarity.3.cosine[order(-similarity.3.cosine[,customer]),customer,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)


#Remove self from neighbors
nearest.neighbors<-nearest.neighbors[rownames(nearest.neighbors)!=customer, , drop=FALSE]

#Remove NAs
nearest.neighbors<-nearest.neighbors[complete.cases(nearest.neighbors), , drop=FALSE]

#Get k nearest neighbors
nearest.neighbors<-nearest.neighbors[1:num.neighbors, ,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)
#Calculate weights -- weights are proportional to similarity
weights<-nearest.neighbors/sum(nearest.neighbors)
colnames(weights)<-c("weight")


#Get ratings for nearest neighbors


ratings.neighbors<-movie_dis[rownames(nearest.neighbors),]
ratings.neighbors[is.na(ratings.neighbors)]<-0
#Use a for loop to calculate the weighted average for each movie, weighted by the weights calculated above
predicted.scores<-c()

for (i in 1:dim(ratings.neighbors)[2]){
  predicted.scores[i]<-weighted.mean(ratings.neighbors[,i],t( weights), na.rm=TRUE)
}

predicted.scores<-as.data.frame(t(predicted.scores))
rownames(predicted.scores)<-movie_name[-1]



# Cut down the predicted scores to only the movies that the customer has not seen and sort by score
predicted.scores.unseen<-predicted.scores[is.na(movie[,customer,with=FALSE]), , drop=FALSE]
predicted.scores.unseen<-predicted.scores.unseen[order(-predicted.scores.unseen), ,drop=FALSE]


#recommend the top n movies. 
recommendations<-predicted.scores.unseen[1:num.recommendations,,drop=FALSE]
recommendations
