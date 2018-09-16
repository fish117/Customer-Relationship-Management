#Yanwen Wang
#University of British Columbia
#Customer Relationship Management
#Updated March 2018

#Demonstrate different distance metrics, how to compute them, pros and cons

#rm(list=ls())

#Set working directory
similarity.1<-t(data.frame(c(1,2),c(5,3)))

rownames(similarity.1)<-c("Alice","Bernard")
colnames(similarity.1)<-c("Movie.1","Movie.2")

plot(similarity.1[,"Movie.1"],similarity.1[,"Movie.2"],ylim=c(0,7),xlim=c(0,7))


similarity.1.manhattan<-as.matrix(dist(similarity.1, method="manhattan"))
rownames(similarity.1.manhattan)<-c("Alice","Bernard")
colnames(similarity.1.manhattan)<-c("Alice","Bernard")
similarity.1.manhattan


similarity.1.euclidean<-as.matrix(dist(similarity.1, method="euclidean"))
rownames(similarity.1.euclidean)<-c("Alice","Bernard")
colnames(similarity.1.euclidean)<-c("Alice","Bernard")
similarity.1.euclidean


#install.packages('proxy') # This package adds cosine to the distance functions that can be computed
library('proxy') 

similarity.1.cosine<-as.matrix(dist(similarity.1, method="cosine"))
similarity.1.cosine<-1-similarity.1.cosine #This function actually returns 1-cosine, so need to convert
similarity.1.cosine
angle.1<-acos(similarity.1.cosine)/(2*pi/360)

rownames(similarity.1.cosine)<-c("Alice","Bernard")
colnames(similarity.1.cosine)<-c("Alice","Bernard")
angle.1

# Correlation can't be visualized for bivariate data
similarity.1.correlation<-cor(t(similarity.1), use="pairwise.complete.obs")
rownames(similarity.1.correlation)<-c("Alice","Bernard")
colnames(similarity.1.correlation)<-c("Alice","Bernard")
similarity.1.correlation



# How each distance is sensitive to scaling? 
# Add a constant to each value
similarity.2<-t(data.frame(c(1 +5,2 +5),c(5 *2,3 +5)))

rownames(similarity.2)<-c("Alice","Bernard")
colnames(similarity.2)<-c("Movie.1","Movie.2")

# Plot
plot(similarity.2[,"Movie.1"],similarity.2[,"Movie.2"],ylim=c(0,10),xlim=c(0,10))
points(c(1,5),c(2,3))
abline(0,7/6); abline(0,8/10)
abline(0,2/1,col="red"); abline(0,3/5,col="red")

# Get similarity matrix and Compare to previous
similarity.2.manhattan<-as.matrix(dist(similarity.2, method="manhattan"))
similarity.2.euclidean<-as.matrix(dist(similarity.2, method="euclidean"))
similarity.2.cosine<-1-as.matrix(dist(similarity.2, method="cosine"))
similarity.2.correlation<-cor(t(similarity.2), use="pairwise.complete.obs")


similarity.1.manhattan
similarity.2.manhattan

similarity.1.euclidean
similarity.2.euclidean

similarity.1.correlation
similarity.2.correlation

similarity.1.cosine
similarity.2.cosine
similarity.2.cosine
angle.2<-acos(similarity.2.cosine)/(pi/180)
angle.2





#How do distance functions treat missing data?
similarity.3<-t(data.frame(c(NA,NA,4,4,5),c(1,2,2,2,3)))

rownames(similarity.3)<-c("Alice","Bernard")
colnames(similarity.3)<-c("Movie.1","Movie.2","Movie.3","Movie.4","Movie.5")
similarity.3

similarity.3.manhattan<-as.matrix(dist(similarity.3, method="manhattan"))
rownames(similarity.3.manhattan)<-c("Alice","Bernard")
colnames(similarity.3.manhattan)<-c("Alice","Bernard")
similarity.3.manhattan

similarity.3.euclidean<-as.matrix(dist(similarity.3, method="euclidean"))
rownames(similarity.3.euclidean)<-c("Alice","Bernard")
colnames(similarity.3.euclidean)<-c("Alice","Bernard")
similarity.3.euclidean






#Compare distance functions after scaling one set of values. Cindy's scores are Alice's plus a constant
similarity.4<-t(data.frame(c(2,2,4,4,5),c(1,2,2,2,3),c(2+5,2+5,4+5,4+5,5+5) ))
rownames(similarity.4)<-c("Alice","Bernard","Cindy")
colnames(similarity.4)<-c("Movie.1","Movie.2","Movie.3","Movie.4","Movie.5")
similarity.4

similarity.4.cosine<-1-as.matrix(dist(similarity.4, method="cosine"))
similarity.4.cosine

similarity.4.correlation<-cor(t(similarity.4), use="pairwise.complete.obs")
rownames(similarity.4.correlation)<-c("Alice","Bernard","Cindy")
colnames(similarity.4.correlation)<-c("Alice","Bernard","Cindy")
similarity.4.correlation

similarity.4.manhattan<-as.matrix(dist(similarity.4, method="manhattan"))
rownames(similarity.4.manhattan)<-c("Alice","Bernard","Cindy")
colnames(similarity.4.manhattan)<-c("Alice","Bernard","Cindy")
similarity.4.manhattan

similarity.4.euclidean<-as.matrix(dist(similarity.4, method="euclidean"))
rownames(similarity.4.euclidean)<-c("Alice","Bernard","Cindy")
colnames(similarity.4.euclidean)<-c("Alice","Bernard","Cindy")
similarity.4.euclidean





