library(caret)
library(ggplot2)
View(iris)
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)

# CLuster with k-Means
KMeans1<-kmeans(subset(training,select=-c(Species)),centers=3)
KMeans1
training$clusters<-as.factor(KMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)



# Compare to real labels
table(KMeans1$cluster,training$Species)

# Build predictor
modFit<-train(clusters~., data=subset(training,select=-c(Species)),method="rpart")

# Apply on test
testClusterPred <-predict(modFit,testing)
table(testClusterPred,testing$Species)
