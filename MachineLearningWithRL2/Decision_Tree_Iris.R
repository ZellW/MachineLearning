# ----------------------Decision Tree -----------------------------
View(iris)
names(iris)
table(iris$Species)

### Create training and testing Dataset
library(caret)
inTrain <-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)


library(rpart)
modFit<-train(Species~.,method = "rpart",data=training)
print(modFit$finalModel)

plot(modFit$finalModel,uniform=TRUE,main="Classification Tree")
text(modFit$finalModel,use.n=TRUE,all=TRUE,cex=.8)

library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

# predict new values
prediction<-predict(modFit,newdata=testing)
# summarize accuracy
table(prediction, testing$Species)
