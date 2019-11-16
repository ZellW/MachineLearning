#-------------------------------Random Forest-----------------------
library(caret)
View(iris)
names(iris)
table(iris$Species)

### Create training and testing Dataset
inTrain <-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)


library(rpart)
modFit<-train(Species~.,method = "rf",data=training,prox=TRUE)
modFit

# get a single Tree
getTree(modFit$finalModel,labelVar = TRUE)

# Predicting new values
pred<-predict(modFit,testing)
testing$predRight<-pred==testing$Species
table(pred,testing$Species)

# plot predict values
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="New data Prediction")

