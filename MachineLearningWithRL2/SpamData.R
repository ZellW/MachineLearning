# Load library carat
library(caret)
# Load library kernlab
library(kernlab)
data(spam)
 View(spam)
 
 # I will partition the data of the spam type in whch I will take 75 % percent data for training
 # and 25 % percent data for testing
inTrain<-createDataPartition(y = spam$type, p=0.75,list=FALSE)

training <- spam[inTrain,]
View(training)
dim(training)
testing<-spam[-inTrain,]
View(testing)
dim(testing)

# Fit a model
set.seed(32343)
modelFit<-train(type~., data = training, methods = "glm")
modelFit

           
# Prediction the testing sample
predictions<-predict(modelFit,newdata=testing)
predictions 

# Confusion Matrix
confusionMatrix(predictions,testing$type)
