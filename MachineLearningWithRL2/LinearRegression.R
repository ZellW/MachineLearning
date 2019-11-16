#--------------Predicting with Linear Regression-------------------------

# Load library
library(caret)

# Load Data
data(faithful)
# View faithful Data
View(faithful)
set.seed(333)

# Get Training Data
inTrain <- createDataPartition(y=faithful$waiting,p=0.5,list=FALSE)
trainFaith<-faithful[inTrain,]

# Get Testing Data
testFaith<-faithful[-inTrain,]

# View some columns of the Training data
head(trainFaith)


# plot a Model Fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

# Fit a linear Model
lm_model<-lm(eruptions~waiting,data=trainFaith)
summary(lm_model)

# fit a line along the points
lines(trainFaith$waiting,lm_model$fitted,lwd=3)

# Predict a new value
newdata<-data.frame(waiting = 80)
predict(lm_model,newdata)

# Prediction Intervals
pred1<-predict(lm_model,newdata = testFaith,interval="prediction")
ord<-order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",col=c(1,2,2),lty=c(1,1,1),lwd=3)

