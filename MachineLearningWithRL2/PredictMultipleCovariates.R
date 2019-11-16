#----------------Predicting with Regression Multiple Covariates----------------------

# Load Required Library
library(ISLR)
library(caret)
library(ggplot2)

# Load Data
data(Wage)
View(Wage)

# Remove logwage varaiable from the dataset
Wage<-subset(Wage,select=-c(logwage))
View(Wage)

# Get the training and testing dataset
inTrain <-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training)
dim(testing)

# Fit a linear Model
modFit<-train(wage~age+jobclass+education,method="lm",data=training)
summary(modFit)
finMod<-modFit$finalModel
print(modFit)

# Diagonistic Plot
plot(finMod,pch=19,cex=0.5,col="#00000010")
# here on x axis we will plot a fitted value, Fitted value is the prediction of the model of the training set
# and On y axis we will use Residuals that the amount of variation thats occur to fit the model.
# This plot is mainly used to detect non-linearity, unequal error variances, and outliers.
# and you can see that on top of this graph some numbers are represented it is outliers


# Predicted versus truth in test dataset
pred<-predict(modFit,testing)
qplot(wage,pred,colour=race,data=testing)
