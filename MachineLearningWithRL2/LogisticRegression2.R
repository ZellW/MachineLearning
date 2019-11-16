#-------------------------Predicting Logistic Regression-------------

library(ISLR)
library(caret)
data(Smarket)
View(Smarket)

# Split data into testing and training
train<-subset(Smarket,Year<2005)
test<-subset(Smarket,Year==2005)

# Perform Logistic Regression
logit <- glm(Direction ~ Lag1+Lag2+Lag3, family='binomial', data=train)

# Run the model on the test set
test.probs <-predict(logit, test, type='response')
test.probs
pred.logit <- rep('Down',length(test.probs))
pred.logit[test.probs>=0.5] <- 'Up'
pred.logit
table(pred.logit, test$Direction)
