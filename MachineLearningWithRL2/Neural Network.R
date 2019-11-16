#----------------------------Neural network Program--------------------
# load library neuralnet
library(caret)
library(neuralnet)
library(nnet)
dim(infert)
View(infert)

# Split data into Training and Testing
trainIndex <- createDataPartition(infert$case, p=.7,list=FALSE)
infert.train <-infert[trainIndex, ]
infert.test <- infert[-trainIndex, ]
View(infert.train)
infert_model<-neuralnet(case~age+parity+induced+spontaneous,
                        data=infert.train,hidden=2,err.fct="ce",
                        
                        # ce stands for cross entrophy error.To summarize, for a neural network classifier, during training you can use mean squared error or average cross-entropy error, and average cross-entropy error is considered slightly better.
                        #If you are using back-propagation, the choice of MSE(Mean Squared Error) or ACE(Average Cross Entrophy) affects the computation of the gradient. 
                        #After training, to estimate the effectiveness of the neural network it's better to use classification error.
                        
                        linear.output=FALSE) # linear.output should be stated as FALSE to ensure that the output of the activation function is mapped to the interval[0,1] 
                                              # You shall set it to FALSE for categorical outputs
plot(infert_model)

# Overall result i.e. output for each replication
infert_model$net.result
infert_model$weights
infert_model$result.matrix

# Check the input of each variable
infert_model$covariate
# Check the output of actual variable
infert$case

# predict the result
temp_test <- subset(infert.test, select = c("age", "parity","induced","spontaneous"))
infert.results <- compute(infert_model, temp_test)

results <- data.frame(actual = infert.test$case, prediction = infert.results$net.result)
results
# Round to the nearest integer to improve readability
results$prediction <- round(results$prediction)
results
