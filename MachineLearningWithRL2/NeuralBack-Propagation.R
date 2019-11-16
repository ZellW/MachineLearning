#----------------------------Neural network Program--------------------
# load library neuralnet
library(neuralnet)
library(nnet)
dim(infert)
View(infert)
infert_model<-neuralnet(case~age+parity+induced+spontaneous, data=infert,hidden=2,err.fct="ce",
                        linear.output=FALSE)
# ce stands for cross entrophy error.To summarize, for a neural network classifier, during training 
# you can use mean squared error or average cross-entropy error, and average cross-entropy error 
# is considered slightly better. If you are using back-propagation, the choice of 
# MSE(Mean Squared Error) or ACE(Average Cross Entrophy) affects the computation of the gradient. 
# After training, to estimate the effectiveness of the neural network it's better to use 
# classification error.
                        
# linear.output should be stated as FALSE to ensure that the output of the activation function 
# is mapped to the interval[0,1] You shall set it to FALSE for categorical outputs

infert_model
plot(infert_model)

# Overall result i.e. output for each replication
infert_model$net.result
infert_model$weights
infert_model$result.matrix

# Check the input of each varaible
infert_model$covariate
# Check the output of actual variable
infert$case
# you want to see the output of the model for these input
infert_model$net.result[[1]]

# check that if probability is less than 50 % then assign 1 otherwise assign 0
nn1<-ifelse(infert_model$net.result[[1]] > 0.5, 1, 0)
nn1
# Let's check the classification error 
misclassificationError = mean(infert$case != nn1)
misclassificationError

# Predict or compare the output Side by side
OutputPutVsPred = cbind(infert$case, nn1)
OutputPutVsPred


#------------------------------Back Propagation Algorithm-------------------------------------------

neural_backprop<- neuralnet(formula = case ~ age + parity + induced + spontaneous, 
                            data = infert, hidden = 2, learningrate = 0.01, 
                            algorithm = "backprop",err.fct = "ce", linear.output = FALSE)
neural_backprop

# compare to previous result
infert_model
plot(neural_backprop)

