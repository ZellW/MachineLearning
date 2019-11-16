#----------------------------------Support Vector Machines--------------------------------------------
# Read Data
letterdata <- read.csv("//192.168.1.66/r/Machine Learning with R Course/MODULES/Module-3 Black Box Method-Neural network and SVM/letterdata.csv", header=TRUE)
View(letterdata)

#training and test set
letters_train <- letterdata[1:16000, ]
View(letters_train)
 letters_test  <- letterdata[16001:20000, ]
 View(letters_test)
 
# ------Training a model on the data-----
 library(kernlab)
 letter_classifier <- ksvm(letter ~ ., data = letters_train,
                           kernel = "vanilladot") # We will use vanilladot for Linear separability 
 letter_classifier
 
 #------ Evaluate Model performance-----
 letter_predictions <- predict(letter_classifier, letters_test)
 head(letter_predictions)
 table(letter_predictions, letters_test$letter)
 agreement <- letter_predictions == letters_test$letter
 table(agreement)
 
 prop.table(table(agreement))
 
 
 #-------------------Improve Model Performance---------------------------------------
 letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                               kernel = "rbfdot")
 letter_predictions_rbf <- predict(letter_classifier_rbf,
                                   letters_test)
 table(letter_predictions_rbf, letters_test$letter)
 agreement_rbf <- letter_predictions_rbf == letters_test$letter
 table(agreement_rbf)
 prop.table(table(agreement_rbf))
