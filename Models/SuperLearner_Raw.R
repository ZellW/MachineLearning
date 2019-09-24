library(SuperLearner)
data(Boston, package = "MASS")

set.seed(1)

sl_lib = c("SL.xgboost", "SL.randomForest", "SL.glmnet", "SL.nnet", "SL.ksvm",
           "SL.bartMachine", "SL.kernelKnn", "SL.rpartPrune", "SL.lm", "SL.mean")

# Fit XGBoost, RF, Lasso, Neural Net, SVM, BART, K-nearest neighbors, Decision Tree, 
# OLS, and simple mean; create automatic ensemble.
result = SuperLearner(Y = Boston$medv, X = Boston[, -14], SL.library = sl_lib)

# Review performance of each algorithm and ensemble weights.
result

# Use external (aka nested) cross-validation to estimate ensemble accuracy.
# This will take a while to run.
result2 = CV.SuperLearner(Y = Boston$medv, X = Boston[, -14], SL.library = sl_lib)

# Plot performance of individual algorithms and compare to the ensemble.
plot(result2) + theme_minimal()

# Hyperparameter optimization --
# Fit elastic net with 5 different alphas: 0, 0.2, 0.4, 0.6, 0.8, 1.0.
# 0 corresponds to ridge and 1 to lasso.
enet = create.Learner("SL.glmnet", detailed_names = T,
                      tune = list(alpha = seq(0, 1, length.out = 5)))

sl_lib2 = c("SL.mean", "SL.lm", enet$names)

enet_sl = SuperLearner(Y = Boston$medv, X = Boston[, -14], SL.library = sl_lib2)

# Identify the best-performing alpha value or use the automatic ensemble.
enet_sl

# https://www.theophanomitsa.com/blog/prediction-using-the-r-superlearner-package/

library(SuperLearner)
library(ElemStatLearn)
data(prostate)

head(prostate)

#The variable to be predicted is lpsa. 
#The train variable is a dummy variable that indicates whether a case belongs to the trainset or the testset.

trainset<-prostate[prostate$train==TRUE,]
testset<-prostate[prostate$train==FALSE,]
testset1<-testset[,-10]
testset2<-testset1[,-9]
trainset1<-trainset[,-10]
trainset2<-trainset1[,-9]
#Specify the learners that will be used by the superlearner. 
mylibrary<-c("SL.glm","SL.randomForest","SL.svm","SL.glmnet") 

#Specify the training set input/output (X,ay below) and the testsetinput(newX)
X<-trainset2
newX<-testset2
ay<-trainset[,9] 

#Call the SuperLearner
out<-SuperLearner(ay, X, newX, SL.library=mylibrary) 

#These below are the predicted values by the SuperLearner.
out$SL.predict

#Let's compute now the mean square error between the predicted values and the actual testset values.
sum=0
tt<-length(testset)
for(i in 1:tt) {sum<-sum+(testset[i,9]-out$SL.predict[i])^2}

sumg<-sum/tt
sumg