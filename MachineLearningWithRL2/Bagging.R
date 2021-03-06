#-----------------------Bagging----------------------------
library(adabag)
# Bagging (Bootstrap Aggregating) is a way to decrease the variance of your prediction by 
# generating additional data for training from your original dataset using combinations with 
# repetitions to produce multisets of the same cardinality/size as your original data. By increasing 
# the size of your training set you can't improve the model predictive force, but just decrease the 
# variance, narrowly tuning the prediction to expected outcome. 
# 
# Boosting is a two-step approach, where one first uses subsets of the original data to produce a 
# series of averagely performing models and then "boosts" their performance by combining them together 
# using a particular cost function (=majority vote). Unlike bagging, in the classical boosting the 
# subset creation is not random and depends upon the performance of the previous models: every 
# new subsets contains the elements that were (likely to be) misclassified by previous models.

# Stacking is a similar to boosting: you also apply several models to your original data. The 
# difference here is, however, that you don't have just an empirical formula for your weight 
# function, rather you introduce a meta-level and use another model/approach to estimate the 
# input together with outputs of every model to estimate the weights or, in other words, to determine 
# what models perform well and what badly given these input data.

plot(imager::load.image("BagBoost.jpg"), axes=FALSE)
# See https://en.wikipedia.org/wiki/Bootstrap_aggregating

library(ElemStatLearn)
View(ozone)
predictors =  data.frame(ozone = ozone$ozone)
temperature  = ozone$temperature
treebag <- caret::bag(predictors, temperature, B = 10, # B is number of bootstrap samples to train over.
               bagControl = bagControl(fit = ctreeBag$fit, predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone, temperature, col = "green", pch = 19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit, predictors), pch = 19, col = "red")
points(ozone$ozone,predict(treebag, predictors), pch = 19, col = "blue")
