# LEARNING LAB 20 ----
# EXPLAINABLE MACHINE LEARNING ----
# CUSTOMER CHURN
# Data: Telco Customer Churn, IBM Sample Datasets

# Evaluate PDP (Global Interpretation), ICE (Global), LIME (Local Interpretation), Shapley (Local)

# PDP - Partial Dependency Plots:  Show how the expected model response for random observations.
plot(imager::load.image("images/pdp.jpg"), axes=FALSE)

#  Hold all other features constant and vary the feature of interest then average results
# ICE is same as PDP but does not average the results.  Can display the center 
#   (a point you specify, perhaps zero to understnad how the variable changes in a relative manner) 
#   or the trend line if desired
plot(imager::load.image("images/ice.jpg"), axes=FALSE)

# LIME:  Select a record. Probe the black box algo with permuted saples of training data, weight the
#  samples by proximity tot eh record of interest, and train a simpler, explainable model like a 
#  decision tree or LASSO to determine the most impactful variables.  
#  (Make a simpler linear model from a black box algo) 

# Shapely:  Each feature is a player in a game.  Prediction is the payout.  How much has each feature
#  contributed to the prediction?  Coalition:  Features that work together to make prediction.
#  Gain is the actual prediction minus the average for all the features.  Shapely value is the average
#  contribution to teh prediction in different coalitions.
#  Shapely is preferred because it is more accurate but can be slow and compute intensive.
#  Shapely returns a value phi which is not interpreatble.  SHAP (SHapely Additive  exPlanations)
#  The goal of SHAP is to explain the prediction of an instance by computing the contribution of each
#  feature to the prediction.This is more interpreatble than Shapely.  SHAP is additive and closer to LIME's
#  "effect".  (Like adding 20% and 30% contribution of 2 variables. 
#   Still slow - KernalSHAP is very slow.  TreeSHAP is faster.  Implemented in DALEX.

# https://christophm.github.io/interpretable-ml-book/
# https://pbiecek.github.io/PM_VEE/introduction.html

# Will be focused on IML in this demo.

# LIBRARIES ----
library(tidyverse)
library(tidyquant)

library(rsample)
library(recipes)
library(h2o)

library(iml)
library(DALEX)

library(correlationfunnel)
library(DataExplorer)

library(tictoc)

setwd("~/GitHub/MachineLearning/Models/Explainable/BSU")

customer_churn_raw_tbl <- read_csv("data/customer_churn.csv")

# 2.0 EDA ----

customer_churn_raw_tbl %>% plot_missing() # Data Explorer

# MISSING TotalCharges
customer_churn_raw_tbl %>% filter(!is.na(TotalCharges)) %>% binarize() %>%
     correlate("TotalCharges__1397.475_3794.7375") %>% plot_correlation_funnel()

customer_churn_raw_tbl %>% ggplot(aes(tenure, TotalCharges)) + geom_point(alpha = 0.2)

customer_churn_raw_tbl %>% filter(is.na(TotalCharges)) %>% select(tenure, TotalCharges)
# Shows thhat tenure = 0 and there fore NA for TotalCharges (hence the missing data)

# FIX MISSING
customer_churn_tbl <- customer_churn_raw_tbl %>%
    mutate(TotalCharges = ifelse(is.na(TotalCharges), 0, TotalCharges))

plot_missing(customer_churn_tbl)

# UNDERSTAND GLOBAL DATA ----
funnel_churn_ggplot <- customer_churn_tbl %>% binarize() %>% correlate("Churn__Yes") %>%
     plot_correlation_funnel()

funnel_churn_ggplot

# 3.0 MACHINE LEARNING ----
customer_churn_tbl %>% glimpse()

# 3.1 Preprocessing ----
set.seed(123)
rsample_splits <- initial_split(customer_churn_raw_tbl, prop = 0.8)

rec_obj <- recipe(Churn ~ ., data = training(rsample_splits)) %>%
    step_mutate(TotalCharges = ifelse(is.na(TotalCharges), 0, TotalCharges)) %>%
    step_rm(customerID) %>% step_string2factor(all_nominal()) %>% prep()

train_tbl <- bake(rec_obj, training(rsample_splits))
test_tbl  <- bake(rec_obj, testing(rsample_splits))

train_tbl

# 3.2 Random Forest ----
h2o.init()

y <- "Churn"
x <- setdiff(names(train_tbl), y)

h2o_rf <- h2o.randomForest(
    x = x,
    y = y,
    training_frame   = as.h2o(train_tbl),
    validation_frame = as.h2o(test_tbl),
    ntrees = 1000,
    max_depth = 8,
    nfolds = 5,
    stopping_metric = "AUC",
    stopping_rounds = 15,
    stopping_tolerance = 0.005,
    seed = 123
)

h2o_rf

h2o.predict(h2o_rf, newdata = as.h2o(test_tbl)) %>% as_tibble()

h2o.auc(h2o_rf, valid = TRUE)
h2o.auc(h2o_rf, xval = TRUE) # Cross-validation is typically lower but still pretty good.

# 4.0 IML ----

# 4.1 IML SETUP ----

# 4.1.1 Model Agnostic Functions
# Required features, numeric vector and function

# Setting up with Test data (20%) - Will make my explanations much faster. Get all the features for IML
features_tbl <- test_tbl %>% select(-Churn)

response_vec <- test_tbl %>% pull(Churn) %>% as.numeric() - 1 #numeric vector required for IML

# IML will use the function 
predict_h2o <- function(model, newdata) {
    results_tbl <- h2o.predict(model, newdata = as.h2o(newdata)) %>% as_tibble()
    results_tbl %>% pull(Yes) }

predict_h2o(h2o_rf, newdata = test_tbl) # Just a test

# 4.1.2 IML Predictor Object

predictor_rf <- Predictor$new(
    model       = h2o_rf,
    data        = features_tbl,
    y           = response_vec,
    predict.fun = predict_h2o,
    class       = "classification" )

# GLOBAL MODEL EXPLAINATION -----

funnel_churn_ggplot

# 5.0 PDP - Partial Dependence Plots ----

# Single Feature - Contract Type 
pdp_contract <- FeatureEffect$new(
    predictor = predictor_rf,
    feature   = "Contract",
    method    = "pdp", 
    grid.size = 20
)

pdp_contract

pdp_contract %>% plot() + expand_limits(y = 0)
# The random forest shows a difffference between contract with everything else is held constant

# 2-Way Interactions - Contract Type & Monthly Charges

# NOTE - LONG RUNNING SCRIPT - May take 1 minute or so to run 
# Tip - up grid size to get more granularity
tic()
pdp_monthly_charges_by_contract <- FeatureEffect$new(
    predictor = predictor_rf,
    feature   = c("Contract", "MonthlyCharges"),
    method    = "pdp",
    grid.size = 10
)
toc()

pdp_monthly_charges_by_contract

pdp_monthly_charges_by_contract %>% 
    plot(rug = TRUE) + expand_limits(y = 0) + theme_tq() + scale_color_tq() +
    labs(title = "How Random Forest Models Churn",
         subtitle = "2-Way Interaction between Monthly Charges & Contract Type", x = "Monthly Charges" ) 
# Suggests that all contracts with higher monthly charges tend to support churn.  
# Month-to-month contunues to more strongly indicate churn

# 6.0 ICE - Individual Conditional Expectation ----

# 6.1 Investigate Contract
ice_contract <- FeatureEffect$new(
    predictor = predictor_rf,
    feature   = "Contract",
    method    = "ice")

ice_contract

ice_contract %>% plot() + expand_limits(y = 0) + theme_tq() +
     labs(title = "ICE Plot", subtitle = "Contract Type")

# 6.2 Investigate Tenure (without centering)
ice_tenure <- FeatureEffect$new(
    predictor = predictor_rf,
    feature   = "tenure",
    method    = "ice",
    grid.size = 10,
    # No  centering
    center.at = NULL) 

ice_tenure %>% plot() + geom_smooth(color = palette_light()["green"], size = 2) +
    expand_limits(y = 0) + theme_tq() + labs(title = "ICE Plot, No Centering")
# As tenure increases, less likely to churn. At around 22 months, customers much less likely to churn

# 6.3 Investigate Tenure (Centering Churn at Zero)
ice_tenure_centered <- FeatureEffect$new(
    predictor = predictor_rf,
    feature   = "tenure",
    method    = "ice",
    grid.size = 10,
    # No centering
    center.at = 0
)

ice_tenure_centered %>% plot() + geom_smooth(color = palette_light()["green"], size = 2) +
    expand_limits(y = 0) + theme_tq() + labs(title = "ICE Plot, Centering at Zero")



# LOCAL ----

# 7.0 LIME ----
lime_rf <- LocalModel$new(
    predictor  = predictor_rf,
    x.interest = test_tbl %>% slice(2) %>% select(-Churn),
    dist.fun   = "gower",
    k          = 5)

lime_rf %>% plot() + theme_tq() + labs(title = "LIME Plot", subtitle = "Customer 2")

# 8.0 SHAP ----
# Remeber - typically more accurate than LIME
shapley_rf <- Shapley$new(
    predictor  = predictor_rf,
    x.interest = test_tbl %>% slice(2) %>% select(-Churn),
    sample.size = 200)

shapley_rf %>% plot() + theme_tq()

# 9.0 LL PRO BONUS - AUTO ML + DALEX ---- 

# H2O AutoML & DALEX
tic()
source("LL_PRO_BONUS_h2o_automl_dalex.R")
toc()

h2o_automl
h2o_automl@leaderboard
h2o_leader

# 9.1 SHAP ----
# plot(shap_h2o_leader, max_features = 5)

# 9.2 BREAKDOWN ----
breakdown_h2o_leader <- break_down(
    x = explainer_h2o_leader,
    test_tbl %>% slice(2) %>% select(-Churn),
    interactions = FALSE)

plot(breakdown_h2o_leader, max_features = 4)
# Recall that the model is predicting Churn_Yes.  Therefore, the variables in green  supports churning.
# The variable in red, in this case, is support NOT to churn.
