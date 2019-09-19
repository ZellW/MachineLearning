# LEARNING LAB 17 ----
# ANOMALY DETECTION ----
# METHOD: H2O ISOLATION FOREST ----
# CASE: DETECTING CREDIT CARD FRAUD ----
# Kaggle Data: https://www.kaggle.com/mlg-ulb/creditcardfraud

# 1.0 LIBRARIES ----
library(h2o)
library(tidyverse)
library(tidyquant)
library(yardstick) # Use devtools::install_github("tidymodels/yardstick")
library(vroom)
library(plotly)


credit_card_tbl <- vroom("../../LargeDataFiles/creditcard.csv")


# 1.1 CLASS IMBALANCE ----
credit_card_tbl %>% count(Class) %>% mutate(prop = n / sum(n))

# 1.2 AMOUNT SPENT VS FRAUD ----
g <- credit_card_tbl %>% select(Amount, Class) %>% ggplot(aes(Amount, fill = as.factor(Class))) +
     # geom_histogram() +
     geom_density(alpha = 0.3) +
     facet_wrap(~ Class, scales = "free_y", ncol = 1) +
     scale_x_log10(label = scales::dollar_format()) + scale_fill_tq() +
    theme_tq() + labs(title = "Fraud by Amount Spent", fill = "Fraud")

ggplotly(g)

# 2.0 H2O ISOLATION FOREST ----
h2o.init()

credit_card_h2o <- as.h2o(credit_card_tbl)
credit_card_h2o

target <- "Class" # Isolation does not need this - it is unsupervised
predictors <- setdiff(names(credit_card_h2o), target)

isoforest <- h2o.isolationForest(
    training_frame = credit_card_h2o,
    x      = predictors,
    ntrees = 100, 
    seed   = 1234 # important because of the randomized selection of a target
)

isoforest

# 3.0 PREDICTIONS ----
predictions <- predict(isoforest, newdata = credit_card_h2o) # Run on the same data - remeber - it is unsupervised
predictions

# predict = the liklihood the record is an outlier
# mean_length = average number of decision trees length - lower the number, better chance it is an outlier
# - H2O max_depth by default = 8


# 4.0 METRICS ----

# 4.1 Quantile ----
h2o.hist(predictions[,"predict"]) # most observations very low - as expected for unbalanced fraud data
h2o.hist(predictions[,"mean_length"]) # most observation are large - as expected for the same reason

quantile <- h2o.quantile(predictions, probs = 0.99)
quantile

# Result:  Anything > .35 is in 99 percentile, very likely to be an outlier
# NOTE:  predictQuantiles is the name of the value returned

thresh <- quantile["predictQuantiles"]

predictions$outlier <- predictions$predict > thresh %>% as.numeric()
predictions$class <- credit_card_h2o$Class

predictions

predictions_tbl <- as_tibble(predictions) %>% mutate(class = as.factor(class)) %>% mutate(outlier = as.factor(outlier))
predictions_tbl


# 4.2 Confusion Matrix ----

predictions_tbl %>% conf_mat(class, outlier) #conf_mat is from yardstick


# 4.3 ROC Curve ----
auc <- predictions_tbl %>% 
    roc_auc(class, predict) %>% #yardstick
    pull(.estimate) %>%
    round(3)

predictions_tbl %>% 
    roc_curve(class, predict) %>% #yardstick
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(color = palette_light()[1], size = 2) +
    geom_abline(lty = 3, size = 1) +
    theme_tq() +
    labs(title = str_glue("ROC AUC: {auc}"), 
         subtitle = "Using H2O Isolation Forest")
# Very good, xgboost


# 4.4 Precision vs Recall AUC ----
predictions_tbl %>% pr_auc(class, predict) # pr_auc is from yardstick


# 5.0 CONCLUSIONS ----
# - Anomalies (Outliers) are more often than not Fraudulent Transactions
# - Isolation Forest does a good job at detecting anomalous behaviour - xgboost may outperform
#    - Want to run with different seeds so the starting target is chaged. 10-20 times is reasonable

# 6.0 BONUS - LL PRO MEMBERS ----
# - Stabilize Predictions with PURRR

# 6.1 Repeatable Prediction Function ----
iso_forest <- function(seed) {
    
    target <- "Class"
    predictors <- setdiff(names(credit_card_h2o), target)
    
    isoforest <- h2o.isolationForest(
        training_frame = credit_card_h2o,
        x      = predictors,
        ntrees = 100, 
        seed   = seed
    )
    
    predictions <- predict(isoforest, newdata = credit_card_h2o)
    
    quantile <- h2o.quantile(predictions, probs = 0.99)
    
    thresh <- quantile["predictQuantiles"]
    
    # predictions$outlier <- predictions$predict > thresh %>% as.numeric()
    # predictions$class <- credit_card_h2o$Class
    
    predictions_tbl <- as_tibble(predictions) %>%
        # mutate(class = as.factor(class)) %>%
        mutate(row = row_number())
    predictions_tbl
    
}

iso_forest(123)

# 6.2 MAP TO MULTIPLE SEEDS ----
multiple_predictions_tbl <- tibble(seed = c(158, 8546, 4593)) %>%
    mutate(predictions = map(seed, iso_forest))

multiple_predictions_tbl

# 6.3 CALCULATE AVERAGE PREDICTIONS ----
stabilized_predictions_tbl <- multiple_predictions_tbl %>% 
    unnest(predictions) %>%
    select(row, seed, predict) %>%
    
    # Calculate stabilized predictions
    group_by(row) %>%
    summarize(mean_predict = mean(predict)) %>%
    ungroup() %>%
    
    # Combine with original data & important columns
    bind_cols(
        credit_card_tbl
    ) %>% 
    select(row, mean_predict, Time, V12, V15, Amount, Class) %>%
    
    # Detect Outliers
    mutate(outlier = ifelse(mean_predict > quantile(mean_predict, probs = 0.99), 1, 0)) %>%
    mutate(Class = as.factor(Class))

# 6.4 MEASURE ----
stabilized_predictions_tbl %>% pr_auc(Class, mean_predict)

# 6.5 VISUALIZE ----
# - Not Run Due to Time

stabilized_predictions_tbl %>%
    ggplot(aes(V12, V15, color = as.factor(outlier))) +
    geom_point(alpha = 0.2) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Anomaly Detected?", color = "Is Outlier?")

stabilized_predictions_tbl %>%
    ggplot(aes(V12, V15, color = as.factor(outlier))) +
    geom_point(alpha = 0.2) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Fraud Present?", color = "Is Fraud?")

