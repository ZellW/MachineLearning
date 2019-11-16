# LEARNING LAB 20 ----
# EXPLAINABLE MACHINE LEARNING ----
# LLPRO BONUS ----

library(iBreakDown)

# H2O Model ----
h2o_automl <- h2o.automl(
    x = x,
    y = y,
    training_frame   = as.h2o(train_tbl),
    validation_frame = as.h2o(test_tbl),
    max_runtime_secs = 30, 
    nfolds = 0
)

h2o_automl@leaderboard

h2o_leader <- h2o_automl@leader


# DALEX SETUP ----
custom_predict <- function(model, new_data)  {
    new_data_h2o <- as.h2o(new_data)
    res <- as_tibble(h2o.predict(model, new_data_h2o))
    return(as.numeric(res$predict))
}

explainer_h2o_leader <- explain(
    model            = h2o_leader, 
    data             = features_tbl,  
    y                = response_vec,
    predict_function = custom_predict,
    label            = "h2o leader")


# SHAP ----
# LONG RUNNING SCRIPT ----
# Need to up B to get best shap values, problem is that B increases time
shap_h2o_leader <- shap(
    x = explainer_h2o_leader,
    test_tbl %>% slice(2) %>% select(-Churn),
    B = 2)

# BREAKDOWN ----
# LONG RUNNING SCRIPT ----




