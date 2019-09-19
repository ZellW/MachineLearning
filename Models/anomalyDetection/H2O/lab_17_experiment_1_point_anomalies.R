# LEARNING LAB 17 ----
# ANOMALY DETECTION ----
# METHOD: H2O ISOLATION FOREST ----
# DETECTING CLUSTER ANOMALIES ----


# 1.0 LIBARIES ----
library(h2o)
library(tidyverse)
library(tidyquant)

# 2.0 GENERATE DATA ----
set.seed(12)
random_data_tbl <- tibble(
    x = rnorm(30, mean = 50, sd = 10),
    y = rnorm(30, mean = 50, sd = 10)
) %>%
    mutate(y = ifelse(row_number() == 1, y + 40, y))

random_data_tbl %>%
    ggplot(aes(x, y)) +
    geom_point(size = 2) +
    expand_limits(x = c(0, 100), y = c(0, 100))

# 3.0 H2O ISOLATION FOREST ----
h2o.init()

random_data_h2o <- as.h2o(random_data_tbl)

isoforest <- h2o.isolationForest(
    training_frame = random_data_h2o,
    x         = names(random_data_h2o),
    ntrees    = 100,
    max_depth = 12,
    seed      = 12345
)

isoforest

# 4.0 PREDICTIONS ----
predictions <- predict(isoforest, newdata = random_data_h2o)
predictions[10,]


# 5.0 VISUALIZATIONS ----

h2o.hist(predictions[,"predict"])
h2o.hist(predictions[,"mean_length"])

quantile <- h2o.quantile(predictions, probs = 0.99)
quantile

# thresh <- quantile["mean_lengthQuantiles"]
# predictions$outlier <- predictions$mean_length <= thresh %>% as.numeric()
# predictions

thresh <- quantile["predictQuantiles"]
predictions$outlier <- predictions$predict >= thresh %>% as.numeric()
predictions

predictions_tbl <- as_tibble(predictions) %>%
    bind_cols(random_data_tbl)
predictions_tbl

predictions_tbl %>%
    ggplot(aes(x, y, color = as.factor(outlier))) +
    geom_point(size = 3) +
    scale_color_tq() + 
    theme_tq() +
    expand_limits(x = c(0, 100), y = c(0, 100)) +
    labs(color = "Outlier")




