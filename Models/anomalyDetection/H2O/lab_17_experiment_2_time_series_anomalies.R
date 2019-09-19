# LEARNING LAB 17 ----
# ANOMALY DETECTION ----
# METHOD: H2O ISOLATION FOREST ----
# DETECTING POINT ANOMALIES ----


# 1.0 LIBARIES ----
library(h2o)
library(tidyverse)
library(tidyquant)

# 2.0 GENERATE DATA ----
set.seed(12)
random_data_tbl <- tibble(
    x = seq(20, 40, length.out = 30),
    y = x + rnorm(30, mean = 50, sd = 3)
) %>%
    mutate(y = ifelse(row_number() %in% c(10, 20), y + 40, y))

random_data_tbl %>%
    ggplot(aes(x, y)) +
    geom_point(size = 2) +
    geom_line() +
    expand_limits(x = c(0, 50), y = c(0, 50))

# 3.0 H2O ISOLATION FOREST ----
h2o.init()

random_data_h2o <- as.h2o(random_data_tbl)

isoforest <- h2o.isolationForest(
    training_frame = random_data_h2o,
    x         = names(random_data_h2o),
    ntrees    = 200,
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

quantile <- h2o.quantile(predictions, probs = 0.70)
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


# 6.0 ANOMALIZE - BETTER METHOD FOR TIME SERIES -----

library(anomalize)

random_data_with_date_tbl <- random_data_tbl %>%
    mutate(date = seq.Date(from = ymd("2019-01-01"), by = "1 day", length.out = 30)) 

random_data_with_date_tbl %>%
    ggplot(aes(date, y)) +
    geom_point()

anomalies_tbl <- random_data_with_date_tbl %>%
    time_decompose(y) %>%
    anomalize(remainder) 

anomalies_tbl %>% plot_anomaly_decomposition()

anomalies_tbl %>% plot_anomalies(size_dots = 3)

