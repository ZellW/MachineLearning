# BUSINESS SCIENCE ----
# LEARNING LAB 07 - TIME SERIES FORECASTING ----
# Data Used is a modified version of the UCI Machine Learning Bike Sharing Data Set
# Data Source: https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset

# May want to review feasts package that includes Hurst coefficient and spectral entropy of a time series (measure predictability)
# tsfeatures calculates Hurst coefficient (takes a univariate time series).  
# Learn about Hurst here: http://analytics-magazine.org/the-hurst-exponent-predictability-of-time-series/
# pracma also provides spectral entropy.  May want to check mean absolute scaled errors (MASE). 
# Forecastable component analysis also seems to be an interesting and new approach to determining forecastability of time series. 
# And, expectedly, there is an R package for that, as well - ForeCA.
# See https://stats.stackexchange.com/questions/23007/assessing-forecastability-of-time-series

# Autocorrelation:  A time series is a series of data points indexed in time. The fact that time series data is ordered makes it 
#unique in the data space because it often displays serial dependence. Serial dependence occurs when the value of a datapoint at 
# one time is statistically dependent on another datapoint in another time. However, this attribute of time series data violates 
# one of the fundamental assumptions of many statistical analyses — that data is statistically independent. What Is Autocorrelation?
# Autocorrelation is a type of serial dependence. Specifically, autocorrelation is when a time series is linearly related to a 
#lagged version of itself. By contrast, correlation is simply when two independent variables are linearly related.
# The presence of seasonality yields high autocorrelation.
# https://dzone.com/articles/autocorrelation-in-time-series-data

# 1.0 GOALS ----
# - Predict next 92-days (3-MONTHS) of daily bike sharing
# - This is a type of demand forecasting like supply chain, inventory and staffing

# - Time Serices:  Order matters. Prediction is called a forecast.  Autocorrelation may exist.  Univariate and multivariate.
# - Most companies today still take teh average of 3 months of data (3 month rolling average) 
#   to predict the nexr 3 months - no seasonality

# - Strategies to improve with timeseries analysis
#   1. Engineering features:  timsestamps: timetk, holidays:  timeDate, text fileds: stringr
#   2. Use alternative dat: determine hypothesis.  Find data that has re;lationships.  Use timsestamps join
#      tidyquant has stock data and econimic data.  reim has weather data
#   3. Use autocorrelation and lages to your advantage.  Correlation - relationship between X and Y.  Lag - offset copy.
#      Autocorrelation - Correlation between Log 0 (original) and Lah N (lagged copy)
#   4. Use multple approaches:  Univariate - ARIMA, Prophet.  Multivariate - GLMNET (linear), XGBOOST (tree), SVM (Regression)
#   5. Combine mutiple approaches:  stacked ensembles (H2O) [H2O does not yet support ARIMA or prophet]

# 2.0 LIBRARIES ----

# Main Packages
library(tidyverse)
library(lubridate)
library(tidyquant) # one of Matts
library(plotly)

# Time Based Feature Extraction
library(timetk) # one of Matts

# Holidays
library(timeDate)

# Weather Data
library(riem)

# Forecasting
library(forecast)
library(sweep) # one of Matts

# Machine Learning 
library(parsnip)
library(yardstick)

setwd("~/GitHub/MachineLearning/Models/TimeSeries/BSU")

# 3.0 DATA IMPORT ----

sharing_d_tbl <- read_csv("00_data/sharing_d_tbl.csv") 

sharing_d_tbl

# 2.0 VISUALIZATION ----
# - Taught in DS4B 101-R: Week 4 (ggplot2), Week 7 (plotly)

g <- sharing_d_tbl %>% ggplot(aes(date, cnt)) + geom_line(alpha = 0.5, color = "#2c3e50") +
    geom_smooth(method = "loess", span = 0.5) + theme_tq() 

ggplotly(g) %>% layout(xaxis = list(rangeslider = list(type = "date")))

# 3.0 HYPOTHESIS ----
# - Bike Sharing is related to:
#   - Growth Trend
#   - Seasonality (aka Time of the year)
#   - Holidays
#   - Weather
#   - Extreme Events


# 4.0 ALTERNATIVE DATA ----

# 4.1 Time-based Features ----
sharing_d_tbl <- sharing_d_tbl %>% tk_augment_timeseries_signature() %>%
    select(date, cnt, index.num, year, half, quarter, month.lbl, day, wday.lbl)

sharing_d_tbl

# 4.2 Holidays ----
holidays <- holidayNYSE(year = c(2011, 2012)) %>% ymd()

sharing_d_tbl <- sharing_d_tbl %>% mutate(holiday = case_when(date %in% holidays ~ 1, TRUE ~ 0))

sharing_d_tbl

# 4.3 Weather Data for DC ----
# - reim package: http://ropensci.github.io/riem/index.html
# - Map: https://mesonet.agron.iastate.edu/request/download.phtml?network=VA_ASOS

# VA Weather Stations, Use DCA (See Map)
riem::riem_stations("VA_ASOS") 
weather_dc_tbl <- riem_measures("DCA", date_start = "2011-01-01", date_end = "2013-01-01")

weather_dc_tbl 

weather_dc_tbl %>% glimpse()

weather_dc_tbl %>% select(valid, tmpf, dwpf, relh, sknt, vsby, skyc1, peak_wind_gust, feel) %>%
    map_df(~ sum(is.na(.)))

# Fill in NAs and group from hours to day
weather_dc_d_tbl <- weather_dc_tbl %>% select(valid, tmpf, dwpf, relh, sknt, vsby, peak_wind_gust, feel) %>%
    mutate(date = as_date(valid)) %>% select(-valid) %>% group_by(date) %>%
    summarize_all(~ median(., na.rm = TRUE)) %>%
    fill(peak_wind_gust, .direction = "down") %>% fill(peak_wind_gust, .direction = "up") 

weather_dc_d_tbl

sharing_d_tbl <- sharing_d_tbl %>% left_join(weather_dc_d_tbl, by = "date") 

sharing_d_tbl

# 4.0 INVESIGATE AUTOCORRELATION ----

autocorrelate <- function(data, value, lags = 0:20) {
    
    value_expr <- enquo(value)
    
    acf_values <- data %>% select(!! value_expr) %>% pull() %>%
        acf(lag.max = tail(lags, 1), plot = FALSE) %>%
        .$acf %>%
        .[,,1]
    
    ret <- tibble(acf = acf_values) %>%  rowid_to_column(var = "lag") %>%
        mutate(lag = lag - 1) %>% filter(lag %in% lags)
    
    return(ret)
}

g <- sharing_d_tbl %>%
    autocorrelate(cnt, lags = 0:nrow(.)) %>%
    ggplot(aes(lag, acf)) + geom_point(alpha = 0.5, color = "#2c3e50") +  expand_limits(y = c(-1, 1)) +
    theme_tq() + labs(title = "Autocorrelation")

ggplotly(g)

# Notes on the data and the plot:
sharing_d_tbl %>% autocorrelate(cnt, lags = 0:nrow(.)) %>% head()
# - The first record always has perfect correlation
# - Looking 3 months forward (92 days) means we cannot use any data before 92 days because there will be NAs. 
#   At 93 days, correlation is only 0.196. (But that is too much data to lose)
# - If there were a much larger spike between 300/400 day lag, that might have been useful
# - If there had been more than just 2 years of data (like 5 or 10), the other highs and lows might have been interesting
# - THereforee, Autocorrelation is not helpful in this data

# 5.0 TRAIN/TEST SPLIT ----
# - Predict next 92-days (3-MONTHS) of daily bike sharing
# - Obviously cannot randomly selet data
train_test_split_date <- "2012-10-01"

train_tbl <- sharing_d_tbl %>% filter(date < ymd(train_test_split_date))

test_tbl <- sharing_d_tbl %>% filter(date >= ymd(train_test_split_date))

# What Autocorrelation can we use in a multivariate model? (Not sure why this is here. . . delete?)

nrow(test_tbl)

# 6.0 EVALUATE CURRENT SYSTEM ----
# - 60-Day moving average

moving_average_train_tbl <- train_tbl %>% select(date, cnt) %>%
    mutate(moving_average = rollmean(cnt, k = 92, na.pad = TRUE, align = "right")) 
# There are 92 NAs in the moving_average column

g <- moving_average_train_tbl %>% bind_rows(test_tbl %>% select(date, cnt)) %>%
    fill(moving_average, .direction = "down") %>%
    ggplot(aes(date, cnt)) +
    geom_vline(xintercept = ymd(train_test_split_date), color = "red") +
    geom_point(color = "#2c3e50") + geom_line(aes(y = moving_average), size = 1, color = "blue") +
    theme_tq() 

ggplotly(g)

test_tbl %>%
    select(cnt) %>% mutate(moving_average = moving_average_train_tbl %>% tail(1) %>% 
                                pull(moving_average)) %>% mae(cnt, moving_average)
# result suggests the forecast is off 2007 bikes on average each day!
# mae is from yarstick

# 7.0 MODELING ----

# 7.1 ARIMA ----
# Univariate analysis - ARIMA is slow

### NOT RUN - Takes about 5 minutes
# fit_arima <- train_tbl %>%  tk_ts(select = cnt, frequency = 364) %>%
#     auto.arima(stepwise = FALSE, parallel = TRUE, num.cores = 4)
#
## The model above did not work well.  The code below was ARIMA optimized
# 
# fit_arima <- train_tbl %>%
#     tk_ts(select = cnt, frequency = 364) %>%
#     Arima(order = c(1, 0, 2), seasonal = c(0, 1, 0), include.drift = TRUE)
# 
# fcast_arima_tbl <- forecast(fit_arima, h = nrow(test_tbl)) %>%
#     sw_sweep(timetk_idx = TRUE, rename_index = "date") #sw_sweep converst to a dataframe making plotting easier

### SAVE RESULTS
# fs::dir_create("00_model")
# fcast_tbl %>% write_rds("00_model/fcast_arima_tbl.rds")

fcast_arima_tbl <- read_rds("00_model/fcast_arima_tbl.rds")

g <- fcast_arima_tbl %>%  ggplot(aes(date, cnt, color = key)) + geom_point(data = test_tbl %>% mutate(key = "actual")) +
    geom_point(alpha = 0.5) + theme_tq() + scale_color_tq()+ labs(title = "ARIMA(1,0,2)(0,1,0)[364] with drift")
    
ggplotly(g) %>% layout(xaxis = list(rangeslider = list(type = "date")))
# pretty good result but it takes a long time so this could be a problem so use other methods that are faster and more accurate

fcast_arima_tbl %>% filter(key == "forecast") %>% rename(.pred = cnt) %>% select(date, .pred) %>%
    left_join(test_tbl) %>% select(date:cnt) %>% mae(truth = cnt, estimate = .pred)
# mae is better - 1368 bikes per day off 

# 7.2.0 Machine Learning ----

# 7.2.2 GLMNET ----

glmnet_fit <- linear_reg(mode = "regression", penalty = 0.01, mixture = 0.5) %>%
    set_engine("glmnet") %>% fit.model_spec(cnt ~ . - date, data = sharing_d_tbl)

fcast_glmnet_tbl <- glmnet_fit %>% predict(new_data = test_tbl) %>%
    bind_cols(test_tbl) %>% bind_rows(train_tbl) %>% arrange(date) %>%
    select(date, cnt, .pred) %>% gather(key = "key", value = "cnt", -date) %>%
    filter(!is.na(cnt)) %>% mutate(key = case_when(key == "cnt" ~ "actual", TRUE ~ "forecast"))

g <- fcast_glmnet_tbl %>% ggplot(aes(date, cnt, color = key)) +
    # geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
    #             color = "white", fill = "dodgerblue") +
    geom_point(data = test_tbl %>% mutate(key = "actual")) + geom_point(alpha = 0.5) +
    theme_tq() + scale_color_tq() + labs(title = "GLMNET: Penalty = 0.01, Mixture = 0.5")

ggplotly(g) %>% layout(xaxis = list(rangeslider = list(type = "date")))

fcast_glmnet_tbl %>% filter(key == "forecast") %>% rename(.pred = cnt) %>%
    select(date, .pred) %>% left_join(test_tbl) %>% select(date:cnt) %>%
    mae(truth = cnt, estimate = .pred)
# Now off 985 bikes/day.  1 - 985/2007 ~ 50% improvement
# Remeber, future forecasts assume you know the weather ahead of time.  Might use past weather history

# 8.0 STACKING ----
# Just an average; MAE gets a bit worse
fcast_arima_tbl %>% select(date:cnt) %>% left_join(fcast_glmnet_tbl, by = c("date", "key")) %>%
    rename(cnt_arima = cnt.x, cnt_glmnet = cnt.y) %>%
    filter(key == "forecast") %>%
    mutate(cnt_stacked = (cnt_arima + cnt_glmnet) / 2) %>%
    left_join(test_tbl %>% select(date, cnt), by = "date") %>%
    mae(cnt, cnt_stacked)
# MAE grows to 1042
