# LEARNING LAB 18 ----
# TIME SERIES ANOMALY DETECTION ----
# METHOD: STL DECOMPOSITION ----
# CASE: DETECTING ANOMALIES IN GOOGLE ANALYTICS / BIG QUERY DATA ----
# Kaggle Data: https://www.kaggle.com/c/web-traffic-time-series-forecasting

# LIBRARIES ----
library(data.table)
library(tidyverse)
library(tidyquant)
library(anytime)
library(vroom)
library(anomalize)
library(plotly)
library(tictoc)

# anomalize: Observed - Seasonal Component - Trend Component
# STL - Seasonal, Trend, Loess
plot(imager::load.image("images/anomalize1.jpg"))
# Process:
# 1. time_decompose: can use STL or twitter method
# 2. anomalize using IQR or GESD to detect outliers
# 3. time_recompose calcualtes boundaries

# Useful to use anomalize to clean data so a timeseries can be better forecasted

# Options
# 1. Flag Anomalies:  Add a y/n as a flag.  Predicts well when future has anomalies similar to past anomalies.  May reduce forecasting accuracy
# 2. Clean anomalies:  Replace anomaly values with Trend + Seasonal components. Improves forecasting accuracy but does not predict future anomalies
#    Preferred

#########################

# NOTE:  May want to do a power or log transform of the data prior to using anomalize if the time series data gets wide at the end
# (will likely be added to anomalize - forecasts automatically does a power transform.)
# (Box Cox is a power transform)
# For right-skewed data—tail is on the right, positive skew—, common transformations include square root, cube root, and log.
# For left-skewed data—tail is on the left, negative skew—, common transformations include square root (constant – x), cube root (constant – x), 
# and log (constant – x).
# Because log (0) is undefined—as is the log of any negative number—, when using a log transformation, a constant should be added to all values to make 
# them all positive before transformation.  It is also sometimes helpful to add a constant when using other transformations.
# Another approach is to use a general power transformation, such as Tukey’s Ladder of Powers or a Box–Cox transformation.  
# These determine a lambda value, which is used as the power coefficient to transform values.  X.new = X ^ lambda for Tukey, 
# and X.new = (X ^ lambda – 1) / lambda for Box–Cox.
# The function transformTukey in the rcompanion package finds the lambda which makes a single vector of values—that is, one variable—as normally distributed 
# as possible with a simple power transformation. 
# The Box–Cox procedure is included in the MASS package with the function boxcox.  It uses a log-likelihood procedure to find the lambda to use to 
# transform the dependent variable for a linear model (such as an ANOVA or linear regression).  It can also be used on a single vector.

############################

# Twitter Method - not scalable
plot(imager::load.image("images/anomalize2.jpg"))

# 2.0 DATA ----
page_visits_tbl <- vroom::vroom("../../../../LargeDataFiles/anomalize/train_1.csv", delim = ",") # columnar dataa from Big Query (same as Redshift)
page_visits_tbl

# 3.1 Convert to data.table ----
page_visits_dt <- page_visits_tbl %>% as.data.table() 
rm(page_visits_tbl)

# 3.2 Setup in Long Format ----
page_visits_dt <- melt(page_visits_dt, id.vars = c("Page"), measure.vars = setdiff(names(page_visits_dt), "Page"))

names(page_visits_dt) <- c("Page", "Date", "Visits")
# page_visits_dt$Date <- anytime::anydate(page_visits_dt$Date)
setkey(page_visits_dt, "Page")

page_visits_dt %>% glimpse()


# 3.3 Prep & Functions ----
page_visit_counts_dt <- page_visits_dt %>%
    .[, .(visits_sum    = sum(Visits, na.rm = TRUE), 
          visits_mean   = mean(Visits, na.rm = TRUE),
          visits_median = median(Visits, na.rm = TRUE),
          visits_count  = sum(Visits > 0)), 
      keyby = .(Page)] %>%
    .[, ratio := visits_mean / (visits_median + 1)] 

page_visit_counts_dt %>% glimpse()


plot_page <- function(page_name) {
    g <- page_visits_dt %>%
        .[Page == page_name] %>%
        .[, Date := anytime::anydate(Date)] %>%
        ggplot(aes(Date, Visits)) +
        geom_line() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::number_format(big.mark = ","))
    
    ggplotly(g)
}

# 3.0 EDA ----

# 3.1 MOST VISITED: Pages with Highest Total Visits ----
page_visit_counts_dt %>%
    .[order(visits_sum, decreasing = TRUE)] %>%
    as_tibble()
   
plot_page("Main_Page_en.wikipedia.org_all-access_all-agents")


# 3.2 SPIKES: Highest Ratio Pages with consistent visits ----
page_visit_counts_dt %>%
    .[visits_count == 550,] %>%
    .[order(ratio, decreasing = TRUE)] %>%
    as_tibble()

plot_page("Bruce_Kingsbury_en.wikipedia.org_desktop_all-agents")

plot_page("Organisme_de_placement_collectif_en_valeurs_mobilières_fr.wikipedia.org_desktop_all-agents")



# 3.3 REGIME SHIFTS: Lowest Ratio Page with 550 consistent visits ----
page_visit_counts_dt %>%
    .[visits_count == 550,] %>%
    .[order(ratio, decreasing = FALSE)] %>%
    as_tibble()

plot_page("IMDb_en.wikipedia.org_all-access_all-agents")

plot_page("XHamster_en.wikipedia.org_mobile-web_all-agents")



# 4.0 ANOMALY DETECTION ----

# 4.1 Single Page ----
plot_page("Main_Page_en.wikipedia.org_all-access_all-agents")

wikipedia_main_page_tbl <- page_visits_dt %>%
  .[Page == "Main_Page_en.wikipedia.org_all-access_all-agents", ] %>%
  .[, Date := anytime::anydate(Date)] %>%
  as_tibble()

wikipedia_main_page_anom_tbl <- wikipedia_main_page_tbl %>%
    
    time_decompose(Visits, merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose() 

wikipedia_main_page_anom_tbl %>%
    
    plot_anomalies(time_recomposed = TRUE) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(x = "Date", y = "Visits")

# 4.2 STL + Residual Analysis----

wikipedia_main_page_anom_tbl

wikipedia_main_page_anom_tbl %>%
    plot_anomaly_decomposition(alpha_dots = 0.5) + 
    scale_y_continuous(labels = scales::number_format(big.mark = ","))

# 4.3 Exploring the anomalize arguments ----

# STL ---- 
wikipedia_main_page_tbl %>%
    # Step 1 - Decomposition
    time_decompose(
        target  = Visits, 
        method  = "stl", # stl or twitter 
        merge   = TRUE,
        frequency = "7 days",
        trend     = "3 months"
    ) %>%
    # Step 2 - Detect Anomalies in Remainder (Residual Analysis)
    anomalize(
        target = remainder, 
        method = "iqr", # iqr or gesd
        alpha  = 0.05
    ) %>%
    # Step 3 - Add Boundaries separating the anomaly lower and upper limits
    time_recompose() %>%
    
    plot_anomaly_decomposition(alpha_dots = 0.5) + 
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = "Anomaly Decomposition", subtitle = "Using Seasonal-Trend-Loess Method")



# Twitter ----
wikipedia_main_page_tbl %>%
    # Step 1 - Twitter Decomposition
    time_decompose(
        target  = Visits, 
        method  = "twitter", # stl or twitter 
        merge   = TRUE,
        frequency = "1 week",
        trend     = "3 months"
    ) %>%
    # Step 2 - Detect Anomalies in Remainder (Residual Analysis)
    anomalize(
        target = remainder, 
        method = "iqr", # iqr or gesd
        alpha  = 0.05
    ) %>%
    # Step 3 - Add Boundaries separating the anomaly lower and upper limits
    time_recompose() %>%
    
    plot_anomaly_decomposition(alpha_dots = 0.5) + 
    scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
    labs(title = "Anomaly Decomposition", subtitle = "Using Twitter (Piecewise Medians) Method")


# Trend & Frequency ----
time_scale_template()


# 4.4 Multi-Time Series - Scaled to 12 Webpages ----
page_names_most_visited <- page_visit_counts_dt %>%
    .[order(visits_sum, decreasing = TRUE)] %>%
    as_tibble() %>%
    slice(1:12) %>%
    pull(Page)

tic()
pages_most_visited_anom_tbl <-  page_visits_dt %>%
    .[Page %in% page_names_most_visited] %>%
    .[, Date := anytime::anydate(Date)] %>%
    as_tibble() %>%
  
    group_by(Page) %>%
    
    time_decompose(Visits, merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose() %>%
  
    mutate(visits_cleaned = ifelse(anomaly == "Yes", season + trend, observed))
toc()

pages_most_visited_anom_tbl

pages_most_visited_anom_tbl %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.5)


# Quick Q&A ----

# 5.0 FORECASTING - MAKING 12 FORECASTS AFTER CLEANING ----
# - METHOD: GLMNET

# 5.1 Cleaning the Time Series (Preparing Data for a Forecast) ----

g <- wikipedia_main_page_anom_tbl %>%
  
  # Clean Outliers
  mutate(visits_clean = ifelse(anomaly == "Yes", season + trend, observed)) %>%
  
  select(Date, Visits, visits_clean) %>%
  gather(key = "key", value = "value", -Date, factor_key = TRUE) %>%
  
  ggplot(aes(Date, value, color = key)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ key, ncol = 1) +
  scale_y_continuous(label = scales::number_format(big.mark = ",")) +
  expand_limits(y = 0) +
  scale_color_tq() +
  theme_tq()

ggplotly(g)

# 5.2 BEFORE VS AFTER CLEANING -----

# BEFORE CLEANING -----
pages_most_visited_anom_tbl %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(title = "Page Visits", x = "Date", y = "Visits") 

# AFTER CLEANING ----
pages_most_visited_anom_tbl %>%
  ggplot(aes(Date, visits_cleaned)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  facet_wrap(~ Page, ncol = 3, scales = "free_y") +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme_tq()

# 5.3 FORECAST RESULTS -----
# - BONUS LL PRO MEMBERS -----

# 5.3.1 FORECAST RESULTS BEFORE CLEANING ----
source("forecasting_bonus_flag_anomalies.R")
forecast_plot_flagged_anoms_vs_observed
forecast_plot_flagged_anoms_vs_cleaned

mape_flagged_anoms

# 5.3.2 FORECAST RESULTS AFTER CLEANING ----
source("forecasting_bonus_clean_anomalies.R")
forecast_plot_cleaned_anoms_vs_observed
forecast_plot_cleaned_anoms_vs_cleaned

mape_cleaned_anoms


