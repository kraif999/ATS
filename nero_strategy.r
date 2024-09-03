# Copyright (c) August 2024 Oleh Bilyk
# Price-time based strategies

# September the 3rd, 2024

# Apply historical time period split
# Modify what metrics is being computed (add trade list across date, % of winning signals, etc.)
# Review Search and Judgement section in Robert Pardo

############### Strategy based on Gerchyk ideas (June 2024) ################

######################################################
# Specify trading strategy parameters
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()
symbol <- "BTC-USD" # bitcoin
capital <- 50000 # units of initial capital invested
leverage <- 1 # financial leverage used to calculate number of positions in estimate_performance in Strategy class
# Also, it is assumed 100% portfolio investment (number of positions =  capital / price per unit).
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()
data_fetcher$plot_close_or_rets(type = "close")

# Convert to tibble format which is further used in Strategy:
ts_df <- data.frame(Date = index(ts), coredata(ts)) %>%
  rename_with(~ sub(".*\\.", "", .), everything()) %>%
  na.omit() %>%
  as_tibble()

head(ts_df)
tail(ts)

# Historical levels
ts_df <- ts_df %>%
  mutate(
    # max
    global_max = cummax(ts_df$Close),
    local_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
    local_half_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
    local_month_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
    local_week_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE), # week
    
    # min
    global_min = cummin(ts_df$Close), # not important
    local_year_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
    local_half_year_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
    local_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
    local_week_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE) # week
    
  ) 


library(dplyr)
library(slider)
library(plotly)

# Define the function to plot candlestick chart with horizontal historical levels
plot_candles <- function(df, ndays) {
  
  # Get the last ndays of data
  df_tail <- tail(df, ndays)
  
  # Latest date in the tail data
  latest_date <- max(df_tail$Date)
  
  # Calculate historical levels
  ts_df <- df %>%
    mutate(
      # max
      global_max = cummax(Close),
      local_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
      local_half_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
      local_month_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
      local_week_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE), # week
      
      # min
      local_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
      local_week_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE) # week
    )
  
  # Get the latest levels for the horizontal lines
  latest_levels <- ts_df %>%
    filter(Date == latest_date) %>%
    select(local_year_max, local_half_year_max, local_month_max, local_week_max,
           local_month_min, local_week_min) %>%
    slice(1)
  
  # Plot the candlestick chart
  fig <- df_tail %>% plot_ly(x = ~Date, type= "candlestick",
                             open = ~Open, close = ~Close,
                             high = ~High, low = ~Low) 
  
  # Add horizontal lines for historical levels
  fig <- fig %>% 
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_year_max, nrow(df_tail)),
              line = list(color = 'blue', dash = 'dash', width = 1), name = 'Year Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_half_year_max, nrow(df_tail)),
              line = list(color = 'green', dash = 'dash', width = 1), name = 'Half Year Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_max, nrow(df_tail)),
              line = list(color = 'red', dash = 'dash', width = 1), name = 'Month Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_max, nrow(df_tail)),
              line = list(color = 'purple', dash = 'dash', width = 1), name = 'Week Max') %>%
    
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_min, nrow(df_tail)),
              line = list(color = 'red', dash = 'dot', width = 1), name = 'Month Min') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_min, nrow(df_tail)),
              line = list(color = 'purple', dash = 'dot', width = 1), name = 'Week Min')
  
  # Add layout title
  fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
  
  # Return the plot
  fig
}

plot_candles(ts_df, 30)

# Building blocks
# Risk management: 3R vs R
# Money management: position sizing
# Estimate performance (adjust estimate_performance in Strategy class or write new method in Nero class, the latter is preffered)


# Define Data parent class (DataFetcher)
DataFetcher <- R6Class(
  "DataFetcher",
  public = list(
    symbol = NULL,
    from_date = NULL,
    to_date = NULL,
    type = NULL,

initialize = function(symbol, from_date, to_date, type = "rets") {
      self$symbol <- symbol
      self$from_date <- from_date
      self$to_date <- to_date
      self$type <- type
},
  
# Download data for multiple symbols and save tibble data frame in wide format    
convert_xts_to_wide_df = function() {
    dfs <- list()
    # Iterate through symbols to retrieve data and create data frames
    for (symbol in self$symbol) {
    # Fetch data for symbol
    tsRaw <- getSymbols(symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    data <- coredata(ts)
    dates <- index(ts)
    close_price <- as.numeric(data[, 4])
    # Create data frame for the symbol
    switch(
        self$type,
        "rets" = {
        # Combine Date and log returns into a data.frame
        df <- data.frame(Date = as.Date(dates), 
                            rets = as.numeric(log(close_price / lag(close_price)))) %>%
            na.omit %>%
            unnest(rets)
        },
        "Close" = {
        df <- data.frame(Date = as.Date(dates), Close = close_price) %>% 
            na.omit %>%
            unnest
        },
        stop("Invalid value for 'type' argument. Choose 'rets' or 'Close'.")
    )
    # Store the data frame in the list
    dfs[[symbol]] <- df
    }
    # Combine all data frames into a single wide data frame
    wide_df <- bind_rows(dfs, .id = "symbol") %>%
    pivot_wider(names_from = "symbol", values_from = ifelse(self$type == "rets", "rets", "Close"), 
                names_prefix = ifelse(self$type == "rets", "rets_", "Close_")) %>%
                    na.omit()
    return(wide_df)
},

# Download xts data and compute log returns
download_xts_data = function() {
    tsRaw <- quantmod::getSymbols(self$symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    # UseMethod("mutate") : no applicable method for 'mutate' applied to an object of class "c('xts', 'zoo')"
    #ts$value <-  log(ts[,paste0(self$symbol, ".Close")]) - log(lag(ts[,paste0(self$symbol, ".Close")]))
    ts$value <- log(ts[, grep("\\.Close$", colnames(ts))]) - log(lag(ts[, grep("\\.Close$", colnames(ts))]))
    ts <- na.omit(ts)
    attr(ts, "na.action") <- NULL

    return(ts)
},

# Visualize Close price or returns
plot_close_or_rets = function(type = "close") {
  colnames(ts) <- sub(".*\\.", "", colnames(ts))
  switch(type,
        "close" = {
          # Plot Close
          ggplot(ts, aes(x = as.Date(index(ts)))) +
            geom_line(aes(y = Close, color = "Active Strategy"), color = "black") +
            geom_hline(yintercept = mean(ts$Close), linetype = "dashed", color = "blue") +
            labs(title = "Close price",
                  x = "Date",
                  y = "Close price") +
            scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
            theme_minimal()
        },

        "rets" = {
         ggplot(ts, aes(x = as.Date(index(ts)))) +
            geom_line(aes(y = value, color = "Active Strategy"), color = "black") +
            geom_hline(yintercept = mean(ts$value), linetype = "dashed", color = "blue") +
            labs(title = "Log returns",
                  x = "Date",
                  y = "log return") +
            scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
            theme_minimal()         
        },

        "rets_hist" = {

          ggplot(ts, aes(value)) +
            geom_histogram(aes(y = after_stat(density)), binwidth = 0.001, fill = "lightblue", color = "black", boundary = 0.5) +
            stat_function(fun = dnorm, args = list(mean = mean(ts$value), sd = sd(ts$value)), color = "red", size = 1) +
            labs(title = "Histogram with Normal Distribution Curve", 
                x = "Numeric Vector", 
                y = "Density") +
            theme_minimal()
        },
        stop("Invalid type. Choose either 'close' or 'rets'.")
  )
},

# Compute missing ratio of values that are not available for Close price (US public holidays are not considered)
compute_NA_close_price_ratio = function() {

  dates <- data.frame(Date = as.Date(seq(from = from_date, to = to_date, by = "day") %>%
    `[`(., !weekdays(.) %in% c("Saturday", "Sunday"))))

  ts_df <- data.frame(ts)
  ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything())

  df <- merge(ts_df %>% select(Date, Close), dates, by = "Date", all.y = TRUE)
  print(paste("Missing Close values ratio considering all dates is:", sum(is.na(df$Close))/nrow(df)))

  # Compute missing ratio across business weekdays
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") 
  NA_weekdays <- lapply(days, function(day) {
  missing_ratio <- sum(is.na(df[df$Date %in% df$Date[weekdays(df$Date) %in% day],]$Close)) / nrow(df[df$Date %in% df$Date[weekdays(df$Date) %in% day],])
  return(missing_ratio)
  })

  names(NA_weekdays) <- days
  # Convert the list of missing ratios to a data frame
  NA_weekdays <- data.frame(Weekday = names(NA_weekdays), Missing_Ratio = unlist(NA_weekdays))

  return(NA_weekdays)
}
  )
)

# Historical period
# Segments of historical period (for example, the consecutive periods of 2-years within 10-years entire period)
# Busket of instruments/markets (selected from instr_config.json file)

######################################################
# Specify trading strategy parameters
######################################################
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()
symbol <- "BTC-USD" # cryptocurrencies
capital <- 50000 # units of initial capital invested
leverage <- 1 # financial leverage used to calculate number of positions in estimate_performance in Strategy class
# Also, it is assumed 100% portfolio investment (number of positions =  capital / price per unit).
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()
data_fetcher$plot_close_or_rets(type = "close")
data_fetcher$plot_close_or_rets(type = "rets")
data_fetcher$plot_close_or_rets(type = "rets_hist")
data_fetcher$compute_NA_close_price_ratio()

# Define TSA class (time series analysis class)
# The purpose is to understand a market state (bull, bear, congested, cyclcing - to be added) and its characteristics
TSA <- R6Class(
  "TSA",
  public = list(
    original_data = NULL,
    data = NULL,

initialize = function(data) {
      self$original_data <- data
      self$data <- private$preprocess_data(freq = "daily")
},
    
estimate_stationarity = function(freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      adf <- aTSA::adf.test(self$data$value, output = FALSE)[["type1"]] %>% data.frame()
      
      # Plot values
      if (plot_flag) {
        print(
          ggplot(self$data, aes(x = Date, y = value)) +
            geom_line() +
            labs(title = "Stationarity (ADF test)") +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme_minimal()
        )
      }
      return(adf)
},
    
estimate_autocorr_rets_vol = function(test, freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      switch(test,
             "rets" = {
               # ACF test for returns  
               acf <- acf(self$data$value, main = "Autocorrelation", plot = TRUE)
               return(list(acf = acf))
             },
             "vol" = {
               # ACF test for squared returns
               acf2 <- acf(self$data$value^2, main = "Volatility clustering", plot = TRUE)
               return(list(acf = acf2))
             }
      )
},
    
estimate_seasonality = function(freq = "daily") {
      self$data <- private$preprocess_data(freq)
      self$data <- ts(self$data$value, frequency = ifelse(freq == "daily", 26, 52))
      # Decompose the time series
      decomposed_data <- decompose(self$data) # decompose into 1) original ts, 2) trend component, 3) seasonal component, 4) residual component
      # Plot the decomposed components
      plot(decomposed_data)
},
    
estimate_heteroscedasticity = function(freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      
      # Fit a linear regression model to the squared log returns
      model <- lm(self$data$value^2 ~ self$data$value, data = data.frame(value = self$data$value))
      
      # Perform Breusch-Pagan test and print results
      bp_test <- bptest(model)
      print(bp_test)
      
      # Get the residuals from the linear regression model
      residuals <- residuals(model)
      
      # Create a data frame for the residuals
      residual_df <- data.frame(Residuals = residuals, Observation = 1:length(residuals))
      
      # Plot using ggplot
      if (plot_flag) {
        print(
          ggplot(residual_df, aes(x = Observation, y = Residuals)) +
            geom_point() +
            geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
            labs(title = "Residuals from Linear Regression Model",
                 x = "Observation Number",
                 y = "Residuals") +
            theme_minimal()
        )
      }
},

estimate_arch_effects = function(freq = "daily", p = 1, q = 1, plot_flag = TRUE) {
  
  self$data <- private$preprocess_data(freq)
  
  # Fit an ARIMA model
  fit <- arima(self$data$value, order=c(p,0,q))
  
  # Engle's ARCH test
  arch <- aTSA::arch.test(fit)
  return(arch)
},

estimate_outliers = function(test, freq = "daily", plot_flag = TRUE, q1 = NULL, q3 = NULL, threshold = 1.5) {
  self$data <- private$preprocess_data(freq)
  switch(
    test,
    # Super smoothing
    "supsmu" = {  
    ts_rets <- ts(self$data$value, frequency = 1)

    # Clean the time series using tsclean
    clean_ts <- tsclean(ts_rets)

    # Identify outliers using tsoutliers
    outliers <- tsoutliers(ts_rets)

    # Plot the original time series, cleaned time series, and outliers
    if (plot_flag) {
      print(
        autoplot(clean_ts, series = "Cleaned", color = 'red', lwd = 0.9) +
        autolayer(ts_rets, series = "Original", color = 'gray', lwd = 1) +
        geom_point(data = outliers %>% as.data.frame(),
        aes(x = index, y = replacements), col = 'blue') +
        labs(x = "Date", y = "Close Price", title = "Time Series with Outliers Highlighted (via SuperSmoothing 'tsoutliers' function)")
        )
      }
    
    # Create a data frame from the outliers list
    outliers_df <- data.frame(
      index = outliers$index,
      replacements = outliers$replacements
    )
    return(outliers_df)
    },

    "zscore" = {
    ts_rets <- ts(self$data$value, frequency = 1)
     # Calculate Modified Z-scores
     median_val <- median(ts_rets)
     mad_val <- mad(ts_rets)

     modified_z_scores <- abs(0.6745 * (ts_rets - median_val) / mad_val)

     # Identify outliers (e.g., Modified Z-score > 3.5)
     outliers_modified <- which(modified_z_scores > 3.5)

     # Plot outliers
      if (plot_flag) {
        print(
          ggplot(data.frame(Date = time(ts_rets), Value = as.numeric(ts_rets)), aes(x = Date, y = Value)) +
            geom_line() +
            geom_point(data = data.frame(Date = time(ts_rets)[outliers_modified], Value = ts_rets[outliers_modified]), aes(x = Date, y = Value), color = "red") +
            labs(title = "Time Series Data with Outliers Identified by Modified Z-Score")
        )
      }

      # Create a data frame from the outliers list
      outliers_df <- data.frame(
        index = time(ts_rets)[outliers_modified],
        replacements = ts_rets[outliers_modified]
      )
      return(outliers_df)
        },
      
    "fences" = {
    ts_rets <- ts(self$data$value, frequency = 1)
    # Calculate IQR
    Q1 <- quantile(ts_rets, q1)
    Q3 <- quantile(ts_rets, q3)
    IQR_val <- Q3 - Q1

    # Identify outliers (e.g., values < Q1 - 1.5 * IQR or values > Q3 + 1.5 * IQR)
    outliers_tukey <- which(ts_rets < (Q1 - threshold * IQR_val) | ts_rets > (Q3 + threshold * IQR_val))

    # Plot outliers
    if (plot_flag) {
      print(
    ggplot(data.frame(Date = time(ts_rets), Value = as.numeric(ts_rets)), aes(x = Date, y = Value)) +
        geom_line() +
        geom_point(data = data.frame(Date = time(ts_rets)[outliers_tukey], Value = ts_rets[outliers_tukey]), aes(x = Date, y = Value), color = "red", size = 2) +
        labs(title = "Time Series Data with Outliers Identified by Tukey's Fences")
            )   
          }
        }
      )
},

compute_wkd_rets = function(freq = "daily") {
  self$data <- private$preprocess_data(freq)
  self$data <- self$data %>%
    mutate(weekday = wday(Date, label = TRUE, abbr = TRUE)) 

  # Compute average return on weekdays
  avg_wkd_rets <- self$data %>%
    group_by(weekday) %>%
      summarize(avg_return = mean(value, na.rm = TRUE)) %>%
        arrange(weekday)
  
  # Compute longest consequtive streak of positive and negative weekday returns
  positive <- self$data %>%
    group_by(weekday) %>%
      summarise(longest_series_weekdays = max(rle(value > 0)$lengths * (rle(value > 0)$values))) %>%
          ungroup()

  negative <- self$data %>%
    mutate(weekday = wday(Date, label = TRUE, abbr = TRUE)) %>%
      group_by(weekday) %>%
        summarise(longest_series_weekdays = max(rle(value < 0)$lengths * (rle(value < 0)$values))) %>%
            ungroup()

  res_longest <- merge(positive, negative, by = "weekday", all = TRUE) %>%
    rename(longest_positive = longest_series_weekdays.x, longest_negative = longest_series_weekdays.y) %>% 
      arrange(weekday)

  # Test hypothesis if any weekday return is statistically different from rets mean return

  # Overall mean return
  overall_mean <- mean(self$data$value)

  # Perform Wilcoxon signed-rank test for each weekday
  avg_wkd_rets <- avg_wkd_rets %>%
    rowwise() %>%
      mutate(
    test_statistic = wilcox.test(self$data$value, mu = avg_return, alternative = "two.sided")$statistic,
    p_value = format(wilcox.test(self$data$value, mu = avg_return, alternative = "two.sided")$p.value, nsmall = 5)
  )

  return(list(avg_wkd_rets = avg_wkd_rets, res_longest = res_longest))
},

compute_summary_statistics = function(freq = "daily") {
  self$data <- private$preprocess_data(freq)
  summary_stats <- self$data %>%
    summarise(
      mean_return = mean(value),
      median_return = median(value),
      sd_return = sd(value),
      skewness = skewness(value),
      kurtosis = kurtosis(value)
    )
  
  return(summary_stats)
}

  ), # end of public list arguments
  
private = list(

preprocess_data = function(freq) {
      # Convert xts data into tibble 
      data <- data.frame(self$original_data)  
      data <- data %>%
        rename_with(~ sub(".*\\.", "", .), everything()) %>%
        mutate(Date = as.Date(rownames(data))) %>%
        select(Date, everything()) %>%
        na.omit() %>%
        as.tibble()
      
      # Choose values frequency: daily overlapping values or bi-weekly non-overlapping 
      switch(
        freq,
        "daily" = {
          data <- data
        },
        "biweekly" = {
          bdates <- seq.Date(
            from = (from_date + 0:6)[weekdays(from_date + 0:6) %in% "Wednesday"], 
            to = (to_date - 0:6)[weekdays(to_date - 0:6) %in% "Wednesday"], by = 14)
            data <- data %>% filter(Date %in% bdates)
        },
        stop("Invalid value for 'freq' argument. Choose 'daily' or 'biweekly'.")
      )
      return(data)
    }
  )
)

# Instances of TSA
# daily overlapping returns
tsa <- TSA$new(ts)
tsa$estimate_stationarity(freq = "daily")
tsa$estimate_heteroscedasticity(freq = "daily")
tsa$estimate_autocorr_rets_vol(test = "rets", freq = "daily", plot_flag = TRUE)
tsa$estimate_seasonality(freq = "daily")
tsa$estimate_arch_effects(freq = "daily", p = 1, q = 1)
tsa$compute_wkd_rets(freq = "daily")
tsa$compute_summary_statistics(freq = "daily")

# biweekly non-overlapping returns
tsa$estimate_stationarity(freq = "biweekly")
tsa$estimate_heteroscedasticity(freq = "biweekly")
tsa$estimate_autocorr_rets_vol(test = "rets", freq = "biweekly", plot_flag = TRUE)
tsa$estimate_seasonality(freq = "biweekly")
tsa$estimate_outliers(test = "zscore", freq = "daily", plot_flag = TRUE)
tsa$estimate_outliers(test = "fences", freq = "biweekly", plot_flag = TRUE, q1 = 0.25, q3 = 0.75, threshold = 1.5)
tsa$estimate_outliers(test = "fences", freq = "biweekly", plot_flag = TRUE, q1 = 0.25, q3 = 0.75, threshold = 2)


# Define parent Strategy class (modified slightly based on the ideas in Robert Parto)
Strategy <- R6Class(
  "Strategy",
  public = list(
    data = NULL,

initialize = function(data) {
      self$data <- data
},

# Signal generation, specific to each sub-class (generic method)
generate_signals = function() {
},

convert_to_tibble = function(ts) {
    ts_df <- data.frame(ts)
    ts_df <- ts_df %>%
        rename_with(~ sub(".*\\.", "", .), everything()) %>%
          mutate(Date = as.Date(as.character(rownames(.)))) %>%
            select(Date, everything()) %>%
                na.omit() %>% 
                    as_tibble()
    return(ts_df)

    # ts_df <- data.frame(Date = index(ts), coredata(ts)) %>%
    #   rename_with(~ sub(".*\\.", "", .), everything()) %>%
    #     na.omit() %>%
    #       as_tibble()
    # return(ts_df)
},

# Performance estimation for Active and Passive strategies (annualized return, standard deviation, Information ratio, maximum drawdown, trades count, success prediction rate)
# estimate_performance = function() {

#   self$generate_signals()  # Call generate_signals dynamically

#   self$data <- self$data %>% 
#     mutate(
#     pnlActive = c(0, diff(Close) * signal[-length(Close)]),
#     pnlPassive = c(0, diff(Close)),
#     nopActive = 0,
#     nopPassive = 0,
#     eqlActive = 0,
#     eqlPassive = 0
#   )

#   # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
#   self$data$eqlActive[1] <- capital
#   self$data$nopActive[1] <- floor(capital / (self$data$Close[1] / leverage))
#   self$data$eqlPassive[1] <- capital
#   self$data$nopPassive[1] <- floor(capital / (self$data$Close[1] / leverage))   

#   # Check if "L" column (probability indicator) exists
#   has_L <- "nop_sizing" %in% names(self$data)
#   has_Vol <- "vol_nop_sizing" %in% names(self$data)

#   for (i in 2:nrow(self$data)) {
#     # Active
#     pnlActive <- self$data$pnlActive[i]
#     prev_nop_Active <- floor(self$data$nopActive[i - 1])
#     current_nop_Active <- floor((self$data$eqlActive[i - 1]) / (self$data$Close[i] / leverage))
#     self$data$eqlActive[i] <- self$data$eqlActive[i - 1] + prev_nop_Active * pnlActive
    
#     # Calculate nopActive based on the presence of the "L" column
#     if (has_L) {
#       self$data$nopActive[i] <- ifelse(
#         self$data$L[i], 
#         current_nop_Active * self$data$nop_sizing[i], 
#         current_nop_Active
#       )
#     } else if (has_Vol) {
#       self$data$nopActive[i] <- ifelse(
#         #self$data$L[i], 
#         self$data$vol_nop_sizing[i], 
#         current_nop_Active * self$data$vol_nop_sizing[i], 
#         current_nop_Active
#       )
#     } else {
#       self$data$nopActive[i] <- current_nop_Active
#     }
    
#     # Passive
#     pnlPassive <- self$data$pnlPassive[i]
#     prev_nop_Passive <- floor(self$data$nopPassive[i - 1])
#     current_nop_Passive <- floor((self$data$eqlPassive[i - 1]) / (self$data$Close[i] / leverage))
#     self$data$eqlPassive[i] <- self$data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
#     self$data$nopPassive[i] <- current_nop_Passive
#   }

#   self$data <- self$data %>%
#     mutate(r_eqlActive = quantmod::Delt(eqlActive),
#             r_eqlPassive = quantmod::Delt(eqlPassive))
#             #%>% na.omit()

#   # Performance metrics for active strategy
#   aR_active <- round(as.numeric(Return.annualized(as.numeric(self$data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
#   aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(self$data$r_eqlActive), scale = 252) * 100), 3)
#   IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
#   MD_active <- round(as.numeric(maxDrawdown(as.numeric(self$data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
#   #trades_active <- sum(c(1, ifelse(self$data$signal[-1] * self$data$signal[-length(self$data$signal)] < 0, 1, 0)))
#   trades_active <- sum(diff(self$data$signal) != 0)
#   buy_active <- sum(self$data$signal == 1)
#   short_active <- sum(self$data$signal == -1)

#   # Performance metrics for passive strategy
#   aR_passive <- round(as.numeric(Return.annualized(as.numeric(self$data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
#   aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(self$data$r_eqlPassive), scale = 252) * 100), 3)
#   IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
#   MD_passive <- round(as.numeric(maxDrawdown(as.numeric(self$data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
#   trades_passive <- 1
#   buy_passive <- 1
#   short_passive <- 0

#   # Calculate success rates
#   buy_success_rate_active <- sum(self$data$position > 0 & self$data$value > 0) / nrow(self$data)
#   buy_success_rate_passive <- sum(self$data$value > 0) / nrow(self$data)

#   short_success_rate_active <- sum(self$data$position < 0 & self$data$value < 0) / nrow(self$data)
#   short_success_rate_passive <- 0

#   combined_rate_active <- (sum(self$data$position > 0 & self$data$value > 0) + sum(self$data$position < 0 & self$data$value < 0)) / nrow(self$data)
#   combined_rate_passive <- (sum(self$data$value > 0)) / nrow(self$data)

#   # Unique months
#   unique_months <- length(unique(format(self$data$Date, "%Y-%m")))

#   # Create performance dataframe
#   df <- data.frame(
#     from = From, # start date given historical data spit
#     to = To, # end date given historical data spit
#     Strategy = c("Active", "Passive"),
#     aR = c(aR_active, aR_passive),
#     aSD = c(aSD_active, aSD_passive),
#     IR = c(IR_active, IR_passive),
#     MD = c(MD_active, MD_passive),
#     trades = c(trades_active, trades_passive),
#     avg_no_monthly_trades = round(c(trades_active / unique_months, 0), 2),
#     buys = c(buy_active, buy_passive),
#     sells = c(short_active, short_passive),
#     Buy_Success_Rate = round(c(buy_success_rate_active, buy_success_rate_passive), 4),
#     Short_Success_Rate = round(c(short_success_rate_active, short_success_rate_passive), 4),
#     Combined_Success_Rate = round(c(combined_rate_active, combined_rate_passive), 4),
#     PortfolioEndValue = round(c(self$data[nrow(self$data),]$eqlActive, self$data[nrow(self$data),]$eqlPassive), 0)
#   )

#   # Print the performance data frame and success rate data frame
#   # print(df)
#   # return(self$data)

#   # print(self$data)
#   return(df)
# },

estimate_performance = function() {
    # Generate signals
    self$generate_signals()
    
    # Ensure self$data is not empty
    if (nrow(self$data) == 0) {
        stop("No data available for performance estimation.")
    }
    
    # Initialize metrics lists
    performance_metrics <- list()

    # Unique periods
    unique_periods <- unique(self$data %>% select(From, To))

    # Loop through each period
    for (i in 1:nrow(unique_periods)) {
        period_data <- self$data %>%
            filter(Date >= unique_periods$From[i] & Date <= unique_periods$To[i])

        if (nrow(period_data) == 0) next

        # Calculate metrics for the period
        period_data <- period_data %>%
            mutate(
                pnlActive = c(0, diff(Close) * signal[-length(Close)]),
                pnlPassive = c(0, diff(Close)),
                nopActive = 0,
                nopPassive = 0,
                eqlActive = 0,
                eqlPassive = 0
            )
        
        # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
        period_data$eqlActive[1] <- capital
        period_data$nopActive[1] <- floor(capital / (period_data$Close[1] / leverage))
        period_data$eqlPassive[1] <- capital
        period_data$nopPassive[1] <- floor(capital / (period_data$Close[1] / leverage))

        has_L <- "nop_sizing" %in% names(period_data)
        has_Vol <- "vol_nop_sizing" %in% names(period_data)

        for (j in 2:nrow(period_data)) {
            # Active
            pnlActive <- period_data$pnlActive[j]
            prev_nop_Active <- floor(period_data$nopActive[j - 1])
            current_nop_Active <- floor((period_data$eqlActive[j - 1]) / (period_data$Close[j] / leverage))
            period_data$eqlActive[j] <- period_data$eqlActive[j - 1] + prev_nop_Active * pnlActive
            
            if (has_L) {
                period_data$nopActive[j] <- ifelse(
                    period_data$L[j], 
                    current_nop_Active * period_data$nop_sizing[j], 
                    current_nop_Active
                )
            } else if (has_Vol) {
                period_data$nopActive[j] <- ifelse(
                    period_data$vol_nop_sizing[j], 
                    current_nop_Active * period_data$vol_nop_sizing[j], 
                    current_nop_Active
                )
            } else {
                period_data$nopActive[j] <- current_nop_Active
            }
            
            # Passive
            pnlPassive <- period_data$pnlPassive[j]
            prev_nop_Passive <- floor(period_data$nopPassive[j - 1])
            current_nop_Passive <- floor((period_data$eqlPassive[j - 1]) / (period_data$Close[j] / leverage))
            period_data$eqlPassive[j] <- period_data$eqlPassive[j - 1] + prev_nop_Passive * pnlPassive
            period_data$nopPassive[j] <- current_nop_Passive
        }

        period_data <- period_data %>%
            mutate(
                r_eqlActive = quantmod::Delt(eqlActive),
                r_eqlPassive = quantmod::Delt(eqlPassive)
            )
        
        # Performance metrics
        aR_active <- round(as.numeric(Return.annualized(as.numeric(period_data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
        aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(period_data$r_eqlActive), scale = 252) * 100), 3)
        IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
        MD_active <- round(as.numeric(maxDrawdown(as.numeric(period_data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
        trades_active <- sum(diff(period_data$signal) != 0)
        buy_active <- sum(period_data$signal == 1)
        short_active <- sum(period_data$signal == -1)

        aR_passive <- round(as.numeric(Return.annualized(as.numeric(period_data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
        aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(period_data$r_eqlPassive), scale = 252) * 100), 3)
        IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
        MD_passive <- round(as.numeric(maxDrawdown(as.numeric(period_data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
        trades_passive <- 1
        buy_passive <- 1
        short_passive <- 0

        # Success rates
        buy_success_rate_active <- sum(period_data$position > 0 & period_data$value > 0) / nrow(period_data)
        buy_success_rate_passive <- sum(period_data$value > 0) / nrow(period_data)

        short_success_rate_active <- sum(period_data$position < 0 & period_data$value < 0) / nrow(period_data)
        short_success_rate_passive <- 0

        combined_rate_active <- (sum(period_data$position > 0 & period_data$value > 0) + sum(period_data$position < 0 & period_data$value < 0)) / nrow(period_data)
        combined_rate_passive <- (sum(period_data$value > 0)) / nrow(period_data)

        unique_months <- length(unique(format(period_data$Date, "%Y-%m")))

        performance_metrics[[i]] <- data.frame(
            From = unique_periods$From[i],
            To = unique_periods$To[i],
            Strategy = c("Active", "Passive"),
            aR = c(aR_active, aR_passive),
            aSD = c(aSD_active, aSD_passive),
            IR = c(IR_active, IR_passive),
            MD = c(MD_active, MD_passive),
            trades = c(trades_active, trades_passive),
            avg_no_monthly_trades = round(c(trades_active / unique_months, 0), 2),
            buys = c(buy_active, buy_passive),
            sells = c(short_active, short_passive),
            Buy_Success_Rate = round(c(buy_success_rate_active, buy_success_rate_passive), 4),
            Short_Success_Rate = round(c(short_success_rate_active, short_success_rate_passive), 4),
            Combined_Success_Rate = round(c(combined_rate_active, combined_rate_passive), 4),
            PortfolioEndValue = round(c(period_data[nrow(period_data),]$eqlActive, period_data[nrow(period_data),]$eqlPassive), 0)
        )
    }

    # Combine all performance metrics into one data frame
    final_performance_df <- bind_rows(performance_metrics)
    return(final_performance_df)
},

# Calculate cumulative return (method to be removed)
calculate_cumulative_return = function() {
      #self$generate_signals()  # Call generate_signals dynamically
      cret = data.frame(
        Active = prod(1+self$data$value * self$data$position) - 1,
        Buy_and_hold = prod(1+self$data$value) - 1
      )
      return(cret)
},

split_data_into_periods = function() {
  
  # Create a sequence of start dates for each 2-year period
  period_starts <- seq(from_date, to_date, by = "2 years")
  period_ends <- c(period_starts[-1] - 1, to_date)  # End dates are one day before the next start

  # Split the data into a list of dataframes, each corresponding to a 2-year period
  period_data <- lapply(seq_along(period_starts), function(i) {
    period_df <- self$data %>%
      filter(Date >= period_starts[i] & Date <= period_ends[i]) %>%
      mutate(From = period_starts[i],   # Add start date column
             To = period_ends[i])      # Add end date column
    return(period_df)
  })

  return(period_data)
},

estimate_performance_for_periods = function(data_list) {
  results <- lapply(data_list, function(period_data) {
    self$data <- period_data  # Assuming `self$data` is the dataframe used in `estimate_performance`
    self$estimate_performance()  # Call the method via `self$`
  })

  # Combine all results into one dataframe
  combined_results <- do.call(rbind, results)

  # Return the combined results
  return(combined_results)
},

# Visualize equity lines for active strategy and passive (buy and hold)
plot_equity_lines = function(strategy_name, signal_flag = FALSE) {
  # Line size
  active_line_size <- ifelse(signal_flag, 1, 0.8)
  passive_line_size <- ifelse(signal_flag, 1, 0.8)
  
  # Plot equity lines
  p <- ggplot(self$data, aes(x = Date)) +
    labs(title = paste0("Equity Lines for Active ", "(", as.character(strategy_name), ")", " and Passive (buy-and-hold) strategies"),
         x = "Date",
         y = "Equity line") +
    theme_minimal()

  # Add signals if signal_flag is TRUE
  if (signal_flag) {
    p <- p +
      geom_vline(data = self$data[self$data$signal == -1, ], aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$signal == 1, ], aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "green", alpha = 0.5) +
      scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "green"))
  }

  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")

  # Print or return the plot
  if (signal_flag) {
    print(p)
  } else {
    return(p)
  }
},

# Plot Japanese candles for the period of latest ndays
plot_candles = function(ndays) {
  self$data <- tail(self$data, ndays)
  fig <- self$data %>% plot_ly(x = ~Date, type= "candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) 
  fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
  fig
},

# Estimate Average True Range (ATR)
estimate_average_true_range = function(ts, n = 14) { # 14 days by default
  atr <- ATR(HLC(ts), n)
  atr_data <- data.frame(Date = index(atr), coredata(atr))
  ts_data <- data.frame(Date = index(ts), coredata(ts))
  combined_data <- merge(ts_data, atr_data, by = "Date") %>%
    #mutate(tr_reserve = 1 - tr/atr * 100)
    mutate(tr_reserve = tr/atr * 100)
  return(combined_data)
}

# Money management rules (risk per trade, position sizing, diversification, profit taking, risk tolerance level) are to be added

  )
)

Nero <- R6Class(
  "Nero",
  inherit = Strategy,
  public = list(
    
    initialize = function(data) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
    },
    
    # identify Close price's global min, global max, local min1, local max1, ...
    identify_historical_levels = function() {
      self$data <- self$data %>%
        mutate(
          # max
          global_max = cummax(Close),
          local_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
          local_half_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
          local_month_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
          local_week_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE), # week
          
          # min
          local_three_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/4, .complete = TRUE), # month
          local_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
          local_week_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE) # week
        )
      #return(self$data)
},
    
    # identify strong price levels (refer to Chapter 2)
    # level, points from which new min or max is reached
    
    # Criteria to support identified strong price level
    # Entry point, stop value, luft, stop loss, take profit points
    
    # generate_signals = function() {
    
    #     # Split data into periods
    #     period_list <- super$split_data_into_periods()
        
    #     # Apply signal generation logic to each period
    #     period_list <- lapply(period_list, function(df) {
    #         df <- df %>%
    #             mutate(
    #                 # Generate signals: 1 for Buy, -1 for Sell, 0 for Hold
    #                 signal = ifelse(Close > lag(Close), 1, ifelse(Close < lag(Close), -1, 0)),
                    
    #                 # Lag the signal to determine position, avoiding using today's signal for today's position
    #                 position = lag(signal, default = 0)
    #             ) %>%
    #             na.omit()  # Remove any rows with NA values that may result from the lag function
    #         return(df)
    #     })
        
    #     # Combine all period data frames into one data frame
    #     self$data <- bind_rows(period_list)
      
    #   # Three main strategies (nearby strong price levels)
    #   # *identify strong price level, market is in range in ~ 3/4 time
    #   # Choose one of the following and trade
    #   # 1. Bounce off the level
    #   # 2. False breakout (simple, 2 bars, complex: 3 or more bars)
    #   # 3. Breakout the level (spring)
      
    #   #self$identify_historical_levels() # add historical levels to plot with candles
    #   #self$identify_reversals(50)
    # },

generate_signals = function() {
    # Split data into periods
    period_list <- super$split_data_into_periods()
    
    # Apply signal generation logic to each period
    period_list <- lapply(period_list, function(df) {
        df <- df %>%
            mutate(
                # Generate signals: 1 for Buy, -1 for Sell, 0 for Hold
                signal = ifelse(Close > lag(Close), 1, ifelse(Close < lag(Close), -1, 0)),
                
                # Lag the signal to determine position, avoiding using today's signal for today's position
                position = lag(signal, default = 0)
            ) %>%
            na.omit()  # Remove any rows with NA values that may result from the lag function
        return(df)
    })
    
    # Combine all period data frames into one data frame
    self$data <- bind_rows(period_list)
},

    # Estimate strategy performance
    # estimate_performance = function() {
    #   # 3R/R
    #   # Money management: position sizing, cascade/de-cascade
    #   # Risk management rules: 1% portfolio daily loss max, etc.
    #   # sequence of 3 losing trades - pause activity
    # },
    
    # Identify levels based on reversal points given rolling window
    identify_reversals = function(window ) {
      # Calculate the rolling minimum
      self$data$RollingMin <- rollapply(self$data$Close, width = window, FUN = min, by.column = TRUE, fill = NA, align = "right")
      
      # Initialize columns
      self$data$strong_min <- NA
      self$data$strong_max <- NA
      
      # Variables to track current minimum and current high
      current_min <- NA
      current_high <- -Inf
      
      # Iterate through each row in the data frame
      for (i in 1:nrow(self$data)) {
        if (!is.na(self$data$RollingMin[i])) {
          # Check if a new rolling window starts (every `window` days)
          if (i %% window == 1) {
            current_min <- self$data$RollingMin[i]  # Reset current_min
            current_high <- self$data$Close[i]      # Reset current_high
            self$data$strong_min[i] <- current_min
          } else {
            # If we encounter a new minimum within the window
            if (is.na(current_min) || self$data$RollingMin[i] < current_min) {
              current_min <- self$data$RollingMin[i]
              current_high <- self$data$Close[i]    # Reset current high
              self$data$strong_min[i] <- current_min
            }
            
            # If we encounter a new high within the window
            if (self$data$Close[i] > current_high) {
              current_high <- self$data$Close[i]
              if (!is.na(current_min)) {
                # Record the current minimum and maximum as strong
                self$data$strong_min[i] <- current_min
                self$data$strong_max[i] <- current_high
              }
            }
          }
        }
      }
      
      # Filter out rows with NA in strong_max
      data_filtered <- self$data[!is.na(self$data$strong_max), ]
      
      # Calculate strongest values
      strongest_values <- data_filtered %>%
        group_by(strong_min) %>%
        mutate(strongest = ifelse(strong_max == max(strong_max, na.rm = TRUE), max(strong_max, na.rm = TRUE), NA)) %>%
        ungroup()
      
      # Create levels data frame
      levels <- strongest_values %>%
        filter(!is.na(strongest)) %>%
        mutate(plot = TRUE)
      
      # Merge strongest column back to original data by Date
      # self$data <- merge(self$data, levels[, c("Date", "strongest", "plot")], by = "Date", all.x = TRUE)
      
      self$data <- self$data %>% left_join(levels[, c("Date", "strongest", "plot")], by = "Date")
      #self$data <- merge(self$data, levels[, c("Date", "strongest", "plot")], by = "Date", all.x = TRUE, suffixes = c("", ""))
      
      # Return self$data for chaining or other purposes
      #return(self$data)
    },
    
    # Plot levels based on reversal points
    plot_reversals_lvl = function(from, to) {
      # Plotting using ggplot2
      ggplot(self$data[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to), ], aes(x = Date, y = Close)) +
        geom_line(color = "black", size = 1) +  # Close prices
        geom_hline(aes(yintercept = strong_min), data = subset(self$data[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & self$data$plot.x == TRUE, ]), linetype = "dashed", color = "green", size = 0.5) +  # strong_min levels
        geom_hline(aes(yintercept = strong_max), data = subset(self$data[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & self$data$plot.x == TRUE, ]), linetype = "longdash", color = "red", size = 0.5) +  # strong_max levels
        annotate("text", x = self$data$Date[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_min) & self$data$plot.x == TRUE], y = self$data$strong_min[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_min) & self$data$plot.x == TRUE], label = "strong_min", vjust = -0.5, color = "green", size = 3) +  # Annotation for strong_min
        annotate("text", x = self$data$Date[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_max) & self$data$plot.x == TRUE], y = self$data$strong_max[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_max) & self$data$plot.x == TRUE], label = "strong_max", vjust = 1, color = "red", size = 3) +  # Annotation for strong_max
        labs(
          title = "Close Prices with strong_min and strong_max Levels",
          x = "Date",
          y = "Price"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    },
    
    # Plot Japanese candles (HLOC) given latest ndays
    plot_candles = function(ndays) {
      
      # Get the last ndays of data
      df_tail <- tail(self$data, ndays)
      
      # Latest date in the tail data
      latest_date <- max(df_tail$Date)
      
      # Get the latest levels for the horizontal lines
      latest_levels <- self$data %>%
        filter(Date == latest_date) %>%
        select(local_year_max, local_half_year_max, local_month_max, local_week_max,
               local_three_month_min, local_month_min, local_week_min) %>%
        slice(1)
      
      # Plot the candlestick chart
      fig <- df_tail %>% plot_ly(x = ~Date, type= "candlestick",
                                 open = ~Open, close = ~Close,
                                 high = ~High, low = ~Low) 
      
      # Add horizontal lines for historical levels
      fig <- fig %>% 
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_year_max, nrow(df_tail)),
                  line = list(color = 'blue', dash = 'dash', width = 1), name = 'Year Max') %>%
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_half_year_max, nrow(df_tail)),
                  line = list(color = 'green', dash = 'dash', width = 1), name = 'Half Year Max') %>%
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_max, nrow(df_tail)),
                  line = list(color = 'red', dash = 'dash', width = 1), name = 'Month Max') %>%
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_max, nrow(df_tail)),
                  line = list(color = 'purple', dash = 'dash', width = 1), name = 'Week Max') %>%
        
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_three_month_min, nrow(df_tail)),
                  line = list(color = 'brown', dash = 'dot', width = 1), name = '3 Month Min') %>%
        
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_min, nrow(df_tail)),
                  line = list(color = 'red', dash = 'dot', width = 1), name = 'Month Min') %>%
        add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_min, nrow(df_tail)),
                  line = list(color = 'purple', dash = 'dot', width = 1), name = 'Week Min')
      
      # Add layout title
      fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
      
      # Return the plot
      fig
    }
    
  )
)

# Instances of Nero strategy
nero <- Nero$new(ts)
#nero$data

c <- nero$generate_signals()
c
#nero$estimate_performance()
#sma1$estimate_performance()
#sma1$data

#a <- nero$generate_signals()
nero$identify_historical_levels()
nero$identify_reversals(window = 50)
a <- nero$identify_reversals(window = 50)
nero$plot_reversals_lvl("2023-01-01", "2024-06-20")

b <- nero$generate_signals()

#df_split <- nero$split_data_into_periods()
per_res <- nero$estimate_performance_for_periods(df_split) %>% filter(Strategy == "Active")
sum(per_res$aR)
per_res <- nero$estimate_performance()

d <- df_split[[1]]

nero$generate_signals()


a$plot.x == a$plot.y

na.omit(a$plot.x)

nero$plot_candles(30)
nero$plot_candles(60)
nero$plot_candles(90) # optimal
nero$plot_candles(120)

atr_btc <- nero$estimate_average_true_range(ts, 14) %>% 
  tail(90)

mean(atr_btc$tr_reserve)

# Strong levels identification

identify_events = function(data, threshold) {
  # Initialize event vector
  data <- data %>%
    mutate(
      mid = (High + Open) / 2
    )
  
  events <- numeric(nrow(data))
  
  # Initialize highest high and lowest low
  highest_high <- 0
  lowest_low <- Inf
  
  # Initialize event flag
  event_flag <- 1  # 1: Event start, -1: Event end
  events[1] <- 1  # Set the first value to 1 as the starting point
  
  # Initialize column for combined overshoot events
  data$OS <- ""
  
  # Initialize columns for highest_high and lowest_low
  data$highest_high <- NA
  data$lowest_low <- NA
  
  # Loop through each row of the data
  for (i in 1:nrow(data)) {
    # Check if event flag is 1
    if (event_flag == 1) {
      # Check condition for Event = 1 (Upturn)
      if (data$High[i] > highest_high) {
        highest_high <- data$High[i]
      }
      if (data$mid[i] <= highest_high * (1 - threshold)) {
        events[i] <- -1
        event_flag <- -1
        #lowest_low <- data$Low[i]
        highest_high <- data$High[i]  # Reset highest_high
        lowest_low <- Inf  # Reset lowest_low to infinity
      }
    } else {
      # Check condition for Event = -1 (Downturn)
      if (data$Low[i] < lowest_low) {
        lowest_low <- data$Low[i]
      }
      if (data$mid[i] >= lowest_low * (1 + threshold)) {
        events[i] <- 1
        event_flag <- 1
        #highest_high <- data$High[i]
        lowest_low <- data$Low[i]  # Reset lowest_low
        highest_high <- Inf
      }
    }
    
    # Update highest_high and lowest_low in the dataframe
    data$highest_high[i] <- highest_high
    data$lowest_low[i] <- lowest_low
  }
  
  # Initialize current state
  current_state <- NA
  
  # Assign OS values
  for (i in seq_along(events)) {
    if (events[i] == 1) {
      current_state <- "UOS"
    } else if (events[i] == -1) {
      current_state <- "DOS"
    }
    if (!is.na(current_state)) {
      data$OS[i] <- current_state
    }
    if (is.na(data$OS[i]) && !is.na(current_state) && i > 1) {
      data$OS[i] <- data$OS[i - 1]
    }
  }
  
  # Lag OS column
  data$OS <- lag(data$OS)
  
  # Return dataframe with events column
  result <- data.frame(data, events = events)
  
  # Calculate dc column
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  
  # Set default values
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)
}
head(ts_df)

# timeframe 1 year
identify_events = function(data, threshold) {
  # Initialize event vector
  data <- data %>%
    mutate(mid = (High + Open) / 2)
  
  events <- numeric(nrow(data))
  
  # Initialize highest high and lowest low
  highest_high <- 0
  lowest_low <- Inf
  
  # Initialize event flag
  event_flag <- 1  # 1: Event start, -1: Event end
  events[1] <- 1  # Set the first value to 1 as the starting point
  
  # Initialize column for combined overshoot events
  data$OS <- ""
  
  # Initialize columns for highest_high and lowest_low
  data$highest_high <- NA
  data$lowest_low <- NA
  
  # Initialize columns for event_price and strong_lvl
  data$event_price <- NA
  data$strong_lvl <- FALSE
  
  # Loop through each row of the data
  for (i in 1:nrow(data)) {
    # Check if event flag is 1
    if (event_flag == 1) {
      # Check condition for Event = 1 (Upturn)
      if (data$High[i] > highest_high) {
        highest_high <- data$High[i]
      }
      if (data$mid[i] <= highest_high * (1 - threshold)) {
        events[i] <- -1
        event_flag <- -1
        highest_high <- data$High[i]  # Reset highest_high
        lowest_low <- Inf  # Reset lowest_low to infinity
        data$event_price[i] <- data$Close[i]
      }
    } else {
      # Check condition for Event = -1 (Downturn)
      if (data$Low[i] < lowest_low) {
        lowest_low <- data$Low[i]
      }
      if (data$mid[i] >= lowest_low * (1 + threshold)) {
        events[i] <- 1
        event_flag <- 1
        lowest_low <- data$Low[i]  # Reset lowest_low
        highest_high <- Inf
        data$event_price[i] <- data$Close[i]
      }
    }
    
    # Update highest_high and lowest_low in the dataframe
    data$highest_high[i] <- highest_high
    data$lowest_low[i] <- lowest_low
  }
  
  # Initialize current state
  current_state <- NA
  
  # Assign OS values
  for (i in seq_along(events)) {
    if (events[i] == 1) {
      current_state <- "UOS"
    } else if (events[i] == -1) {
      current_state <- "DOS"
    }
    if (!is.na(current_state)) {
      data$OS[i] <- current_state
    }
    if (is.na(data$OS[i]) && !is.na(current_state) && i > 1) {
      data$OS[i] <- data$OS[i - 1]
    }
  }
  
  # Lag OS column
  data$OS <- lag(data$OS)
  
  # Track strong level price
  for (i in 1:nrow(data)) {
    if (!is.na(data$event_price[i])) {
      event_start <- events[i]
      event_price <- data$event_price[i]
      for (j in i:nrow(data)) {
        if (event_start == 1) { # Upturn
          if (data$High[j] > data$High[i]) {
            break
          }
          if (data$Close[j] >= event_price) {
            data$strong_lvl[i] <- TRUE
            break
          }
        } else { # Downturn
          if (data$Low[j] < data$Low[i]) {
            break
          }
          if (data$Close[j] <= event_price) {
            data$strong_lvl[i] <- TRUE
            break
          }
        }
      }
    }
  }
  
  # Return dataframe with events column
  result <- data.frame(data, events = events)
  
  # Calculate dc column
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  
  # Set default values
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)
}

a <- identify_events(ts_df[ts_df$Date >= as.Date("2020-01-01"),], 0.075)
table(a$events)

############### Trend reversals levels ###################

identify_reversals <- function(data) {
  # Calculate the rolling minimum (100-day window)
  data$RollingMin <- rollapply(data$Close, width = 100, FUN = min, by.column = TRUE, fill = NA, align = "right")
  
  # Initialize columns
  data$strong_min <- NA
  data$strong_max <- NA
  
  # Variables to track current minimum and current high
  current_min <- NA
  current_high <- -Inf
  
  # Iterate through each row in the data frame
  for (i in 1:nrow(data)) {
    if (!is.na(data$RollingMin[i])) {
      # Check if a new rolling window starts (every 100 days)
      if (i %% 100 == 1) {
        current_min <- data$RollingMin[i]  # Reset current_min
        current_high <- data$Close[i]      # Reset current_high
        data$strong_min[i] <- current_min
      } else {
        # If we encounter a new minimum within the window
        if (is.na(current_min) || data$RollingMin[i] < current_min) {
          current_min <- data$RollingMin[i]
          current_high <- data$Close[i]    # Reset current high
          data$strong_min[i] <- current_min
        }
        
        # If we encounter a new high within the window
        if (data$Close[i] > current_high) {
          current_high <- data$Close[i]
          if (!is.na(current_min)) {
            # Record the current minimum and maximum as strong
            data$strong_min[i] <- current_min
            data$strong_max[i] <- current_high
          }
        }
      }
    }
  }
  
  
  data_filtered <- data[!is.na(data$strong_max), ]
  
  strongest_values <- data_filtered %>%
    group_by(strong_min) %>%
    mutate(strongest = ifelse(strong_max == max(strong_max, na.rm = TRUE), max(strong_max, na.rm = TRUE), NA)) %>%
    ungroup()
  
  levels <- strongest_values %>%
    filter(!strongest == 0) %>%
    mutate(plot = T)
  
  # Merge strongest column back to original data by Date
  merged_data <- merge(data, levels[, c("Date", "strongest", "plot")], by = "Date", all.x = TRUE)
  
  # Return the merged data
  return(merged_data)
  
}

a <- identify_reversals(ts_df, 100)
table(a$strongest)

plot_reversals_lvl <- function(merged_data) {
  # Plotting using ggplot2
  ggplot(merged_data, aes(x = Date, y = Close)) +
    geom_line(color = "black", size = 1) +  # Close prices
    # geom_hline(aes(yintercept = strong_min), data = subset(merged_data, !is.na(strong_min)), linetype = "dashed", color = "green", size = 0.5) +  # strong_min levels
    # geom_hline(aes(yintercept = strong_max), data = subset(merged_data, !is.na(strong_max)), linetype = "longdash", color = "red", size = 0.5) +  # strong_max levels
    
    geom_hline(aes(yintercept = strong_min), data = subset(merged_data, plot == TRUE), linetype = "dashed", color = "green", size = 0.5) +  # strong_min levels
    geom_hline(aes(yintercept = strong_max), data = subset(merged_data, plot == TRUE), linetype = "longdash", color = "red", size = 0.5) +  # strong_max levels
    
    annotate("text", x = merged_data$Date[!is.na(merged_data$strong_min) & merged_data$plot == TRUE], y = merged_data$strong_min[!is.na(merged_data$strong_min) & merged_data$plot == TRUE], label = "strong_min", vjust = -0.5, color = "green", size = 3) +  # Annotation for strong_min
    annotate("text", x = merged_data$Date[!is.na(merged_data$strong_max) & merged_data$plot == TRUE], y = merged_data$strong_max[!is.na(merged_data$strong_max) & merged_data$plot == TRUE], label = "strong_max", vjust = 1, color = "red", size = 3) +  # Annotation for strong_max
    labs(
      title = "Close Prices with strong_min and strong_max Levels",
      x = "Date",
      y = "Price"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_reversals_lvl(a[a$Date >= as.Date("2023-01-01") & a$Date <= Sys.Date(),])

b <- a[a$Date >= as.Date("2023-01-01") & a$Date <= Sys.Date(),]

lvl <- b[!is.na(b$strongest),]
lvl

############### Mirror levels ###################
