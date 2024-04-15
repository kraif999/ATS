# Helper functions
estimate_realized_volatility <- function(data) {

ohlc <- data %>% 
  data.frame %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      select(-Volume, -Adjusted) %>%
        na.omit  %>%
          as.matrix

# Different realized volatility estimators for returns (TTR)
histVolest <- merge(
  garman <- as.xts(na.omit(TTR::volatility(ohlc, calc = "garman"))) / sqrt(252),
  close <- as.xts(na.omit(TTR::volatility(ohlc[,4], calc = "close"))) / sqrt(252),
  parkinson <- as.xts(na.omit(TTR::volatility(ohlc, calc = "parkinson"))) / sqrt(252),
  rogers.satchell <- as.xts(na.omit(TTR::volatility(ohlc, calc = "rogers.satchell"))) / sqrt(252),
  garman_modified <- as.xts(na.omit(TTR::volatility(ohlc, calc = "gk.yz"))) / sqrt(252),
  yang.zhang <- as.xts(na.omit(TTR::volatility(ohlc, calc = "yang.zhang"))) / sqrt(252)
) %>% 
  as.data.frame %>% 
    rename_with(~ c("garman", "close", "parkinson", "rogers_satchell", "garman_modified", "yang_zhang")) %>%
      mutate(TradeDate = as.Date(rownames(.))) %>%
        select(TradeDate, everything(.)) %>%
          na.omit

return(histVolest)
}

# Extract GARCH forecast errors from a fitted object
extract_forecast_data <- function(forecast) {
  add_business_days <- function(start_date, n_days) {
  end_date <- start_date
  for (i in 1:n_days) {
    end_date <- end_date + 1
    while (weekdays(end_date) %in% c("Saturday", "Sunday")) {
      end_date <- end_date + 1
    }
  }
  return(end_date)
}
date_vector <- c()
# Loop over each column of forecast@forecast$sigmaFor
for (col in colnames(forecast@forecast$sigmaFor)) {
  # Add business days starting from the date in the current column
  dates <- add_business_days(as.Date(col), 1)
  # Append the dates to the date_vector
  date_vector <- c(date_vector, dates)
}

n_row_vector <- c() # number of rolling
# Loop over each column of forecast@forecast$sigmaFor
for (i in 1:ncol(forecast@forecast$sigmaFor)) {
  # Determine the n.roll value based on the column index
  n_row <- ifelse(i == 1, 0, i - 1)
  n_row_vector <- c(n_row_vector, rep(n_row, nrow(forecast@forecast$sigmaFor)))
}

dates <- as.Date(colnames(forecast@forecast$sigmaFor))
forecast_data <- data.frame(
  Date = c(dates[-1], add_business_days(as.Date(tail(dates, 1)), 1)), # given T+1 is next day
  SigmaFor = as.vector(forecast@forecast$sigmaFor),
  n.roll = n_row_vector
  )
return(forecast_data)
}

# Extract refitted GARCH model coefficients given TradeDate
coef_roll_refit <- function(roll, plot = TRUE) {
  coef_df_list <- list()
  
  # Loop through each element in 'roll@model$coef'
  for (i in seq_along(roll@model$coef)) {
    # Extract date and coefficients
    date <- as.Date(roll@model$coef[[i]]$index)
    coef_values <- roll@model$coef[[i]]$coef
    
    # Create a data frame for the current date
    coef_df <- data.frame(
      Date = date,
      t(coef_values)  # Transpose the coefficient matrix to match the desired format
    )
    
    # Append the data frame to the list
    coef_df_list[[i]] <- coef_df
  }
  
  # Combine all data frames into a single data frame
  combined_coef_df <- do.call(rbind, coef_df_list) %>%
    mutate(Date = as.Date(Date),  # Convert 'Date' column to Date object
           rows = rownames(.),
           rows2 = gsub("\\d+", "", rows)) %>%
    filter(rows2 == " Estimate") %>%
    select(-c(rows, rows2))
  
  # Plot each coefficient separately if plot is TRUE
  if (plot) {
    # Plot each coefficient separately
    plots <- lapply(names(combined_coef_df)[-1], function(coef_name) {
      ggplot(combined_coef_df, aes(x = Date, y = !!sym(coef_name))) +
        geom_line() +
        labs(x = "Date", y = paste("Coefficient Value -", coef_name)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        #scale_x_date(date_labels = "%b-%Y")  # Adjust the date format as needed
        scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") 
    })
    
    # Combine plots using grid.arrange (assuming you have the gridExtra package)
    grid.arrange(grobs = plots, ncol = 1)
  }
  
  # Return combined_coef_df
  return(combined_coef_df)
}

# Calculate equity lines, number of positions WITH TRANSACTIONAL COSTS
calculate_eqlGARCH <- function(data, dfl = 0.25, capital = 1000000, TC = 2) {
  data <- transform(data, pnl = c(0, signal[-nrow(data)] * diff(Close)),
                          pnlPassive = c(0, diff(Close)),
                          nop = 0, 
                          eqlGARCH = 0,
                          nopPassive = 0,
                          eqlPassive = 0  # Initialize equity line for Passive strategy
                          )  # Initialize positions for Passive strategy
  
  data$eqlGARCH[1] <- capital
  data$nop[1] <- floor(capital * dfl / data$Close[1])
  data$eqlPassive[1] <- capital  # Initial equity line for Passive strategy
  data$nopPassive[1] <- data$nop[1]  # Initial position for Passive strategy
  
  for (i in 2:nrow(data)) {
    # GARCH
    pnl <- data$pnl[i]
    prev_nop_GARCH <- floor(data$nop[i - 1])
    current_nop_GARCH <- floor((data$eqlGARCH[i - 1] * dfl) / data$Close[i])
    data$eqlGARCH[i] <- data$eqlGARCH[i - 1] + prev_nop_GARCH * pnl - TC * abs(prev_nop_GARCH - current_nop_GARCH)
    data$nop[i] <- current_nop_GARCH
    
    # Passive
    pnlPassive <- data$pnlPassive[i]
    prev_nop_Passive <- floor(data$nopPassive[i - 1])
    current_nop_Passive <- floor((data$eqlPassive[i - 1] * dfl) / data$Close[i])
    # data$eqlPassive[i] <- data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive - TC * abs(prev_nop_Passive - current_nop_Passive) 
    data$eqlPassive[i] <- data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive # remove TC since this is just B&H
    data$nopPassive[i] <- current_nop_Passive
  }
  r_eql <- data %>%
    mutate(r_eqlGARCH = quantmod::Delt(eqlGARCH),
           r_eqlPassive = quantmod::Delt(eqlPassive)) %>%
            na.omit
  return(r_eql)
}

# Calculate equity lines, number of positions WITHOUT TRANSACTIONAL COSTS
calculate_eqlGARCH_no_TC <- function(data, dfl = 0.25, capital = 1000000, TC = 2) {
  data <- transform(data, pnl = c(0, signal[-nrow(data)] * diff(Close)),
                          pnlPassive = c(0, diff(Close)),
                          nop = 0, 
                          eqlGARCH = 0,
                          nopPassive = 0,
                          eqlPassive = 0  # Initialize equity line for Passive strategy
                          )  # Initialize positions for Passive strategy
  
  data$eqlGARCH[1] <- capital
  data$nop[1] <- floor(capital * dfl / data$Close[1])
  data$eqlPassive[1] <- capital  # Initial equity line for Passive strategy
  data$nopPassive[1] <- data$nop[1]  # Initial position for Passive strategy
  
  for (i in 2:nrow(data)) {
    # GARCH
    pnl <- data$pnl[i]
    prev_nop_GARCH <- floor(data$nop[i - 1])
    current_nop_GARCH <- floor((data$eqlGARCH[i - 1] * dfl) / data$Close[i])
    data$eqlGARCH[i] <- data$eqlGARCH[i - 1] + prev_nop_GARCH * pnl
    data$nop[i] <- current_nop_GARCH
    
    # Passive
    pnlPassive <- data$pnlPassive[i]
    prev_nop_Passive <- floor(data$nopPassive[i - 1])
    current_nop_Passive <- floor((data$eqlPassive[i - 1] * dfl) / data$Close[i])
    # data$eqlPassive[i] <- data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive - TC * abs(prev_nop_Passive - current_nop_Passive) 
    data$eqlPassive[i] <- data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive # remove TC since this is just B&H
    data$nopPassive[i] <- current_nop_Passive
  }
  r_eql <- data %>%
    mutate(r_eqlGARCH = quantmod::Delt(eqlGARCH),
           r_eqlPassive = quantmod::Delt(eqlPassive)) %>%
            na.omit
  return(r_eql)
}

generate_combinations <- function(

  RV = "close", # Choose historical (realized volatility) estimator
  entry_signal_function = generate_entry_signals, # signal generation engine
  specification, # GARCH model specification
  n_start, refit_every, refit_window, distribution_model, 
  realized_vol,
  plots_path,
  cl) {

  listgarch <- expand.grid(
    specification = specification,
    n_start = n_start,
    refit_every = refit_every,
    refit_window = refit_window,
    distribution_model = distribution_model,
    realized_vol = realized_vol,
    aR = 0,
    aSD = 0,
    MD = 0,
    IR = 0,
    trades = 0,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    mutate(across(c(aR, aSD, MD, IR, trades), as.numeric))
    colnames(listgarch)[1:6] <- c("specification","window.size", "refit.frequency", "refit.window.type", "distribution.model", "realized.vol.method")
  
################################################################################
# Roll density forecasts (one day ahead)
################################################################################

#registerDoParallel(cl)

# RV <- "close" # which realized volatility estimation is chosen

for (i in 1:dim(listgarch)[1]){
  tryCatch({ 
  
  if(listgarch[i,1] == "fGARCH") {
    spec <- ugarchspec(
        variance.model = list(
        model = listgarch[i,1],
        garchOrder = c(1, 1), 
        submodel = "TGARCH", 
        external.regressors = NULL, 
        variance.targeting = FALSE), 
        
        mean.model = list(
        armaOrder = c(1, 1), 
        include.mean = TRUE, 
        archm = FALSE, 
        archpow = 1, 
        arfima = FALSE, 
        external.regressors = NULL, 
        archex = FALSE),

        distribution.model = listgarch[i,5]) 
    # , start.pars = list(), fixed.pars = list(), ...)
  } else {
    spec <- ugarchspec(
        variance.model = list(
        model = listgarch[i,1], 
        garchOrder = c(1, 1), 
        submodel = NULL, 
        external.regressors = NULL, 
        variance.targeting = FALSE), 

        mean.model = list(
        armaOrder = c(1, 1), 
        include.mean = TRUE, 
        archm = FALSE, 
        archpow = 1, 
        arfima = FALSE, 
        external.regressors = NULL, 
        archex = FALSE),
        
        distribution.model = listgarch[i,5]) 
  }

  if(listgarch[i,4] == "moving") {
    roll = ugarchroll(
        spec, 
        instr_ret[,1], 
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch[i,2],  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch[i,3], # determines every how many periods the model is re-estimated.
        refit.window = listgarch[i,4], # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead).
        window.size = listgarch[i,2],
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = cl,
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
} else {
    roll = ugarchroll(
        spec, 
        instr_ret[,1], 
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch[i,2],  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch[i,3], # determines every how many periods the model is re-estimated.
        refit.window = listgarch[i,4], # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead)
        # window.size = listgarch[i,2],
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = cl,
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
  }

# roll <- resume(roll, solver= "gosolnp") # if object contains non-converged windows

# show(roll) # 20.02 secs

forecastVolRoll <- data.frame(
  Date = roll@model$index[(listgarch$window.size[i]+1):length(roll@model$index)],
  Forecast = roll@forecast$density$Sigma
)

# Join realized volatility estimation and instr log returns given TradeDate
volForHistRoll <- forecastVolRoll %>%
  mutate(TradeDate = Date) %>% 
    left_join(histVolest, by = 'TradeDate') %>%
      na.omit %>% select(-Date) %>%
        select(TradeDate, everything()) %>%
          left_join(select(instr, TradeDate, Close, rets), by = "TradeDate")

# Choose historical volatility estimator to compare with one day ahead rolling forecasts
as.data.frame(apply(volForHistRoll %>% select(Forecast, histVol %>% names), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))

volForHistRoll <- generate_entry_signals(volForHistRoll)

################################################################################
# Strategy performance
################################################################################

performance <- calculate_eqlGARCH_no_TC(volForHistRoll) # contains number of positions, equity lines, return of equity line given initial investment 

# Performance metrics of strategy based on GARCH
aR <- round(as.numeric(Return.annualized(as.numeric(performance$r_eqlGARCH), scale = 252, geometric = TRUE) * 100), 3)
aSD <- round(as.numeric(StdDev.annualized(as.numeric(performance$r_eqlGARCH), scale = 252) * 100), 3)
IR <- round(as.numeric(aR / aSD), 3) 
MD <- round(as.numeric(maxDrawdown(as.numeric(performance$r_eqlGARCH), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
trades <- sum(c(1, ifelse(performance$signal[-1] * performance$signal[-length(performance$signal)] < 0, 1, 0)))

listgarch[i, "aR"] <- aR
listgarch[i, "aSD"] <- aSD
listgarch[i, "MD"] <- MD
listgarch[i, "IR"] <- IR
listgarch[i, "trades"] <- trades

# Passive strategy
passive <- data.frame(
  Buy_and_Hold = as.character(paste0(meta$assets[[symbol]]$description, "_buy_and_hold")),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  stringsAsFactors = FALSE
) %>%
mutate(
  aR = round(as.numeric(Return.annualized(as.numeric(performance$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3),
  aSD = round(as.numeric(StdDev.annualized(as.numeric(performance$r_eqlPassive), scale = 252) * 100), 3),
  IR = round(aR / aSD, 3),
  MD = round(as.numeric(maxDrawdown(as.numeric(performance$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
)

active_passive <- bind_rows(passive %>% rename(Strategy_Specification = Buy_and_Hold), 
          listgarch[i,] %>% rename(Strategy_Specification = specification) %>% select(Strategy_Specification, aR, aSD, MD, IR))

active_passive

print(active_passive)

# Plots
generate_and_save_plot(performance, listgarch, i, plots_path)

rm(roll, passive)

  } ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#   }, error = function(e) {
#      cat("ERROR :",conditionMessage(e), "\n")
#   })
# }

#fwrite(listgarch, "listgarchAll.csv")
  listgarch <- cbind(Symbol = symbol, Class = class, listgarch)
  return(listgarch)
}

generate_and_save_plot <- function(performance, listgarch, i, plots_path) {
  # Wrap the long title text
  wrapped_title <- str_wrap(
    paste0(
    meta$assets[[symbol]]$description, " (", 
    meta$assets[[symbol]]$class, ")",
    ": ", 
    "the comparison of equity lines between GARCH based strategy ", 
    "(specifcation: ", listgarch[i,"specification"], ",",
    " window size: ",  listgarch[i,"window.size"], ",",
    " refit frequency: ", listgarch[i, "refit.frequency"], ",",
    " refit window type: ", listgarch[i, "refit.window.type"], ",",
    " distribution: ", listgarch[i, "distribution.model"], ",",
    " realized vol estimator: ", listgarch[i, "realized.vol.method"], ")",
    " Vs and passive investing (buy and hold)"))

  # Create ggplot with wrapped title and modified legend label
  p <- ggplot(performance, aes(x = TradeDate)) +
    geom_line(aes(y = eqlGARCH, color = "Strategy based on GARCH"), na.rm = TRUE) +
    geom_line(aes(y = eqlPassive, color = "Passive (buy and hold)"), na.rm = TRUE) +
    ggtitle(wrapped_title) +  # Use the wrapped title
    scale_color_manual(values = c("Strategy based on GARCH" = "red", "Passive (buy and hold)" = "blue")) +
    labs(x = "TradeDate", y = "Equity line value", color = "Strategy") +  # Modify legend label
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          plot.title = element_text(size = 12))  # Adjust title font size
  
  # Save the plot with a unique filename
  plot_filename <- paste0(plots_path, "plot_", symbol, "_",
                          listgarch[i,"specification"], "_", 
                          listgarch[i,"window.size"], "_",
                          listgarch[i, "refit.frequency"], "_",
                          listgarch[i, "refit.window.type"], "_",
                          listgarch[i, "distribution.model"], "_",
                          listgarch[i, "realized.vol.method"], ".png")
  
  # Save the plot with a unique filename
  ggsave(plot_filename, p)
}

# Calculate summary statistics for instr
calculate_summary_statistics <- function(data) {
  summary_stats <- data %>%
    summarise(
      mean_return = mean(rets, na.rm = TRUE),
      median_return = median(rets, na.rm = TRUE),
      sd_return = sd(rets, na.rm = TRUE),
      skewness = skewness(rets, na.rm = TRUE),
      kurtosis = kurtosis(rets, na.rm = TRUE)
    ) 
  return(summary_stats)
}

