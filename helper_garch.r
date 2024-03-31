# Helper functions

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

# Calculate equity lines, number of positions
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
    data$eqlPassive[i] <- data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive - TC * abs(prev_nop_Passive - current_nop_Passive)
    data$nopPassive[i] <- current_nop_Passive
  }
  r_eql <- data %>%
    mutate(r_eqlGARCH = quantmod::Delt(eqlGARCH),
           r_eqlPassive = quantmod::Delt(eqlPassive)) %>%
            na.omit
  return(r_eql)
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