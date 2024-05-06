data_fetcher <- DataFetcher$new("MXN=X", from_date, to_date)
ts <- data_fetcher$download_xts_data()

ts_df <- data.frame(ts)
ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything()) %>%
            na.omit() %>% 
                as_tibble()


# STEP1 : DETECT DIRECTIONAL CHANGES

identify_events <- function(data, threshold) {
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
        lowest_low <- data$Low[i]
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
        highest_high <- data$High[i]
        lowest_low <- data$Low[i]  # Reset lowest_low
      }
    }
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
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)

}

# adding extremes to df
identify_events <- function(data, threshold) {
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

# prototype
prot <- identify_events(ts_df, 0.01)

table(prot$dc)

# STEP2 : INTRODUCE PROBABILITY INDICATOR FOR POSITION SIZING

# Function to compute transition probabilities and surprises
estimate_prob_surprise <- function(data) {
  # Step 1: Identify transitions in the price trajectory
  transitions <- data %>%
    mutate(state_i = ifelse(dc, "change", "overshoot"),
           state_j = lead(state_i)) %>%
    filter(!is.na(state_j)) %>%
    select(state_i, state_j)
  
  if (nrow(transitions) == 0) {
    return(tibble(probability = NA_real_, surprise = NA_real_))
  }
  
  # Step 2: Calculate transition probabilities
  transition_probs <- transitions %>%
    group_by(state_i, state_j) %>%
    summarise(count = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(probability = count / sum(count))
  
  # Step 3: Compute surprise for each transition
  transition_probs <- transition_probs %>%
    mutate(surprise = -log(probability))
  
  return(transition_probs)
}

# Function to compute entropies
estimate_entropies <- function(data) {
  # Identify transitions
  entropy_transitions <- data %>%
    mutate(state_i = lag(dc),
           state_j = dc) %>%
    filter(!is.na(state_i)) %>%
    select(state_i, state_j)
  
  # Calculate transition probabilities for H(1)
  entropy_transition_probs <- entropy_transitions %>%
    group_by(state_i, state_j) %>%
    summarise(count = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(probability = count / sum(count))
  
  # Compute entropy rate H(1)
  H1 <- -sum(entropy_transition_probs$probability * log(entropy_transition_probs$probability))
  
  # Identify pairs of consecutive transitions for H(2)
  pairs <- entropy_transitions %>%
    mutate(state_i_2 = lag(state_i)) %>%
    filter(!is.na(state_i_2)) %>%
    select(state_i_2, state_i, state_j)
  
  # Calculate joint probabilities for each pair of consecutive transitions
  joint_probs <- pairs %>%
    group_by(state_i_2, state_i, state_j) %>%
    summarise(count = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(probability = count / sum(count))
  
  # Compute the entropy rate H(2)
  H2 <- -sum(joint_probs$probability * log(joint_probs$probability))
  
  # Create dataframe with entropies
  entropy_df <- data.frame(H1 = H1,
                           H2 = H2)
  
  return(entropy_df)
}

# prototype
prot$surprise <- NA_real_
prot$H1 <- NA_real_
prot$H2 <- NA_real_

# Loop over rows to compute probability and surprise for each row
for (i in 1:nrow(prot)) {

    data <- slice(prot, 1:i)  # Subset data up to current row
    probs_surprises <- estimate_prob_surprise(data)  # Compute probabilities and surprises
    entropies <- estimate_entropies(data)

    prot$surprise[i] <- sum(probs_surprises$surprise) # surprise of the transition
    prot$H1[i] <- entropies$H1
    prot$H2[i] <- entropies$H2
}

# Initialize K with 1
prot$K <- 1

# Initialize a vector to store unique combinations
unique_combinations <- c(paste(prot$events[1], prot$j[1]))
prot$j <- lag(prot$events, default = 0)

# Iterate over rows and update K when a new combination is identified
for (i in 2:nrow(prot)) {
  current_combination <- paste(prot$dc[i], prot$j[i])
  if (!(current_combination %in% unique_combinations)) {
    unique_combinations <- c(unique_combinations, current_combination)
  }
  prot$K[i] <- min(length(unique_combinations), 4)
  if (length(unique_combinations) > 4) {
    prot$K[i:nrow(prot)] <- 4
    break
  }
}

#View(prot)

# estimate L
prot <- prot %>%
  mutate(
    surprise = replace_na(surprise, 0),
    d = (surprise - K * H1) / sqrt(K * H2),
    L = 1 - pnorm(d)
  )

##############################
# ENTRY SIGNAL GENERATION BASED ON THRESHOLD (if overshoot moves further the original threshold)
##############################

colnames(prot)

# Identify threshold value (which is used to compare current mid price)

th <- 0.005 # assymetric
# Initialize an empty vector to store mid prices for the change_value
prot$change_value <- NA

# Initialize variables
prev_dc_index <- NULL
prev_mid <- NULL

# Loop through each row
for (i in 1:nrow(prot)) {
  # Check if dc is TRUE
  if (prot$dc[i]) {
    # If this is the first dc or if it's not consecutive with the previous one
    if (is.null(prev_dc_index) || i != prev_dc_index + 1) {
      prev_dc_index <- i  # Update prev_dc_index
      prev_mid <- prot$mid[i]  # Record the mid price
    }
  }
  
  # Assign the previous mid price to the change_value column
  prot$change_value[i] <- prev_mid
  
  # Check if the price further changes by the threshold and update prev_mid accordingly
  if (!is.na(prev_mid) && !is.na(prot$OS[i])) {
    if (prot$OS[i] == "UOS" && prot$mid[i] > prev_mid * (1 + th)) {
      prev_mid <- prot$mid[i]  # Update prev_mid for UOS
    } else if (prot$OS[i] == "DOS" && prot$mid[i] < prev_mid * (1 - th)) {
      prev_mid <- prot$mid[i]  # Update prev_mid for DOS
    }
  }
}

prot <- prot %>%
  mutate(
    row_number = row_number(),
    signal = case_when(
      OS == "UOS" & mid >= change_value * (1 + th) ~ -1,
      OS == "DOS" & mid <= change_value * (1 - th) ~ 1,
      TRUE ~ 0
    ),
    position = lag(signal, default = 0),
    OSS = mid >= change_value * (1 + th) | mid <= change_value * (1 - th) 
  ) %>% 
    select(Date, High, Low, Close, mid, change_value, OS, dc, events, L, signal, OSS, position, row_number)

View(prot)

entries <- prot %>% 
        filter(signal == 1 | signal == -1) %>%
            mutate(
                change = c(2, abs(diff(signal))), default = 0,
                Exit = case_when(
                    signal == -1 ~ mid * (1 - th),
                    signal == 1 ~ mid * (1 + th),
                    TRUE ~ 0
                    )
                )

prot <- prot %>%
  mutate(in_entries = row_number() %in% entries$row_number,
  signal = ifelse(in_entries, signal, 0),
  position = lag(signal, default = 0)
  ) %>% 
    left_join(entries %>% select(row_number, Exit), by = c("row_number" = "row_number"))

# Plot Close price
p <- ggplot(prot, aes(x = Date, y = Close)) +
  geom_line() +  # Plot the Close price as a line
  labs(title = "Close Price") +
  theme_minimal()

# Highlight buy and sell signals
p + geom_point(data = subset(prot, signal == 1), aes(x = Date, y = Close), color = "blue", size = 1.5) +  # Highlight buy signals in blue
  geom_point(data = subset(prot, signal == -1), aes(x = Date, y = Close), color = "red", size = 1.5) +  # Highlight sell signals in red
  theme_minimal()

#View(prot) # here Exits are in the prot2

##############################
# ENTRY SIGNAL GENERATION BASED ON OVERSHOOT DURATION (enter when the number of observed OS in a sequence exceeds or equal to the average OS based on the whole dataset)
##############################

# overshoot duration
calculate_sequence_lengths <- function(h) {
  uos_lengths <- numeric()  # Initialize vector to store UOS sequence lengths
  dos_lengths <- numeric()  # Initialize vector to store DOS sequence lengths
  #current_sequence <- NULL  # Initialize variable to store current sequence
  current_sequence <- ""
  
  for (event in h$OS) {
    if (is.null(current_sequence)) {
      current_sequence <- event  # Initialize current sequence if it's NULL
    } else if (current_sequence != event) {
      if (current_sequence == "UOS") {
        #uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
        uos_lengths <- c(uos_lengths, nchar(current_sequence))
      } else if (current_sequence == "DOS") {
        #dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
        dos_lengths <- c(dos_lengths, nchar(current_sequence))  # Store length of DOS sequence
      }
      current_sequence <- event  # Reset current sequence to new event
    } else {
      #current_sequence <- c(current_sequence, event)  # Add event to current sequence
      current_sequence <- paste(current_sequence, event, sep = "")
    }
  }
  
  # Add length of the last sequence
  if (!is.null(current_sequence)) {
    if (current_sequence == "UOS") {
      #uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
      uos_lengths <- c(uos_lengths, nchar(current_sequence))  # Store length of UOS sequence
    } else if (current_sequence == "DOS") {
      #dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
      dos_lengths <- c(dos_lengths, nchar(current_sequence))  # Store length of DOS sequence
    }
  }
  
  return(list(uos_lengths = uos_lengths, dos_lengths = dos_lengths))
}

# Calculate sequence lengths for UOS and DOS
sequence_lengths <- calculate_sequence_lengths(prot)

avgOS <- data.frame(avgUOS = floor(mean(sequence_lengths$uos_lengths)), avgDOS = floor(mean(sequence_lengths$dos_lengths)))

calculate_OS_length <- function(data) {
  # Initialize an empty vector to store OS lengths
  data$OS_length <- 0
  
  # Initialize variables to keep track of previous signal
  prev_signal <- NULL
  prev_os_length <- 0
  
  # Loop through each row
  for (i in 1:nrow(data)) {
    # Check if the signal changes from the previous row
    if (!is.null(prev_signal) && data$OS[i] != prev_signal) {
      # Reset OS length when the signal changes
      prev_os_length <- 0
    }
    
    # Increment OS length if the signal is UOS or DOS
    if (data$OS[i] == "UOS" || data$OS[i] == "DOS") {
      prev_os_length <- prev_os_length + 1
    } else {
      prev_os_length <- 0
    }
    
    # Update OS_length column
    data$OS_length[i] <- prev_os_length
    
    # Update previous signal for the next iteration
    prev_signal <- data$OS[i]
  }
  
  # Calculate signalOS based on OS_length and average UOS/DOS lengths
  data <- data %>% mutate(
    signalOS = ifelse(OS == "UOS" & OS_length >= avgOS$avgUOS, -1, 
                      ifelse(OS == "DOS" & OS_length >= avgOS$avgDOS, 1, 0))
  )
  
  return(data)
}

# Usage:
prot <- calculate_OS_length(prot)
prot <- prot %>%
    mutate(
        ExitOS = case_when(
        signalOS == -1 ~ mid * (1 - th),
        signalOS == 1 ~ mid * (1 + th),
        TRUE ~ 0
        )
    )

subset <- prot %>% head(100)
fwrite(subset, "subset.csv")

############### EXIT SIGNAL GENERATION ###############
############### COASTLINE TRADING $$$$$$$$$$$$$$$$$$$

generateExitSignals <- function(data, signal_generation) {
  # Initialize exit signals column
  data$signalE <- 0
  
  # Initialize exit_value_downward and exit_value_upward
  exit_value_downward <- NA
  exit_value_upward <- NA
  
  # Initialize flags for current run direction
  current_run <- "upward"
  
  # Modifications for further calculations
  data$signal[data$signal == 0] <- NA
  data <- data %>% mutate(signal = na.locf(signal, na.rm = FALSE))
  data$signal <- na.locf(data$signal, na.rm = FALSE, fromLast = TRUE)
  
  # Choose Exit column based on signal_generation
  if (signal_generation == 'OS') {
    data$Exit <- na.locf(data$ExitOS, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  } else {
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  }
  
  # Loop through each row
  for (i in 1:nrow(data)) {
    
    # Check if Exit is not NA
    if (!is.na(data$Exit[i])) {
      
      # Set exit value for the respective directional run
      if (data$signal[i] == -1) {
        exit_value_downward <- data$Exit[i]  # Set exit value for downward run
        current_run <- "upward"  # Update current run direction
      } else if (data$signal[i] == 1) {
        exit_value_upward <- data$Exit[i]  # Set exit value for upward run
        current_run <- "downward"  # Update current run direction
      }
      
      # Check for exit condition during downward run
      if (current_run == "downward" && !is.na(exit_value_downward) && data$mid[i] < exit_value_downward) {
        data$signalE[i] <- 1  # Close sell signal with buy signal during downward run
        cat("Exit condition met for downward run at row:", i, "\n")
      }
      
      # Check for exit condition during upward run
      if (current_run == "upward" && !is.na(exit_value_upward) && data$mid[i] > exit_value_upward) {
        data$signalE[i] <- -1  # Close buy signal with sell signal during upward run
        cat("Exit condition met for upward run at row:", i, "\n")
      }
    } 
  }
  
  # Reset exit values and current run direction after loop
  exit_value_downward <- NA
  exit_value_upward <- NA
  current_run <- "upward"
  
  return(data)
}

generateExitSignals2 <- function(data, signal_generation) {
  # Initialize exit signals column
  data$signalE <- 0
  
  # Initialize exit_value_downward and exit_value_upward
  exit_value_downward <- NA
  exit_value_upward <- NA
  
  # Initialize flags for current run direction
  current_run <- "upward"
  
  # Modifications for further calculations
  data$signal[data$signal == 0] <- NA
  data <- data %>% mutate(signal = na.locf(signal, na.rm = FALSE))
  data$signal <- na.locf(data$signal, na.rm = FALSE, fromLast = TRUE)
  if (signal_generation == 'OS') {
    data$Exit <- na.locf(data$ExitOS, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  } else {
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  }
  
  # Loop through each row
  for (i in 1:nrow(data)) {
    
    # Check if Exit is not NA
    if (!is.na(data$Exit[i])) {
      
      # Set exit value for the respective directional run
      if (data$signal[i] == -1) {
        exit_value_downward <- data$Exit[i]  # Set exit value for downward run
        current_run <- "upward"  # Update current run direction
      } else if (data$signal[i] == 1) {
        exit_value_upward <- data$Exit[i]  # Set exit value for upward run
        current_run <- "downward"  # Update current run direction
      }
      
      # Check for exit condition during downward run
      if (current_run == "downward" && !is.na(exit_value_downward) && data$mid[i] < exit_value_downward) {
        data$signalE[i] <- 1  # Close sell signal with buy signal during downward run
        cat("Exit condition met for downward run at row:", i, "\n")
        # Find the original line corresponding to this exit
        original_line <- which(data$events == -1 & data$OS == "DOS" & data$Exit == exit_value_downward)
        if (length(original_line) > 0) {
          cat("Exit condition corresponds to the original line:", original_line, "\n")
        }
      }
      
      # Check for exit condition during upward run
      if (current_run == "upward" && !is.na(exit_value_upward) && data$mid[i] > exit_value_upward) {
        data$signalE[i] <- -1  # Close buy signal with sell signal during upward run
        cat("Exit condition met for upward run at row:", i, "\n")
        # Find the original line corresponding to this exit
        original_line <- which(data$events == 1 & data$OS == "UOS" & data$Exit == exit_value_upward)
        if (length(original_line) > 0) {
          cat("Exit condition corresponds to the original line:", original_line, "\n")
        }
      }
    } 
  }
  
  # Reset exit values and current run direction after loop
  exit_value_downward <- NA
  exit_value_upward <- NA
  current_run <- "upward"
  
  return(data)
}

generateExitSignals <- function(df, signal_generation = "TH") {
    
  df <- df %>%
    mutate(Exit = case_when(
      signal_generation == 'OS' ~ ExitOS,
      TRUE ~ Exit
    ))
  
  sell_exit_price <- c()
  buy_exit_price <- c()
  
  # Initialize signalE column with NA
  df$signalE <- NA
  
  # Initialize metrics to count removals
  sell_exit_removals <- 0
  buy_exit_removals <- 0
  
  # Initialize columns to store which row signalE closes and where exit prices are saved
  df$signalE_closes_row <- NA
  sell_exit_rows <- list()
  buy_exit_rows <- list()
  
  # Iterate over each row of the dataframe
  for (i in 1:nrow(df)) {
    if (!is.na(df$Exit[i]) && df$signal[i] == 1) {
      sell_exit_price <- c(sell_exit_price, df$Exit[i])
      sell_exit_rows <- c(sell_exit_rows, i)
    } else if (!is.na(df$Exit[i]) && df$signal[i] == -1) {
      buy_exit_price <- c(buy_exit_price, df$Exit[i])
      buy_exit_rows <- c(buy_exit_rows, i)
    }
    
    if (df$events[i] == 1) {
      if (any(df$Close[i] > sell_exit_price)) {
        first_index <- which.max(df$Close[i] > sell_exit_price)  # Get the index of the first TRUE value
        df$signalE[i] <- -1  # Set signalE = -1
        df$signalE_closes_row[i] <- sell_exit_rows[first_index]  # Store the index of the row where exit condition is met
        sell_exit_price <- sell_exit_price[-first_index]  # Remove the exit price at the first_index
        sell_exit_rows <- sell_exit_rows[-first_index]  # Remove the corresponding row index
        sell_exit_removals <- sell_exit_removals + 1  # Increment removal count
      }
    } else if (df$events[i] == -1) {
      if (any(df$Close[i] < buy_exit_price)) {
        first_index <- which.max(df$Close[i] < buy_exit_price)  # Get the index of the first TRUE value
        df$signalE[i] <- 1  # Set signalE = 1
        df$signalE_closes_row[i] <- buy_exit_rows[first_index]  # Store the index of the row where exit condition is met
        buy_exit_price <- buy_exit_price[-first_index]  # Remove the exit price at the first_index
        buy_exit_rows <- buy_exit_rows[-first_index]  # Remove the corresponding row index
        buy_exit_removals <- buy_exit_removals + 1  # Increment removal count
      }
    }
  }
  
  # Replace NA values in signalE with 0
  df$signalE[is.na(df$signalE)] <- 0
  
  # Calculate the ratio of removed entry signals to the total number of entry signals for sell exits
  total_sell_entry_signals <- sum(df$signal == 1)
  ratio_removed_to_total_sell <- sell_exit_removals / total_sell_entry_signals
  
  # Print the ratio for sell exits
  cat("Ratio of removed sell entry signals to total:", ratio_removed_to_total_sell, "\n")
  
  # Calculate the ratio of removed entry signals to the total number of entry signals for buy exits
  total_buy_entry_signals <- sum(df$signal == -1)
  ratio_removed_to_total_buy <- buy_exit_removals / total_buy_entry_signals
  
  # Print the ratio for buy exits
  cat("Ratio of removed buy entry signals to total:", ratio_removed_to_total_buy, "\n")
  
  return(df)
}

#prot <- generateExitSignals(prot, signal_generation = "TH")
prot2 <- generateExitSignals(prot, signal_generation = "TH")
subset <- generateExitSignals(subset, signal_generation = "TH")

exits <- prot2 %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    )

exits <- prot2 %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    ) %>% 
                        filter(change == 2)

#View(exits)


# PLOT ENTRIES (GREEN) AND EXITS (RED)

prot2 <- prot2 %>%
  mutate(
    out_exits = row_number() %in% exits$row_number,
    signal = if_else(in_entries, signal, 0),
    signalE = ifelse(signalE != 0, signalE, 0),
    signalE = if_else(out_exits, signalE, 0),
    signal = if_else(signal != signalE & signalE != 0, signalE, signal),
    #signal = if_else(signal != signalE & signalE != 0, signalE, signal), # signals cancel entry and exit
    position = lag(signal, default = 0)
  ) %>% 
    select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, 
        signal, signalOS, signalE, signalE_closes_row, L, position, in_entries, out_exits, Exit, OS_length)

View(prot) # final table with signals

plotSignals <- function(data) {
  ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_point(data = subset(data, signal == 1), aes(color = "Buy", shape = "Buy"), size = 1.5) +
    geom_point(data = subset(data, signal == -1), aes(color = "Sell", shape = "Sell"), size = 1.5) +
    geom_point(data = subset(data, signalE == 1), aes(color = "Exit Buy", shape = "Exit Buy"), size = 1.5) +
    geom_point(data = subset(data, signalE == -1), aes(color = "Exit Sell", shape = "Exit Sell"), size = 1.5) +
    scale_shape_manual(values = c("Buy" = 1, "Sell" = 2, "Exit Buy" = 3, "Exit Sell" = 4)) +
    scale_color_manual(values = c("Buy" = "green", "Sell" = "red", "Exit Buy" = "darkgreen", "Exit Sell" = "darkred")) +
    labs(title = "Close Price") +
    theme_minimal()
}

# Example usage:
plotSignals(prot2)
