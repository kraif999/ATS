data_fetcher <- DataFetcher$new("MXN=X", from_date, to_date)
ts <- data_fetcher$download_xts_data()

ts_df <- data.frame(ts)
ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything()) %>%
            na.omit() %>% 
                as_tibble()

ts_df <- ts_df %>%
  mutate(mid = (High + Low) / 2)

from_date <- as.Date("2020-01-01", format = "%Y-%m-%d")
# STEP1 : DETECT DIRECTIONAL CHANGES

identify_events <- function(data, threshold) {
  # Initialize event vector
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

threshold <- 0.01
# prototype
prot <- identify_events(ts_df, 0.01)



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

# Initialize an empty vector to store mid prices for the change_value
prot$change_value <- NA

# Initialize variables
prev_dc_index <- NULL
prev_mid <- NULL


th <- 0.005 # assymetric

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
  if (!is.na(prev_mid) && abs(prot$mid[i] - prev_mid) > th * prev_mid) {
    prev_mid <- prot$mid[i]  # Update prev_mid
  }
}


# add OS type in initializer: signalTh or signalOS

prot2 <- prot %>%
  mutate(
    row_number = row_number(),
    signal = case_when(
      OS == "UOS" & mid >= change_value * (1 + th) ~ -1,
      OS == "DOS" & mid <= change_value * (1 - th) ~ 1,
      TRUE ~ 0
    ),
    position = lag(signal, default = 0),
    OSS = mid >= change_value * (1 + th) | mid <= change_value * (1 - th) 
  ) %>% select(Date, High, Low, Close, mid, change_value, OS, dc, L, signal, OSS, position, row_number)

entries <- prot2 %>% 
        filter(signal == 1 | signal == -1) %>%
            mutate(
                change = c(2, abs(diff(signal))), default = 0,
                Exit = case_when(
                    signal == -1 ~ mid * (1 - th),
                    signal == 1 ~ mid * (1 + th),
                    TRUE ~ 0
                    )
                )

prot2 <- prot2 %>%
  mutate(in_entries = row_number() %in% entries$row_number,
  signal = ifelse(in_entries, signal, 0),
  position = lag(signal, default = 0)
  ) %>% 
    left_join(entries %>% select(row_number, Exit), by = c("row_number" = "row_number"))


#View(prot2) # here Exits are in the prot2

##############################
# ENTRY SIGNAL GENERATION BASED ON OVERSHOOT DURATION (enter when the number of observed OS in a sequence exceeds or equal to the average OS based on the whole dataset)
##############################

# overshoot duration
calculate_sequence_lengths <- function(h) {
  uos_lengths <- numeric()  # Initialize vector to store UOS sequence lengths
  dos_lengths <- numeric()  # Initialize vector to store DOS sequence lengths
  current_sequence <- NULL  # Initialize variable to store current sequence
  
  for (event in h$OS) {
    if (is.null(current_sequence)) {
      current_sequence <- event  # Initialize current sequence if it's NULL
    } else if (current_sequence != event) {
      if (current_sequence == "UOS") {
        uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
      } else if (current_sequence == "DOS") {
        dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
      }
      current_sequence <- event  # Reset current sequence to new event
    } else {
      current_sequence <- c(current_sequence, event)  # Add event to current sequence
    }
  }
  
  # Add length of the last sequence
  if (!is.null(current_sequence)) {
    if (current_sequence == "UOS") {
      uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
    } else if (current_sequence == "DOS") {
      dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
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
prot2 <- calculate_OS_length(prot2)
prot2 <- prot2 %>%
    mutate(
        ExitOS = case_when(
        signal == -1 ~ mid * (1 - th),
        signal == 1 ~ mid * (1 + th),
        TRUE ~ 0
        )
    )

############### EXIT SIGNAL GENERATION ###############
############### COASTLINE TRADING $$$$$$$$$$$$$$$$$$$

generateExitSignals <- function(data) {
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
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  
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

prot3 <- generateExitSignals(prot2)

exits <- prot3 %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    ) %>% 
                    filter(change == 2)

View(exits)

prot4 <- prot3 %>%
  mutate(
    out_exits = row_number() %in% exits$row_number,
    signal = if_else(in_entries, signal, 0),
    signalE = ifelse(signalE != 0, signalE, 0),
    signalE = if_else(out_exits, signalE, 0),
    signal = if_else(signal != signalE & signalE != 0, signalE, signal),
    position = lag(signal, default =) # final signal
  ) %>% 
    select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, signal, signalOS, signalE, L, position, in_entries, out_exits, Exit, ExitOS, OS_length)

View(prot4) # final table with signals
table(prot4$signal) # final signals


# Create a ggplot object
p <- ggplot(prot4, aes(x = Date, y = Close)) +
  geom_line() +  # Add a line plot for Close price
  labs(x = "Date", y = "Close Price", title = "Close Price and Signals") +  # Set axis labels and plot title
  theme_minimal()  # Use minimal theme (optional)

# Add vertical dashed lines for buy signals (green)
p + geom_vline(data = subset(prot4, signal == 1), aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "green") +
  # Add vertical dashed lines for sell signals (red)
  geom_vline(data = subset(prot4, signal == -1), aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red")


################################## do it for one data (self$data) ##################################
################################## ################################## ################################## 
################################## ################################## ################################## 
################################## ################################## ################################## 
################################## ################################## ################################## 


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
  if (!is.na(prev_mid) && abs(prot$mid[i] - prev_mid) > th * prev_mid) {
    prev_mid <- prot$mid[i]  # Update prev_mid
  }
}

th <- 0.005 # assymetric
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
    select(Date, High, Low, Close, mid, change_value, OS, dc, L, signal, OSS, position, row_number)

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


#View(prot) # here Exits are in the prot2

##############################
# ENTRY SIGNAL GENERATION BASED ON OVERSHOOT DURATION (enter when the number of observed OS in a sequence exceeds or equal to the average OS based on the whole dataset)
##############################

# overshoot duration
calculate_sequence_lengths <- function(h) {
  uos_lengths <- numeric()  # Initialize vector to store UOS sequence lengths
  dos_lengths <- numeric()  # Initialize vector to store DOS sequence lengths
  current_sequence <- NULL  # Initialize variable to store current sequence
  
  for (event in h$OS) {
    if (is.null(current_sequence)) {
      current_sequence <- event  # Initialize current sequence if it's NULL
    } else if (current_sequence != event) {
      if (current_sequence == "UOS") {
        uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
      } else if (current_sequence == "DOS") {
        dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
      }
      current_sequence <- event  # Reset current sequence to new event
    } else {
      current_sequence <- c(current_sequence, event)  # Add event to current sequence
    }
  }
  
  # Add length of the last sequence
  if (!is.null(current_sequence)) {
    if (current_sequence == "UOS") {
      uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
    } else if (current_sequence == "DOS") {
      dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
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

############### EXIT SIGNAL GENERATION ###############
############### COASTLINE TRADING $$$$$$$$$$$$$$$$$$$

generateExitSignals <- function(data) {
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
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  
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

prot <- generateExitSignals(prot)

exits <- prot %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    ) %>% 
                        filter(change == 2)

#View(exits)

prot <- prot %>%
  mutate(
    out_exits = row_number() %in% exits$row_number,
    signal = if_else(in_entries, signal, 0),
    signalE = ifelse(signalE != 0, signalE, 0),
    signalE = if_else(out_exits, signalE, 0),
    signal = if_else(signal != signalE & signalE != 0, signalE, signal),
    position = lag(signal, default = 0)
  ) %>% 
    select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, signal, signalOS, signalE, L, position, in_entries, out_exits, Exit, OS_length)

View(prot) # final table with signals


################################################################################
# Define AlphaEngine (based on intrinsic time approach)
################################################################################

AlphaEngine <- R6Class(
  "AlphaEngine",
  inherit = Strategy,
  public = list(

    threshold = NULL,
    profit_taking = NULL,
    signal_generation = NULL,
    position_sizing = NULL,

initialize = function(data, threshold, profit_taking, signal_generation = "TH", position_sizing = FALSE) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$threshold <- threshold
      self$profit_taking <- profit_taking
      self$signal_generation <- signal_generation
      self$position_sizing <- position_sizing
},

generate_signals = function() {

    cat("Threshold value:", self$threshold, "\n")  # Print the threshold value

    # Mid data
    self$data <- self$data %>%
        mutate(
            mid = (High + Low) / 2
        )

    # STEP1 : DETECT DIRECTIONAL CHANGES
    self$data <- private$identify_events(data = self$data, threshold = self$threshold)

    # STEP2 : INTRODUCE PROBABILITY INDICATOR FOR POSITION SIZING

    # Function to compute transition probabilities and surprises

    self$data$surprise <- NA_real_
    self$data$H1 <- NA_real_
    self$data$H2 <- NA_real_

    # Loop over rows to compute probability and surprise for each row
    for (i in 1:nrow(self$data)) {

        data <- slice(self$data, 1:i)  # Subset data up to current row
        probs_surprises <- private$estimate_prob_surprise(data)  # Compute probabilities and surprises
        entropies <- private$estimate_entropies(data)

        self$data$surprise[i] <- sum(probs_surprises$surprise) # surprise of the transition
        self$data$H1[i] <- entropies$H1
        self$data$H2[i] <- entropies$H2
    }

    # Initialize K with 1
    self$data$K <- 1

    # Initialize a vector to store unique combinations
    unique_combinations <- c(paste(self$data$events[1], self$data$j[1]))
    self$data$j <- lag(self$data$events, default = 0)

    # Iterate over rows and update K when a new combination is identified
    for (i in 2:nrow(self$data)) {
    current_combination <- paste(self$data$dc[i], self$data$j[i])
    if (!(current_combination %in% unique_combinations)) {
        unique_combinations <- c(unique_combinations, current_combination)
    }
    self$data$K[i] <- min(length(unique_combinations), 4)
    if (length(unique_combinations) > 4) {
        self$data$K[i:nrow(self$data)] <- 4
        break
    }
    }

    # estimate L
    self$data <- self$data %>%
    mutate(
        surprise = replace_na(surprise, 0),
        d = (surprise - K * H1) / sqrt(K * H2),
        L = 1 - pnorm(d)
    )

    # STEP3 : GENERATE ENTRY SIGNALS (BASED ON THRESHOLD)

    # Identify threshold value (which is used to compare current mid price)

    # Initialize an empty vector to store mid prices for the change_value
    self$data$change_value <- NA

    # Initialize variables
    prev_dc_index <- NULL
    prev_mid <- NULL

    # Loop through each row
    for (i in 1:nrow(self$data)) {
    # Check if dc is TRUE
    if (self$data$dc[i]) {
        # If this is the first dc or if it's not consecutive with the previous one
        if (is.null(prev_dc_index) || i != prev_dc_index + 1) {
        prev_dc_index <- i  # Update prev_dc_index
        prev_mid <- self$data$mid[i]  # Record the mid price
        }
    }
    # Assign the previous mid price to the change_value column
    self$data$change_value[i] <- prev_mid
    
    # Check if the price further changes by the threshold and update prev_mid accordingly
    if (!is.na(prev_mid) && !is.na(self$data$OS[i])) {
      if (self$data$OS[i] == "UOS" && self$data$mid[i] > prev_mid * (1 + self$profit_taking)) {
        prev_mid <- self$data$mid[i]  # Update prev_mid for UOS
      } else if (self$data$OS[i] == "DOS" && self$data$mid[i] < prev_mid * (1 - self$profit_taking)) {
        prev_mid <- self$data$mid[i]  # Update prev_mid for DOS
      }
    }
  }

    #self$profit_taking <- 0.005 # assymetric
    self$data <- self$data %>%
    mutate(
        row_number = row_number(),
        signal = case_when(
        OS == "UOS" & mid >= change_value * (1 + self$profit_taking) ~ -1,
        OS == "DOS" & mid <= change_value * (1 - self$profit_taking) ~ 1,
        TRUE ~ 0
        ),
        position = lag(signal, default = 0),
        OSS = mid >= change_value * (1 + self$profit_taking) | mid <= change_value * (1 - self$profit_taking) 
    ) %>% 
        select(Date, High, Low, Close, mid, change_value, OS, dc, events, L, signal, OSS, position, row_number)

    entries <- self$data %>% 
            filter(signal == 1 | signal == -1) %>%
                mutate(
                    change = c(2, abs(diff(signal))), default = 0,
                    Exit = case_when(
                        signal == -1 ~ mid * (1 - self$profit_taking),
                        signal == 1 ~ mid * (1 + self$profit_taking),
                        TRUE ~ 0
                        )
                    )

    self$data <- self$data %>%
    mutate(in_entries = row_number() %in% entries$row_number,
    signal = ifelse(in_entries, signal, 0),
    position = lag(signal, default = 0)
    ) %>% 
        left_join(entries %>% select(row_number, Exit), by = c("row_number" = "row_number"))


    # STEP3 : GENERATE ENTRY SIGNALS (BASED ON AVERAGE LENGTH OF OVERSHOOTS : overshoot duration)

    # Calculate sequence lengths for UOS and DOS
    sequence_lengths <- private$calculate_sequence_lengths(self$data)

    avgOS <- data.frame(avgUOS = floor(mean(sequence_lengths$uos_lengths)), avgDOS = floor(mean(sequence_lengths$dos_lengths)))

    # Usage:
    self$data <- private$calculate_OS_length(self$data)
    self$data <- self$data %>%
        mutate(
            ExitOS = case_when(
            signalOS == -1 ~ mid * (1 - self$profit_taking),
            signalOS == 1 ~ mid * (1 + self$profit_taking),
            TRUE ~ 0
            )
        )

    # Updating signal and Exits value given signal based method

    self$data <- self$data %>%
      mutate(
        signal = case_when(
          self$signal_generation == "OS" ~ signalOS,
          TRUE ~ signal  # No change for other cases
        ),
        Exit = case_when(
          self$signal_generation == "OS" ~ ExitOS,
          TRUE ~ Exit  # No change for other cases
        )
      )

    # STEP4 : EXIT SIGNALS GENERATION (CASCADE AND DE-CASCADE POSITIONS)

    self$data <- private$generateExitSignals(self$data, self$signal_generation)

    exits <- self$data %>% 
            filter(signalE == 1 | signalE == -1) %>%
                mutate(
                    change = c(2, abs(diff(signalE))),
                        ) %>% 
                            filter(change == 2)

    # self$data <- self$data %>%
    # mutate(
    #     out_exits = row_number() %in% exits$row_number,
    #     signal = if_else(in_entries, signal, 0),
    #     signalE = ifelse(signalE != 0, signalE, 0),
    #     signalE = if_else(out_exits, signalE, 0),
    #     signal = if_else(signal != signalE & signalE != 0, signalE, signal), # shift to have more exits rather than entries
    #      #signal = if_else(signal != signalE & signalE != 0, signalE, signal), # signals cancel entry and exit
    #     position = lag(signal, default = 0)
    # ) %>% select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, events,
    #     signal, signalOS, signalE, signalE_closes_row, L, position, in_entries, out_exits, Exit, OS_length)

    self$data <- self$data %>%
      mutate(
        out_exits = row_number() %in% exits$row_number,
        signal = if_else(in_entries, signal, 0),
        signalE = ifelse(signalE != 0, signalE, 0),
        signalE = if_else(out_exits, signalE, 0),
        signal = if_else(signal != signalE & signalE != 0, signalE, signal),
        position = lag(signal, default = 0),
        nop_sizing = case_when(
          L < 0.1 ~ 0.1,
          L < 0.4 ~ 0.5,
          TRUE ~ 1
      )
  ) %>% 
    mutate(
      L = replace_na(L, 1)
  ) %>% 
      select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, events,
         signal, signalOS, signalE, signalE_closes_row, L, nop_sizing, position, in_entries, out_exits, Exit, OS_length)

  if (!self$position_sizing) {
    self$data <- subset(self$data, select = -nop_sizing)
  }

},
    
# Plot directional changes and market overshoots: 
plot_events = function(symbol) {

    #data <- na.omit(self$data)

    p <- ggplot(self$data, aes(x = Date, y = Close, color = OS)) +
    geom_point(data = self$data[self$data$dc == TRUE,], aes(shape = "dc"), color = "black", size = 2) +  # Black triangles for dc
    geom_point(data = self$data, aes(shape = ifelse(dc, NA, "Close")), size = 1, alpha = 0.6) +  # Regular points with decreased size and some transparency
    scale_color_manual(values = c("black", "red", "green"),
                        labels = c("Regular", "Downward Overshoot", "Upward Overshoot")) +
    scale_shape_manual(values = c("dc" = 17, "Close" = 16), 
                        labels = c("Close", "Directional Change")) +
    labs(title = paste("Market overshoots and directional changes for", symbol),
        x = "Date", y = "Close Price") +  # Adding axis labels
    
    scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +

    theme_minimal() +
    theme(legend.position = "bottom",  # Moving legend to bottom
            axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels for better readability
            plot.title = element_text(size = 14, face = "bold"),  # Increasing title font size and making it bold
            axis.title = element_text(size = 12),  # Increasing axis label font size
            legend.text = element_text(size = 10),  # Adjusting legend text size
            legend.title = element_text(size = 12, face = "bold"))  # Adjusting legend title size and making it bold

    print(p)

},

# Plot Close price given intrinsic time (event based price)
plot_dc = function(symbol) {

    self$data <- self$data %>% 
    filter(events == 1 | events == -1)

    ggplot(self$data, aes(x = Date, y = Close)) +
    geom_line() +  # Plot close price
    geom_vline(data = subset(self$data, events == 1), aes(xintercept = as.numeric(Date)), color = "blue", linetype = "dashed") +  # Add vertical lines for UE
    geom_vline(data = subset(self$data, events == -1), aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed") +  # Add vertical lines for DE
    labs(title = paste("Close Price filtered by Directional Changes for", symbol), x = "Date", y = "Close Price") +
    theme_minimal()

},

plotSignals = function(data) {
  ggplot(self$data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_point(data = subset(self$data, signal == 1), aes(color = "Buy", shape = "Buy"), size = 1.5) +
    geom_point(data = subset(self$data, signal == -1), aes(color = "Sell", shape = "Sell"), size = 1.5) +
    geom_point(data = subset(self$data, signalE == 1), aes(color = "Exit Buy", shape = "Exit Buy"), size = 1.5) +
    geom_point(data = subset(self$data, signalE == -1), aes(color = "Exit Sell", shape = "Exit Sell"), size = 1.5) +
    scale_shape_manual(values = c("Buy" = 1, "Sell" = 2, "Exit Buy" = 1, "Exit Sell" = 2)) +
    scale_color_manual(values = c("Buy" = "green", "Sell" = "red", "Exit Buy" = "darkgreen", "Exit Sell" = "darkred")) +
    labs(title = "Close Price") +
    theme_minimal()
},

run_backtest = function(symbols, thresholds, profit_takings, signal_generations, position_sizings, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      tryCatch({
        # Loop through symbols, window size  and bands to create instances and estimate performance
        for (symbol in symbols) {
          for (threshold in thresholds) {
            for (profit_taking in profit_takings) {
              for (signal_generation in signal_generations) {
                for (position_sizing in position_sizings) {

            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
          
            # Create an instance of Alpha strategy 
            alpha <- AlphaEngine$new(data, threshold, profit_taking, signal_generation, position_sizing)

            # Store the results
            results[[paste(symbol, threshold, profit_taking, signal_generation, position_sizing, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("AlphaEngine:", threshold, profit_taking, signal_generation, position_sizing),
              Threshold = threshold,
              ProfitTaking = profit_taking,
              Signal_generation  = signal_generation,
              Position_sizing = position_sizing,
              Performance = alpha$estimate_performance()
            )

            print(paste0("Results for ", "AlphaEngine: ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
              "threshold:", threshold, ",", "profit_taking:", profit_taking, ",", "signal_generation:", signal_generation, ",", "position sizing:", position_sizing, ")"))
                
                }
              }
            }
          }
        }
      }, error = function(e) {
        # Handle errors
        print(paste("Error in iteration", i, ":", e$message))
      })

      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class = item$Class,
          Methodology = item$Methodology,
          Threshold = item$Threshold,
          ProfitTaking = item$ProfitTaking,
          Signal_generation = item$Signal_generation,
          Position_sizing = item$Position_sizing,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
      } else {
        return(results)
      }
}
  
  ),

  private = list(

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
},

estimate_prob_surprise = function(data) {
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
},

estimate_entropies = function(data) {
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
},

calculate_sequence_lengths = function(h) {
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
},

calculate_OS_length = function(data) {
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
},

generateExitSignals = function(df, signal_generation = "TH") {
    
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

    )
)

################################################################################
# IMPLEMENTATION (class instances)
################################################################################

# Instances of AlphaEngine strategy
leverage <- 1
symbol <- "MXN=X"
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")

fxs <- names(Filter(function(x) x$class == "FX", meta$assets))

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

# Exchange rate evolution  over time
data_fetcher$plot_close_or_rets(type = "close")
data_fetcher$plot_close_or_rets(type = "rets")
data_fetcher$plot_close_or_rets(type = "rets_hist")
data_fetcher$compute_NA_close_price_ratio()

# Instance of AlphaEngine class given threshold
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005) # signal_generation by default is based on threshold
#alpha1$generate_signals()
alpha1$estimate_performance()
alpha1$plot_equity_lines(paste0("EventBased for ", symbol), signal_flag = TRUE)
alpha1$plot_events(symbol)
alpha1$plot_dc(symbol)
#a <- alpha1$data # check how the final data looks like

################################################################################
# PICK UP ONLY LEAST VOLATILE CURRENCIES
################################################################################

# Take least volatile currencies

# Downlaod all FX data
data_fetcher_mult <- DataFetcher$new(fxs, from_date, to_date, type = "Close")
df_fx <- data_fetcher_mult$convert_xts_to_wide_df()

# Compute standard deviations (volatility proxy) for each column except the first one (Date)
lapply(df_fx[-1, ], sd)
vol_df <- data.frame(FX = names(df_fx)[-1],
                        SD = unlist(lapply(df_fx[-1, -1], sd)))

# Rank the columns based on their standard deviation
vol_df <- vol_df[order(vol_df$SD), , drop = FALSE]

print(vol_df) 

# TOP5 most mean reverting ones (in descending order) are:
# EUR=X (USD/EUR)
# AUDUSD=X (AUD/USD))
# EURUSD=X (EUR/USD)
# CAD=X (USD/CAD)
# GBPUSD=X (GBP/USD)

##############################
# Test strategy
##############################
symbol <- "GBPUSD=X"
symbol <- "NZDJPY=X"
#from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
#from_date <- as.Date("2020-01-01", format = "%Y-%m-%d")
#to_date <- Sys.Date()
from_date <- as.Date("2006-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2015-01-01", format = "%Y-%m-%d")
##############################
# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

# Exchange rate evolution  over time
data_fetcher$plot_close_or_rets(type = "close")

# Instance of AlphaEngine class given threshold
# alpha1 <-  AlphaEngine$new(ts, threshold = 2.525729 * 0.01, profit_taking = 0.005, signal_generation = "TH")
# alpha1 <-  AlphaEngine$new(ts, threshold = 0.015, profit_taking = 0.005, signal_generation = "OS")
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH")
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE) # AR: 15%

#alpha1$generate_signals()
alpha1$estimate_performance()
alpha1$plot_equity_lines(paste0("AlphaEngine for ", symbol), signal_flag = TRUE)
alpha1$plot_events(symbol)
alpha1$plot_dc(symbol)
alpha1$plotSignals()
a <- alpha1$data

##############################
# Run backtest:
 ##############################
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH", position_sizing = FALSE) # signal_generation by default is based on threshold
res_alpha <- alpha1$run_backtest(
  symbols = symbol,  
  thresholds = c(0.01),
  profit_takings = c(0.005, 0.001),
  signal_generations = c("TH", "OS"),
  position_sizings = c(FALSE, TRUE),
  from_date,
  to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)
