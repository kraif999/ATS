# Load all libraries
libraries <- c("PerformanceAnalytics", "ggplot2", "lmtest", "fBasics", "urca", "forecast", "quantmod", "tseries", "fUnitRoots", "xts",  "fBasics", "tseries",
 "car", "FinTS", "fGarch",  "psych", "rugarch", "parallel", "caTools", "plyr", "expss", "base", "tidyr", "dplyr", "MLmetrics", "tibble", "gridExtra", "writexl")

lapply(libraries, library, character.only = TRUE)

options(scipen=999)

# VIX futures contract data
vxData <- readRDS("vxDataAll.rds") 

vxData2 <- # filtering monthly contracts only
  vxData %>%
  filter(Type == "monthly") %>%
  select(TradeDate, ContractName, Close, ttm)

# Download S&P 500 data
sp500 <- getSymbols("^GSPC", from = "2010-01-01", to = "2024-10-03", period = "day", auto.assign = FALSE)

# Remove NAs, compute arithmetic return based on Close price
sp500ret2 <- sp500 %>%
  Cl() %>%
    na.omit() %>%
      Delt() %>%
        tail(-8)

# Open, High, Low, Close
ohlc <- sp500 %>% 
  data.frame %>%
    select(-GSPC.Volume, -GSPC.Adjusted)

# Convert sp500 data xts to data frame and add TradeDate column
sp500gk <- sp500 %>%
  data.frame() %>%
    mutate(TradeDate = as.Date(rownames(.))) %>%
      select(TradeDate, GSPC.Open, GSPC.High, GSPC.Low, GSPC.Close) %>%
        mutate(rets = Delt(GSPC.Close),
          EquityLine = cumprod(ifelse(is.na(rets), 1, 1 + rets)))


# Calculate summary statistics for SP500
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

calculate_summary_statistics(sp500gk)


# GARCH model parameters configuration scope
# https://www.r-bloggers.com/problems-in-estimating-garch-parameters-in-r-part-2-rugarch/

# Generate all possible combinations including performance metrics
listgarch <- expand.grid(
  specification = as.character(c("sGARCH", "eGARCH", "gjrGARCH", "fGARCH")),
  n.start = as.numeric(c("252", "126", "504")),
  refit.every = as.numeric(c("21", "63", "126")),
  refit.window = as.character(c("moving", "expanding")),
  distribution.model = as.character(c("norm", "snorm", "nig")),
  realized.vol = as.character(c("garman", "close")),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  trades = 0,
  signal_long = 0,
  rmse = 0,
  elf = 0,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>% mutate(across(c(aR, aSD, MD, IR, trades, signal_long, rmse, elf), as.numeric))
colnames(listgarch)[1:6] <- c("specification","window.size", "refit.frequency", "refit.window.type", "distribution.model", "realized.vol.method")

str(listgarch)

# VIX futures contracts investing assumptions 
dfl <- 0.25 # financial leverage
m <- 1000 # the multiplicator for one VIX futures point change is 1000 USD
capital <- 1000000 # initial investment equals 1000000


# Socket cluster with 8 nodes on host ‘localhost’
getDoParWorkers()
max_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(max_cores)
#cl = makePSOCKcluster(5)
#registerDoParallel(cl)
#stopImplicitCluster()
#registerDoSEQ() # Registering a sequential backend

# Define the variables to be exported
# export_vars <- c("aR", "aSD", "MD", "IR", "trades", "signal_long", "rmse", "ELF")
# foreach(i = 1:nrow(listgarch), .combine = rbind, .export = export_vars) %dopar% {
#   tryCatch({

# Create a directory to save the plots if it doesn't exist
if (!file.exists("EquityLines")) {
  dir.create("EquityLines")
}

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
        sp500ret2[,1], 
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
        sp500ret2[,1], 
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

roll = resume(roll, solver= "gosolnp") # if object contains non-converged windows
#plot(roll)

show(roll) # 20.02 secs

# Preparing the data:
roll.df <- roll %>% 
  as.data.frame %>% 
    mutate(TradeDate = as.Date(rownames(.))) %>% 
      `[`(c(7, 1:6))

# rm(roll)

sp500gk2 <- sp500gk[sp500gk$TradeDate >= roll.df$TradeDate[1],]

# Realized volatility estimation by different approaches
roll.df$RV <- 0
# Select relevant OHLC data and rename columns
ohlc <- sp500gk2[, c("GSPC.Open", "GSPC.High", "GSPC.Low", "GSPC.Close")]
colnames(ohlc) <- c("Open", "High", "Low", "Close")

# Calculate realized volatility using ifelse and coerce result to vector and remove NAs
roll.df <- roll.df %>% 
  mutate(RV = case_when(
    listgarch[i,6] != "close" ~ as.numeric(unlist(volatility(ohlc, calc = listgarch[i,6], N = 252) / sqrt(252))),
    TRUE ~ as.numeric(volatility(ohlc[, 4], n = 21, calc = "close", N = 252) / sqrt(252))
  )) %>% na.omit

rmse <- round(RMSE(roll.df$Sigma, roll.df$RV) * 100, 3)

roll.df$elf <- 0
roll.df$elf[2:nrow(roll.df)] <- ifelse(
  (
    (roll.df$RV[2:nrow(roll.df)] > roll.df$RV[1:(nrow(roll.df) - 1)]) & (roll.df$RV[1:(nrow(roll.df) - 1)] < roll.df$Sigma[2:nrow(roll.df)])
   ) 
   |
  (
    (roll.df$RV[2:nrow(roll.df)] < roll.df$RV[1:(nrow(roll.df) - 1)]) & (roll.df$RV[1:(nrow(roll.df) - 1)] > roll.df$Sigma[2:nrow(roll.df)])
    ), 
        1, # sign of change is predicted
            0) # sign of change is NOT predicted

ELF <- round(sum(roll.df$elf) / nrow(roll.df) * 100,3) # percentage of correct sign  prediction

# Signal generation
# Compare Sigma with estimated volatilty (Realized Volatility by 'Close' method from TTR library)
roll.df$signal <- 0
for(z in 1:nrow(roll.df)) {
  roll.df$signal[z] <- ifelse(roll.df$RV[z] < roll.df$Sigma[z+1],
                              1, # long
                              ifelse(roll.df$RV[z] > roll.df$Sigma[z+1],
                                      -1, # short
                                      roll.df$signal[z-1])) # stay flat
}

# Selecting TradeDate and Signals only
rollSignal.df <- 
  roll.df %>% 
    select(TradeDate, signal)

# Pulling the contract name and its close price which is the 1st closest to expiration on a particular TradeDate
tmp1 <- vxData2 %>% 
  as_tibble() %>%
    arrange(TradeDate, ttm) %>%
      group_by(TradeDate) %>%
        filter(row_number() == 1) %>%
            ungroup() %>%
            dplyr::rename(ContractName1 = ContractName,
            Close1 = Close,
            ttm1 = ttm)

# Pulling the contract name and its close price which is the 2nd closest to expiration on a particular TradeDate
tmp2 <- vxData2 %>% 
  as_tibble() %>%
    arrange(TradeDate, ttm) %>%
      group_by(TradeDate) %>%
        filter(row_number() == 2) %>%
            ungroup() %>%
            dplyr::rename(ContractName2 = ContractName,
            Close2 = Close,
            ttm2 = ttm)

# Merging the above two into the tbl dataframe
tmp3 <-
  tmp1 %>%
  left_join(tmp2) %>%
  left_join(rollSignal.df)  %>% 
  mutate(Close = ifelse(ttm1 == 0, Close2, Close1))

# For some models NAs are produced in signal, replacing NA by 0s
tmp3$TradeDate[is.na(tmp3$signal)]
tmp3$signal[is.na(tmp3$signal)] <- 0

# Carrying signal forward if no change
for (v in 1:nrow(tmp3)){
  if(tmp3$signal[v] == 0) {
    tmp3$signal[v] = tmp3$signal[v-1]
  }
}

# Count of trades
tmp3$trades <- 0
tmp3$trades[1] <- 1
for(q in 2:nrow(tmp3)) {
  tmp3$trades[q] <- ifelse(tmp3$signal[q-1] * tmp3$signal[q] < 0, 1, 0)
}

short <- sum(tmp3$signal < 0) # how many times we sell
long <- sum(tmp3$signal > 0) # how many times we buy

# Close price arithmetic returns
tmp3$rets <- c(0, diff(tmp3$Close)/tmp3$Close[-length(tmp3$Close)])

# Absolute PnL
tmp3$PnL <- 0
for(l in 2:nrow(tmp3)) {
  tmp3$PnL[l] <- 
    ifelse(tmp3$ttm1[l] == 0,
            tmp3$signal[l-1]*(tmp3$Close2[l]-tmp3$Close2[l-1]),
              tmp3$signal[l-1]*(tmp3$Close[l]-tmp3$Close[l-1])) 
}

# Strategy based on GARCH model forecasts
tmp3$nop <- 0 # number of positions
tmp3$eqlGARCH <- 0 # capital + PnL * nop * m - TC * delta nop
tmp3$eqlGARCH[1] <- capital # initial investment
tmp3$nop[1] <- capital * dfl / (tmp3$Close1 * m) 
TC <- 2 # transactional costs 2 USD per 1 position

for(s in 2:nrow(tmp3)) {
  tmp3$eqlGARCH[s] <- tmp3$eqlGARCH[s-1] + tmp3$nop[s-1] * tmp3$PnL[s] * m - TC * (abs(tmp3$nop[s] - tmp3$nop[s-1]))
  tmp3$nop[s] <- floor(tmp3$eqlGARCH[s] * dfl / (tmp3$Close[s] * m)) # using 'floor' function to round 
}
tmp3$r_eqlGARCH <- quantmod::Delt(tmp3$eqlGARCH) # equity line return

# Passive strategy for VIX futures (buy and hold)
tmp3$eqlVIXBH <- 0
tmp3$eqlVIXBH[1] <- capital 
tmp3$nopBH[1] <- capital * dfl / (tmp3$Close1 * m) # number of positions at first entry

for(p in 2:nrow(tmp3)) {
  TC <- ifelse(tmp3$ttm1[p] == 0, 2 * abs(tmp3$nopBH[p-1]), 0) # adding transactional costs given contract rolling forward
  tmp3$eqlVIXBH[p] <- tmp3$eqlVIXBH[p-1] + (tmp3$Close[p] - tmp3$Close[p-1]) * tmp3$nopBH[p-1] * m - TC
  tmp3$nopBH[p] <- floor(tmp3$eqlVIXBH[p] * dfl / (tmp3$Close[p] * m)) # using 'floor' function to round 
}
tmp3$r_eqlVIXBH <- quantmod::Delt(tmp3$eqlVIXBH) # equity line return

# Passive strategy for SP500 data
tmp3 <- left_join(tmp3, sp500gk %>% mutate(retsSP = rets) %>% 
  select(TradeDate, retsSP), by = "TradeDate") %>%
    mutate(eqlSP500BH = ifelse(row_number() == 1, 1, cumprod(1 + na.locf(retsSP))) * capital)

tmp3$r_eqlSP500BH <- quantmod::Delt(tmp3$eqlSP500BH)

# Performance metrics of strategy based on GARCH
aR <- round(as.numeric(Return.annualized(as.numeric(tmp3$r_eqlGARCH), scale = 252, geometric = TRUE) * 100), 3)
aSD <- round(as.numeric(StdDev.annualized(as.numeric(tmp3$r_eqlGARCH), scale = 252) * 100), 3)
IR <- round(as.numeric(aR / aSD), 3) 
MD <- round(as.numeric(maxDrawdown(as.numeric(tmp3$r_eqlGARCH), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
trades <- sum(tmp3$trades) # count of all trades
signal_long <- round(long/(long+short),3) * 100 # long signal percentage
rmse <- rmse
elf <- ELF

listgarch[i, "aR"] <- aR
listgarch[i, "aSD"] <- aSD
listgarch[i, "MD"] <- MD
listgarch[i, "IR"] <- IR
listgarch[i, "trades"] <- trades
listgarch[i, "signal_long"] <- signal_long
listgarch[i, "rmse"] <- rmse
listgarch[i, "ELF"] <- ELF

# assign("listgarch", c(aR, aSD, MD, IR, trades, signal_long, rmse, ELF), pos = i)
    
# For VIX strategy
passive1 <- data.frame(
  Buy_and_Hold = as.character("VIX"),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  stringsAsFactors = FALSE
) %>%
mutate(
  aR = round(as.numeric(Return.annualized(as.numeric(tmp3$r_eqlVIXBH), scale = 252, geometric = TRUE) * 100), 3),
  aSD = round(as.numeric(StdDev.annualized(as.numeric(tmp3$r_eqlVIXBH), scale = 252) * 100), 3),
  IR = round(aR / aSD, 3),
  MD = round(as.numeric(maxDrawdown(as.numeric(tmp3$r_eqlVIXBH), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
)

# For SP500 strategy
passive2 <- data.frame(
  Buy_and_Hold = as.character("S&P500"),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  stringsAsFactors = FALSE
) %>%
mutate(
  aR = round(as.numeric(Return.annualized(as.numeric(tmp3$r_eqlSP500BH), scale = 252, geometric = TRUE) * 100), 3),
  aSD = round(as.numeric(StdDev.annualized(as.numeric(tmp3$r_eqlSP500BH), scale = 252) * 100), 3),
  IR = round(aR / aSD, 3),
  MD = round(as.numeric(maxDrawdown(as.numeric(tmp3$r_eqlSP500BH), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
)

passive <- rbind(passive1, passive2)

active_passive <- bind_rows(passive %>% rename(Strategy_Specification = Buy_and_Hold), 
          listgarch[i,] %>% rename(Strategy_Specification = specification) %>% select(Strategy_Specification, aR, aSD, MD, IR))

print(active_passive)
print(listgarch[i,])

#Equity lines for strategy based on GARCH and two passive buy-and-hold strategies: investing in S$P500 and VIX futures contract
p <- ggplot(tmp3, aes(x = TradeDate)) +
      geom_line(aes(y = eqlGARCH, color = "Strategy based on GARCH"), na.rm = TRUE) +
      geom_line(aes(y = eqlVIXBH, color = "Buy & hold VIX futures"), na.rm = TRUE) +
      geom_line(aes(y = eqlSP500BH, color = "Buy & hold S&P500"), na.rm = TRUE) +
      ggtitle("The comparison of equity lines between GARCH based strategy and passive investing") +
      scale_color_manual(values = c("Strategy based on GARCH" = "red", "Buy & hold VIX futures" = "blue", "Buy & hold S&P500" = "black")) +
      labs(x = "TradeDate", y = "Return") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))

# Save the plot with a unique filename
plot_filename <- paste0("EquityLines/plot_", 
                        listgarch[i,"specification"], "_", 
                        listgarch[i,"window.size"], "_",
                        listgarch[i, "refit.frequency"], "_",
                        listgarch[i, "refit.window.type"], "_",
                        listgarch[i, "distribution.model"], "_",
                        listgarch[i, "realized.vol.method"], ".png")

ggsave(plot_filename, p)

# free up the space before the next iteration:
# rm(spec, roll.df, roll11.df, tmp1, tmp2, tmp3)
rm(passive1, passive2, spec, roll.df, tmp1, tmp2, tmp3)

} ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#   }, error = function(e) {
#      cat("ERROR :",conditionMessage(e), "\n")
#   })
# }

fwrite(listgarch, "listgarchAll.csv")