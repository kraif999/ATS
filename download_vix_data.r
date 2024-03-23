# latest VIX data: dates and names (prepared via JavaScript, see save_vix_expiration_dates.js)
lastUpdateDate <- format(Sys.Date(), "%Y-%m-%d")  # "2024-03-17"
data <- read.csv("vix_names_dates.csv", row.names = NULL)

data <- data %>%
  mutate(
    ContractDescription = paste(product_display, sprintf("(%s)", format(as.Date(contract_dt), "%b %Y"))),
    Expired = expire_date < lastUpdateDate) %>% 
      arrange(expire_date) %>%
  mutate(
    Type = ifelse(
      substr(ContractDescription, 7, 7) %in% (0:9 %>% as.character()),
      "weekly",
      "monthly")
    ) %>%
        dplyr::rename(ExpiryDate = expire_date) %>%
            select(ExpiryDate, ContractDescription, Expired, Type)

head(data)

# Usage example
url_base <- "https://cdn.cboe.com/data/us/futures/market_statistics/historical_data/VX/VX_"
path <- "" # specify the path where all VIX data (.rds files for each expiration date) is saved

download_and_save_data <- function(dataWeb, url_base, path) {
  for (thisDate in dataWeb$ExpiryDate) {
    url <- paste0(url_base, thisDate, ".csv")
    
    # Read CSV data from URL
    df <- readr::read_csv(url)
    df$ExpiryDate <- thisDate
    
    # Save as RDS file
    saveRDS(df, file = paste0(path, thisDate, ".rds"))
  }
}

download_and_save_data(data, url_base, path)

# vx_instance <- readRDS(file.path(path, "2013-02-13.rds"))
# View(vx_instance)

# Get a list of all .rds files in the directory
rds_files <- list.files(path, full.names = TRUE)
str(rds_files)

# Read each .rds file and bind them row-wise into one long table
vxData <- map_dfr(rds_files, readRDS)

# View the structure of the combined data
str(vxDataAll)

vxDataAll <- vxData %>%
  dplyr::rename(TradeDate = `Trade Date`,
         TotalVolume = `Total Volume`,
         OpenInterest = `Open Interest`,
         ContractName = Futures) %>%
            arrange(TradeDate, ContractName)

vxDataAll <- vxDataAll %>% left_join(data) %>% 
    mutate(ttm = as.Date(ExpiryDate) - TradeDate)
    
vxDataAll %>% saveRDS("vxDataAll.rds")

