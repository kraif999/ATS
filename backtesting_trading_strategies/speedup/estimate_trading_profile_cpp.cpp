#include <Rcpp.h>
#include <cmath>
#include <numeric>  // For std::partial_sum
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector extract_years(NumericVector date_vec) {
    std::set<int> unique_years;
    
    for (double date : date_vec) {
        // Convert numeric date to Date object
        Date dt = Date(date);
        int year = dt.getYear(); // Extract year
        unique_years.insert(year);
    }

    // Convert set to IntegerVector
    return IntegerVector(unique_years.begin(), unique_years.end());
}

// Custom lag function
IntegerVector lag_custom(IntegerVector vec, int lag_val = 1) {
    
  IntegerVector result(vec.size(), NA_INTEGER);
  for (int i = lag_val; i < vec.size(); i++) {
    result[i] = vec[i - lag_val];
  }
  return result;
}


// [[Rcpp::export]]
List calculateDrawdown(NumericVector equity, NumericVector dates) {
    int n = equity.size();
    if (n == 0) return List::create(Named("MaxDrawdown") = NA_REAL,
                                    Named("StartDateMaxDrawdown") = NA_REAL,
                                    Named("EndDateMaxDrawdown") = NA_REAL,
                                    Named("LengthOfMaxDrawdown") = NA_INTEGER);

    NumericVector cum_max_eql(n);
    NumericVector drawdown(n);

    // Compute cumulative max equity and drawdown
    cum_max_eql[0] = equity[0];
    for (int i = 1; i < n; i++) {
        cum_max_eql[i] = std::max(cum_max_eql[i - 1], equity[i]);
    }
    for (int i = 0; i < n; i++) {
        drawdown[i] = (equity[i] - cum_max_eql[i]) / cum_max_eql[i];
    }

    // Maximum drawdown percentage
    // double MaxDrawdown = round(100.0 * min(drawdown));
    double MaxDrawdown = std::round(100.0 * min(drawdown) * 100) / 100;

    // Find peak (start of max drawdown) and trough (lowest drawdown point)
    int trough_idx = which_min(drawdown);
    int peak_idx = 0;
    for (int i = 0; i < trough_idx; i++) {
        if (equity[i] == cum_max_eql[trough_idx]) {
            peak_idx = i;
            break;
        }
    }

    // Get start and end dates
    double StartDateMaxDrawdown = peak_idx >= 0 ? dates[peak_idx] : NA_REAL;
    double EndDateMaxDrawdown = trough_idx >= 0 ? dates[trough_idx] : NA_REAL;

    // Length of max drawdown period
    int LengthOfMaxDrawdown = !R_IsNA(StartDateMaxDrawdown) && !R_IsNA(EndDateMaxDrawdown) 
        ? EndDateMaxDrawdown - StartDateMaxDrawdown 
        : NA_INTEGER;

    return List::create(
        Named("MaxDrawdown") = MaxDrawdown,
        Named("StartDateMaxDrawdown") = StartDateMaxDrawdown,
        Named("EndDateMaxDrawdown") = EndDateMaxDrawdown,
        Named("LengthOfMaxDrawdown") = LengthOfMaxDrawdown
    );
}

// Function to calculate Maximum Run-Up
List calculateMaxRunUp(NumericVector equity, NumericVector dates) {
    int n = equity.size();
    if (n == 0) return List::create(Named("MaxRunUp") = NA_REAL,
                                    Named("StartDateMaxRunUp") = NA_REAL,
                                    Named("EndDateMaxRunUp") = NA_REAL,
                                    Named("LengthOfMaxRunUp") = NA_INTEGER);

    NumericVector cum_min_eql(n);
    NumericVector run_up(n);

    // Compute cumulative minimum equity and run-up
    cum_min_eql[0] = equity[0];
    for (int i = 1; i < n; i++) {
        cum_min_eql[i] = std::min(cum_min_eql[i - 1], equity[i]);
    }
    for (int i = 0; i < n; i++) {
        run_up[i] = (equity[i] - cum_min_eql[i]) / cum_min_eql[i];
    }

    // Maximum run-up percentage
    double MaxRunUp = round(100.0 * max(run_up));

    // Find low point (start of max run-up) and peak (highest point)
    int peak_idx = which_max(run_up);
    int low_idx = 0;
    for (int i = 0; i < peak_idx; i++) {
        if (equity[i] == cum_min_eql[peak_idx]) {
            low_idx = i;
            break;
        }
    }

    // Get start and end dates
    double StartDateMaxRunUp = low_idx >= 0 ? dates[low_idx] : NA_REAL;
    double EndDateMaxRunUp = peak_idx >= 0 ? dates[peak_idx] : NA_REAL;

    // Length of max run-up period
    int LengthOfMaxRunUp = !R_IsNA(StartDateMaxRunUp) && !R_IsNA(EndDateMaxRunUp) 
        ? EndDateMaxRunUp - StartDateMaxRunUp 
        : NA_INTEGER;

    return List::create(
        Named("MaxRunUp") = MaxRunUp,
        Named("StartDateMaxRunUp") = StartDateMaxRunUp,
        Named("EndDateMaxRunUp") = EndDateMaxRunUp,
        Named("LengthOfMaxRunUp") = LengthOfMaxRunUp
    );
}

// Function to calculate Length of Average Win
double calculateLengthOfAverageWin(IntegerVector trade_id, NumericVector pnl_col, NumericVector date_vec) {
    std::map<int, std::vector<int>> trade_dates_map;
    std::map<int, double> trade_pnls_map;

    // Group by trade_id and store the dates and PnL for each trade
    for (int i = 0; i < trade_id.size(); i++) {
        if (pnl_col[i] > 0) { // Only consider positive PnL for wins
            trade_dates_map[trade_id[i]].push_back(i);  // Store the index of the date
            trade_pnls_map[trade_id[i]] += pnl_col[i];  // Aggregate PnL for each trade
        }
    }

    // Calculate the length of each winning trade
    std::vector<int> win_lengths;
    for (const auto& trade : trade_dates_map) {
        if (trade_pnls_map[trade.first] > 0) {  // Only consider winning trades
            // Calculate the length of the win (difference between first and last date of the trade)
            int length_of_win = date_vec[trade.second.back()] - date_vec[trade.second.front()] + 1;
            win_lengths.push_back(length_of_win);
        }
    }

    // Calculate the average length of wins
    if (win_lengths.empty()) return NA_REAL;

    double sum_lengths = std::accumulate(win_lengths.begin(), win_lengths.end(), 0.0);
    return std::round(sum_lengths / win_lengths.size());
}

// Function to calculate Length of Average Loss
double calculateLengthOfAverageLoss(IntegerVector trade_id, NumericVector pnl_col, NumericVector date_vec) {
    std::map<int, std::vector<int>> trade_dates_map;
    std::map<int, double> trade_pnls_map;

    // Group by trade_id and store the dates and PnL for each trade
    for (int i = 0; i < trade_id.size(); i++) {
        if (pnl_col[i] < 0) { // Only consider negative PnL for losses
            trade_dates_map[trade_id[i]].push_back(i);  // Store the index of the date
            trade_pnls_map[trade_id[i]] += pnl_col[i];  // Aggregate PnL for each trade
        }
    }

    // Calculate the length of each losing trade
    std::vector<int> loss_lengths;
    for (const auto& trade : trade_dates_map) {
        if (trade_pnls_map[trade.first] < 0) {  // Only consider losing trades
            // Calculate the length of the loss (difference between first and last date of the trade)
            int length_of_loss = date_vec[trade.second.back()] - date_vec[trade.second.front()] + 1;
            loss_lengths.push_back(length_of_loss);
        }
    }

    // Calculate the average length of losses
    if (loss_lengths.empty()) return NA_REAL;

    double sum_lengths = std::accumulate(loss_lengths.begin(), loss_lengths.end(), 0.0);
    return std::round(sum_lengths / loss_lengths.size());
}

//[[Rcpp::export]]
List estimate_trading_profile_cpp(DataFrame data_subset, std::string strategy_type) {
NumericVector date_vec = data_subset["Date"];
IntegerVector position = data_subset["position"];
IntegerVector trade_id_m = data_subset["trade_id_m"];
IntegerVector trade_id_m2 = data_subset["trade_id_m2"];
NumericVector pnl_col = data_subset[(strategy_type == "Active") ? "pnlActive" : "pnlPassive"];
NumericVector eql_col = data_subset[(strategy_type == "Active") ? "eqlActive" : "eqlPassive"];
NumericVector r_col = data_subset[(strategy_type == "Active") ? "r_eqlActive" : "r_eqlPassive"];
LogicalVector cryptoClass = data_subset["cryptoClass"];

// Gross profit
NumericVector eql_col_clean = na_omit(eql_col);
double GrossProfit = eql_col_clean.size() > 0 ? std::round(eql_col_clean[eql_col_clean.size() - 1] - eql_col_clean[0]) : NA_REAL;
    
// Annualized profit
bool is_crypto = cryptoClass[0];  // First value of CryptoClass as boolean
int trading_days = is_crypto ? 365 : 252;  // 365 for crypto, 252 for traditional assets

NumericVector r_col_clean = na_omit(r_col);
double annualized_return = (r_col_clean.size() > 0) ? (exp(sum(log(1 + r_col_clean)) / r_col_clean.size() * trading_days) - 1) * 100 : NA_REAL;
double AnnualizedProfit = std::round(annualized_return * 100) / 100;
    
// Number of trades per year
IntegerVector years = extract_years(date_vec);
int num_years = unique(years).size();
// int trade_changes = sum(diff(position) != 0);  // Count position changes
int trade_changes = sum(diff(trade_id_m) != 0);  // Count position changes

// Add +1 like R to avoid zero division or missing count
int NumberOfTradesPerYear = (num_years > 0) ? std::round((trade_changes + 1) / static_cast<double>(num_years)) : NA_INTEGER;

// Percentage of winning trades
LogicalVector position_change = position != lag_custom(position, 1);  // Position change
IntegerVector trade_id(position_change.size());

// Manually convert LogicalVector to IntegerVector
for (int i = 0; i < position_change.size(); i++) {
    trade_id[i] = position_change[i] ? 1 : 0;  // Convert TRUE to 1, FALSE to 0
}

IntegerVector trade_id_cumsum(trade_id.size());

// Use std::partial_sum to accumulate the trade_id changes
std::partial_sum(trade_id.begin(), trade_id.end(), trade_id_cumsum.begin());

NumericVector aggregated_pnl(trade_id_cumsum.size());
std::map<int, double> trade_pnl_map;
for (int i = 0; i < trade_id_cumsum.size(); i++) {
    trade_pnl_map[trade_id_cumsum[i]] += pnl_col[i];
}
int winning_trades = 0;
for (const auto& trade : trade_pnl_map) {
    if (trade.second > 0) winning_trades++;
}
double PercentageOfWinningTrades = std::round((100.0 * winning_trades / trade_pnl_map.size()) * 100) / 100; // Ensure two decimal precision

// Largest win
// double LargestWin = std::round(max(na_omit(pnl_col)));

// Create a map to store the sum of pnl for each trade_id_m2
std::map<int, double> pnl_map;

// Loop through the data and aggregate pnl by trade_id_m2
for (int i = 0; i < trade_id_m2.size(); ++i) {
    int trade_id = trade_id_m2[i];
    double pnl_value = pnl_col[i];
    pnl_map[trade_id] += pnl_value;  // Summing pnl values for the same trade_id_m2
}

// Convert the map to two vectors: one for the trade IDs and one for the summed pnl values
std::vector<int> unique_trade_ids;
std::vector<double> summed_pnl;

for (auto& entry : pnl_map) {
    unique_trade_ids.push_back(entry.first);
    summed_pnl.push_back(entry.second);
}

// Find LargestWin
double LargestWin = std::round(*std::max_element(summed_pnl.begin(), summed_pnl.end()));

// Average win
NumericVector positive_pnls = pnl_col[pnl_col > 0];
double AverageWin = NA_REAL;
if (positive_pnls.size() > 0) {
    // Manually calculate the mean
    double sum_positive_pnls = std::accumulate(positive_pnls.begin(), positive_pnls.end(), 0.0);
    AverageWin = std::round((sum_positive_pnls / positive_pnls.size()) * 100.0) / 100.0;
}

// Average loss
NumericVector negative_pnls = pnl_col[pnl_col < 0];
double AverageLoss = NA_REAL;
if (negative_pnls.size() > 0) {
    // Manually calculate the mean for negative values
    double sum_negative_pnls = std::accumulate(negative_pnls.begin(), negative_pnls.end(), 0.0);
    AverageLoss = std::round((sum_negative_pnls / negative_pnls.size()) * 100.0) / 100.0;
}

// Largest loss
// double LargestLoss = std::round(min(na_omit(pnl_col)));  // Find the minimum pnl value
double LargestLoss = std::round(*std::min_element(summed_pnl.begin(), summed_pnl.end()));

// Compute Max Drawdown
List drawdownResults = calculateDrawdown(eql_col, date_vec);
double MaxDrawdown = drawdownResults["MaxDrawdown"];
double StartDateMaxDrawdown = drawdownResults["StartDateMaxDrawdown"];
double EndDateMaxDrawdown = drawdownResults["EndDateMaxDrawdown"];
int LengthOfMaxDrawdown = drawdownResults["LengthOfMaxDrawdown"];

// Compute Max Run-Up
List maxRunUpResults = calculateMaxRunUp(eql_col, date_vec);
double MaxRunUp = maxRunUpResults["MaxRunUp"];

// Expected profit per trade
double ExpectedAbsoluteReturn = std::round((PercentageOfWinningTrades / 100.0) * (AverageWin + AverageLoss) * 100) / 100;

// Calmar ratio
double CR = std::round((AnnualizedProfit / -MaxDrawdown) * 10000) / 10000;

// Compute Length of Average Win
double LengthOfAverageWin = calculateLengthOfAverageWin(trade_id_cumsum, pnl_col, date_vec);
double LengthOfAverageLoss = calculateLengthOfAverageLoss(trade_id_cumsum, pnl_col, date_vec);

// Max streaks
int MaxLosingStreak = 0;
int current_losing_streak = 0;

int MaxWinningStreak = 0;
int current_winning_streak = 0;

for (int i = 0; i < pnl_col.size(); i++) {
    if (pnl_col[i] < 0) {
        current_losing_streak++;
        if (current_losing_streak > MaxLosingStreak) {
            MaxLosingStreak = current_losing_streak;
        }
    } else {
        current_losing_streak = 0;
    }
}

for (int i = 0; i < pnl_col.size(); i++) {
    if (pnl_col[i] > 0) {
        current_winning_streak++;
        if (current_winning_streak > MaxWinningStreak) {
            MaxWinningStreak = current_winning_streak;
        }
    } else {
        current_winning_streak = 0;
    }
}

// Add new metrics to the return list
return List::create(
    Named("GrossProfit") = GrossProfit,
    Named("AnnualizedProfit") = AnnualizedProfit,
    Named("NumberOfTradesPerYear") = NumberOfTradesPerYear,
    Named("ExpectedAbsoluteReturn") = ExpectedAbsoluteReturn,
    Named("PercentageOfWinningTrades") = PercentageOfWinningTrades,
    Named("LargestWin") = LargestWin,
    Named("AverageWin") = AverageWin,
    Named("LengthOfAverageWin") = LengthOfAverageWin,
    Named("AverageLoss") = AverageLoss,
    Named("LengthOfAverageLoss") = LengthOfAverageLoss,
    Named("LargestLoss") = LargestLoss,
    Named("MaxDrawdown") = MaxDrawdown,
    Named("MaxRunUp") = MaxRunUp,
    Named("MaxLosingStreak") = MaxLosingStreak,
    Named("MaxWinningStreak") = MaxWinningStreak,
    Named("CR") = CR);
}