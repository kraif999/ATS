#include <Rcpp.h>
#include <cmath>
#include <numeric>  // For std::partial_sum
using namespace Rcpp;

// Helper function to extract years from Date vector
IntegerVector extract_years(NumericVector date_vec) {
  IntegerVector years(date_vec.size());
  for (int i = 0; i < date_vec.size(); i++) {
    years[i] = static_cast<int>(date_vec[i] / 10000); // Assuming date is stored as YYYYMMDD
  }
  return years;
}

// Custom lag function
IntegerVector lag_custom(IntegerVector vec, int lag_val = 1) {
    
  IntegerVector result(vec.size(), NA_INTEGER);
  for (int i = lag_val; i < vec.size(); i++) {
    result[i] = vec[i - lag_val];
  }
  return result;
}

IntegerVector rle_lengths(LogicalVector x, bool value) {
    IntegerVector lengths;
    int count = 0;
    for (int i = 0; i < x.size(); i++) {
        if (x[i] == value) {
            count++;
        } else if (count > 0) {
            lengths.push_back(count);
            count = 0;
        }
    }
    if (count > 0) lengths.push_back(count); // Add last run if still counting
    return lengths;
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
    double MaxDrawdown = round(100.0 * min(drawdown));

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

// [[Rcpp::export]]
List estimate_trading_profile_cpp(DataFrame data_subset, std::string strategy_type) {
    NumericVector date_vec = data_subset["Date"];
    IntegerVector position = data_subset["position"];
    NumericVector pnl_col = data_subset[(strategy_type == "Active") ? "pnlActive" : "pnlPassive"];
    NumericVector eql_col = data_subset[(strategy_type == "Active") ? "eqlActive" : "eqlPassive"];
    NumericVector r_col = data_subset[(strategy_type == "Active") ? "r_eqlActive" : "r_eqlPassive"];
    
    // Gross profit
    NumericVector eql_col_clean = na_omit(eql_col);
    double GrossProfit = eql_col_clean.size() > 0 ? std::round(eql_col_clean[eql_col_clean.size() - 1] - eql_col_clean[0]) : NA_REAL;
    
    // // Annualized profit
    // NumericVector r_col_clean = na_omit(r_col);
    // double annualized_return = (r_col_clean.size() > 0) ? (pow(1 + mean(r_col_clean), 252) - 1) * 100 : NA_REAL;
    // double AnnualizedProfit = std::round(annualized_return * 100) / 100; // Ensure two decimal precision
    
    // // Number of trades per year
    // IntegerVector years = extract_years(date_vec);
    // int num_years = unique(years).size();
    // int NumberOfTradesPerYear = std::round((strategy_type == "Active" ? sum(diff(position) != 0) + 1 : 1) / static_cast<double>(num_years));
    
    // Annualized profit
    NumericVector r_col_clean = na_omit(r_col);
    double mean_daily_return = (r_col_clean.size() > 0) ? mean(r_col_clean) : NA_REAL;
    double annualized_return = (!R_IsNA(mean_daily_return)) ? (pow(1 + mean_daily_return, 252) - 1) * 100 : NA_REAL;
    double AnnualizedProfit = std::round(annualized_return * 100) / 100; // Ensure two decimal precision

    // Number of trades per year
    IntegerVector years = extract_years(date_vec);
    int num_years = unique(years).size();
    int trade_changes = sum(diff(position) != 0);  // Count position changes (entries/exits)
    int NumberOfTradesPerYear = (num_years > 0) ? std::round(trade_changes / static_cast<double>(num_years)) : NA_INTEGER;


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
    double LargestWin = std::round(max(na_omit(pnl_col)));
    
    // Length of largest win
    int max_pnl_index = which_max(pnl_col);
    int trade_id_of_max = trade_id_cumsum[max_pnl_index];
    LogicalVector trade_filter = trade_id_cumsum == trade_id_of_max;
    NumericVector trade_dates = date_vec[trade_filter];
    int LengthOfLargestWin = trade_dates.size() > 0 ? std::round(max(trade_dates) - min(trade_dates) + 1) : NA_INTEGER;
    
    // Average win
    NumericVector positive_pnls = pnl_col[pnl_col > 0];
    double AverageWin = NA_REAL;
    if (positive_pnls.size() > 0) {
        // Manually calculate the mean
        double sum_positive_pnls = std::accumulate(positive_pnls.begin(), positive_pnls.end(), 0.0);
        AverageWin = std::round(sum_positive_pnls / positive_pnls.size());
    }

    // Average loss
    NumericVector negative_pnls = pnl_col[pnl_col < 0];
    double AverageLoss = NA_REAL;
    if (negative_pnls.size() > 0) {
        // Manually calculate the mean for negative values
        double sum_negative_pnls = std::accumulate(negative_pnls.begin(), negative_pnls.end(), 0.0);
        AverageLoss = std::round(sum_negative_pnls / negative_pnls.size());
    }

    // Length of Average Win
    NumericVector cumulative_pnls = cumsum(pnl_col); // Calculate cumulative pnl
    NumericVector positive_cumsum = cumulative_pnls[cumulative_pnls > 0];
    double LengthOfAverageWin = NA_REAL;
    if (positive_cumsum.size() > 0) {
        double sum_length = std::accumulate(positive_cumsum.begin(), positive_cumsum.end(), 0.0);
        LengthOfAverageWin = std::round(sum_length / positive_cumsum.size());
    }

    // Length of Average Loss
    NumericVector negative_cumsum = cumulative_pnls[cumulative_pnls < 0];
    double LengthOfAverageLoss = NA_REAL;
    if (negative_cumsum.size() > 0) {
        double sum_length_loss = std::accumulate(negative_cumsum.begin(), negative_cumsum.end(), 0.0);
        LengthOfAverageLoss = std::round(sum_length_loss / negative_cumsum.size());
    }


// Largest loss
double LargestLoss = std::round(min(na_omit(pnl_col)));  // Find the minimum pnl value

// Length of largest loss
int min_pnl_index = which_min(pnl_col);  // Find index of minimum pnl
int trade_id_of_min = trade_id_cumsum[min_pnl_index];  // Get trade_id at this index
LogicalVector trade_filter_loss = trade_id_cumsum == trade_id_of_min;  // Filter for the trade
NumericVector trade_dates_loss = date_vec[trade_filter_loss];  // Dates for the specific trade
int LengthOfLargestLoss = trade_dates_loss.size() > 0 ? std::round(max(trade_dates_loss) - min(trade_dates_loss) + 1) : NA_INTEGER;  // Calculate the length of the loss

// Winning runs
LogicalVector is_winning = pnl_col > 0;
IntegerVector winning_run_lengths = rle_lengths(is_winning, true);
NumericVector winning_runs = na_omit(as<NumericVector>(winning_run_lengths));

// 12. Average Winning Run
double AverageWinningRun = winning_runs.size() > 0 ? round(mean(winning_runs)) : NA_REAL;

// 13. Largest Winning Run
double LargestWinningRun = winning_runs.size() > 0 ? max(winning_runs) : NA_REAL;

// 14. Length of Time in Largest Winning Run
int largest_run_index = which_max(winning_runs);
int largest_run_start = sum(head(winning_run_lengths, largest_run_index)) + 1;
int largest_run_end = largest_run_start + LargestWinningRun - 1;
double LengthOfTimeInLargestWinningRun = largest_run_start > 0 && largest_run_end > 0
    ? round(max(date_vec[Range(largest_run_start - 1, largest_run_end - 1)]) - 
            min(date_vec[Range(largest_run_start - 1, largest_run_end - 1)]) + 1)
    : NA_REAL;

// 15. Length of Time in Average Winning Run
NumericVector winning_run_lengths_time(winning_runs.size());
for (int i = 0; i < winning_runs.size(); i++) {
    int start = sum(head(winning_run_lengths, i)) + 1;
    int end = start + winning_runs[i] - 1;
    winning_run_lengths_time[i] = end > start 
        ? round(max(date_vec[Range(start - 1, end - 1)]) - min(date_vec[Range(start - 1, end - 1)]) + 1) 
        : NA_REAL;
}
double LengthOfTimeInAverageWinningRun = winning_run_lengths_time.size() > 0 
    ? round(mean(na_omit(winning_run_lengths_time))) 
    : NA_REAL;

// Losing runs
LogicalVector is_losing = pnl_col < 0;
IntegerVector losing_run_lengths = rle_lengths(is_losing, true);
NumericVector losing_runs = na_omit(as<NumericVector>(losing_run_lengths));

// 16. Average Losing Run
double AverageLosingRun = losing_runs.size() > 0 ? round(mean(losing_runs)) : NA_REAL;

// 17. Length of Time in Average Losing Run
NumericVector losing_run_lengths_time(losing_runs.size());
for (int i = 0; i < losing_runs.size(); i++) {
    int start = sum(head(losing_run_lengths, i)) + 1;
    int end = start + losing_runs[i] - 1;
    losing_run_lengths_time[i] = end > start 
        ? round(max(date_vec[Range(start - 1, end - 1)]) - min(date_vec[Range(start - 1, end - 1)]) + 1) 
        : NA_REAL;
}
double LengthOfTimeInAverageLosingRun = losing_run_lengths_time.size() > 0 
    ? round(mean(na_omit(losing_run_lengths_time))) 
    : NA_REAL;

// 18. Largest Losing Run
double LargestLosingRun = losing_runs.size() > 0 ? max(losing_runs) : NA_REAL;

// 19. Length of Time in Largest Losing Run
int largest_loss_run_index = which_max(losing_runs);
int largest_loss_run_start = sum(head(losing_run_lengths, largest_loss_run_index)) + 1;
int largest_loss_run_end = largest_loss_run_start + LargestLosingRun - 1;
double LengthOfLargestLosingRun = largest_loss_run_start > 0 && largest_loss_run_end > 0
    ? round(max(date_vec[Range(largest_loss_run_start - 1, largest_loss_run_end - 1)]) - 
            min(date_vec[Range(largest_loss_run_start - 1, largest_loss_run_end - 1)]) + 1)
    : 0;
    
// Compute Max Drawdown
List drawdownResults = calculateDrawdown(eql_col, date_vec);
double MaxDrawdown = drawdownResults["MaxDrawdown"];
double StartDateMaxDrawdown = drawdownResults["StartDateMaxDrawdown"];
double EndDateMaxDrawdown = drawdownResults["EndDateMaxDrawdown"];
int LengthOfMaxDrawdown = drawdownResults["LengthOfMaxDrawdown"];

// Compute Max Run-Up
List maxRunUpResults = calculateMaxRunUp(eql_col, date_vec);
double MaxRunUp = maxRunUpResults["MaxRunUp"];
double StartDateMaxRunUp = maxRunUpResults["StartDateMaxRunUp"];
double EndDateMaxRunUp = maxRunUpResults["EndDateMaxRunUp"];
int LengthOfMaxRunUp = maxRunUpResults["LengthOfMaxRunUp"];

double ExpectedAbsoluteReturn = (PercentageOfWinningTrades / 100.0) * (AverageWin + AverageLoss);

// Add new metrics to the return list
return List::create(Named("GrossProfit") = GrossProfit,
                    Named("AnnualizedProfit") = AnnualizedProfit,
                    Named("NumberOfTradesPerYear") = NumberOfTradesPerYear,
                    Named("ExpectedAbsoluteReturn") = ExpectedAbsoluteReturn,
                    Named("PercentageOfWinningTrades") = PercentageOfWinningTrades,
                    Named("LargestWin") = LargestWin,
                    Named("LengthOfLargestWin") = LengthOfLargestWin,
                    Named("AverageWin") = AverageWin,
                    Named("AverageLoss") = AverageLoss,
                    Named("LengthOfAverageWin") = LengthOfAverageWin,
                    Named("LengthOfAverageLoss") = LengthOfAverageLoss,
                    Named("LargestLoss") = LargestLoss,
                    Named("LengthOfLargestLoss") = LengthOfLargestLoss,
                    Named("AverageWinningRun") = AverageWinningRun,
                    Named("LargestWinningRun") = LargestWinningRun,
                    Named("LengthOfTimeInLargestWinningRun") = LengthOfTimeInLargestWinningRun,
                    Named("LengthOfTimeInAverageWinningRun") = LengthOfTimeInAverageWinningRun,
                    Named("AverageLosingRun") = AverageLosingRun,
                    Named("LargestLosingRun") = LargestLosingRun,
                    Named("LengthOfLargestLosingRun") = LengthOfLargestLosingRun,
                    Named("LengthOfTimeInAverageLosingRun") = LengthOfTimeInAverageLosingRun,
                    Named("MaxDrawdown") = MaxDrawdown,
                    Named("StartDateMaxDrawdown") = StartDateMaxDrawdown,
                    Named("EndDateMaxDrawdown") = EndDateMaxDrawdown,
                    Named("LengthOfMaxDrawdown") = LengthOfMaxDrawdown,
                    Named("MaxRunUp") = MaxRunUp,
                    Named("StartDateMaxRunUp") = StartDateMaxRunUp,
                    Named("EndDateMaxRunUp") = EndDateMaxRunUp,
                    Named("LengthOfMaxRunUp") = LengthOfMaxRunUp);
}