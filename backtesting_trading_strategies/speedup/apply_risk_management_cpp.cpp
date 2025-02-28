#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame apply_risk_management_cpp(
    NumericVector Date, 
    NumericVector Close,
    NumericVector High,
    NumericVector Low,
    NumericVector value,
    IntegerVector signal,
    IntegerVector position, 
    double max_risk, 
    double reward_ratio,
    double leverage, 
    double capital,
    bool flat_after_event,
    bool dynamic_limits
  ) {
  
  int n = Date.size();

  // Initialize vectors for output
  NumericVector stopLoss(n), profitTake(n), oldStopLoss(n), equity_growth_factor(n);
  LogicalVector eventSL(n), eventPT(n), eventSLShift(n);
  NumericVector nopActive(n), pnlActive(n), eqlActive(n);
  NumericVector nopPassive(n), pnlPassive(n), eqlPassive(n);
  CharacterVector pnlActiveType(n);
  LogicalVector Liquidation(n);
  IntegerVector group(n);
  group[0] = 1; // First group starts at 1
  NumericVector From(n, NA_REAL), To(n, NA_REAL); // Initialize From and To

  // Compute group values (similar to cumsum(signal != shift(signal, type = "lag", fill = 0)))
  for (int i = 1; i < n; ++i) {
      if (signal[i] != signal[i - 1]) {
          group[i] = group[i - 1] + 1; // Increment group if signal changes
      } else {
          group[i] = group[i - 1]; // Keep the same group
      }
  }
  
  // Initialize variables
  eqlActive[0] = capital;
  double eqlActiveValue = capital, eqlPassiveValue = capital;
  double eqlActiveValue2 = capital;
  // int previous_position = 0;
  double stopLossValue = NA_REAL, profitTakeValue = NA_REAL;
  bool flat = false;
  int reversed_position = NA_INTEGER;
  bool pnlActiveR = false;
  bool next_day_zero_pnl = false;
  //position[0] = 0;
  Liquidation[0] = false;
  group[0] = 0;

  // Initialize values for the first row
  nopPassive[0] = capital / Close[0] * leverage;
  pnlActive[0] = 0;
  pnlPassive[0] = 0;
  eqlActive[0] = capital;
  eqlPassive[0] = capital;
  equity_growth_factor[0] = 1;
  pnlActiveType[0] = "U";
  
  for (int i = 1; i < n; ++i) {

    if (eqlActiveValue <= 0) {
      position[i] = 0;
      Liquidation[i] = true;
    } else {
      Liquidation[i] = Liquidation[i - 1];
    }

    if (flat_after_event && flat) {
      position[i] = 0;
    }

      if (reversed_position != NA_INTEGER) {
        position[i] = reversed_position;
        reversed_position = NA_INTEGER;  // Reset after use
    }

    if (position[i] != position[i - 1] || (!pnlActiveType[i - 1].empty() && pnlActiveType[i - 1] == "R")) {

      //nopActive[i] = std::max(0.0, eqlActiveValue * leverage / Close[i]);
      nopActive[i] = (position[i] == 0) ? 0 : std::max(0.0, eqlActiveValue * leverage / Close[i]);
      eqlActiveValue2 = eqlActive[i - 1] + (Close[i] - Close[i - 1]) * position[i - 1] * nopActive[i - 1];

      if (position[i] == 1) {
        stopLossValue = Close[i] - (max_risk * eqlActiveValue2 / nopActive[i]);
        profitTakeValue = Close[i] + (reward_ratio * max_risk * eqlActiveValue2 / nopActive[i]);
      } else if (position[i] == -1) {
        stopLossValue = Close[i] + (max_risk * eqlActiveValue2 / nopActive[i]);
        profitTakeValue = Close[i] - (reward_ratio * max_risk * eqlActiveValue2 / nopActive[i]);
      } else {
        stopLossValue = profitTakeValue = NA_REAL;
      }
      
    } 
    else {
      nopActive[i] = nopActive[i - 1];
      
      if (dynamic_limits) {

        if (position[i] == 0) {
          eqlActiveValue2 = 0;
      } else {
          //eqlActiveValue2 = eqlActive[i - 1] + round((Close[i] - Close[i - 1]) * position[i - 1] * nopActive[i - 1] * 100) / 100.0;
          eqlActiveValue2 = eqlActive[i - 1] + (Close[i] - Close[i - 1]) * position[i - 1] * nopActive[i - 1];

      }
        equity_growth_factor[i] = (NumericVector::is_na(eqlActiveValue2 / eqlActive[i - 1]) || eqlActive[i - 1] <= 0) 
          ? 1 
          : eqlActiveValue2 / eqlActive[i - 1];

        oldStopLoss[i] = stopLossValue;
        
        if (position[i] == 1) {
          double new_stopLoss = std::max(stopLossValue, Close[i] - (equity_growth_factor[i] * max_risk * eqlActiveValue2 / nopActive[i]));
          eventSLShift[i] = (new_stopLoss != stopLossValue);
          stopLossValue = new_stopLoss;
          
        } else if (position[i] == -1) {
          double new_stopLoss = std::min(stopLossValue, Close[i] + (equity_growth_factor[i] * max_risk * eqlActiveValue2 / nopActive[i]));
          eventSLShift[i] = (new_stopLoss != stopLossValue);
          stopLossValue = new_stopLoss;
        }

      }
    }

    stopLoss[i] = stopLossValue;
    profitTake[i] = profitTakeValue;

    // Check for stop-loss or profit-take events
    if (position[i] == 1) {
      eventSL[i] = (!NumericVector::is_na(stopLossValue) && Close[i] <= stopLossValue) ? true : NA_LOGICAL;
      eventPT[i] = (!NumericVector::is_na(profitTakeValue) && Close[i] >= profitTakeValue) ? true : NA_LOGICAL;
    } else if (position[i] == -1) {
      eventSL[i] = (!NumericVector::is_na(stopLossValue) && Close[i] >= stopLossValue) ? true : NA_LOGICAL;
      eventPT[i] = (!NumericVector::is_na(profitTakeValue) && Close[i] <= profitTakeValue) ? true : NA_LOGICAL;
    } else {
      eventSL[i] = NA_LOGICAL;
      eventPT[i] = NA_LOGICAL;
    }

    // Reverse position logic
    if (!flat) {
      if (!LogicalVector::is_na(eventSL[i]) || !LogicalVector::is_na(eventPT[i])) {
        reversed_position = -position[i];  // Store for next period
        if (flat_after_event) {
          flat = true;
        }
      }
    }

  //   // Reset flat flag
    if (i > 2 && group[i] != group[i - 1] && 
      LogicalVector::is_na(eventSL[i]) && LogicalVector::is_na(eventPT[i])) {
    flat = false;
  }  
    
  //   Set up when a reversal happens (position flat out)
    if (position[i] == 0) {
      // If position is flat, reset PnL type to "U"
      pnlActiveType[i] = "U";
      pnlActiveR = false;  // Reset reversal flag on flat
  } else {
      // Check if a position reversal happened (1 to -1 or -1 to 1)
      if (!pnlActiveR && 
          ((position[i - 1] == 1 && position[i] == -1) || 
          (position[i - 1] == -1 && position[i] == 1))) {
          pnlActiveType[i] = "R";  // Set PnL type to "R" on reversal
          pnlActiveR = true;  // Mark that a reversal happened
      } else if (pnlActiveR) {
          // After reversal, reset to "U" for the next period
          pnlActiveType[i] = "U";
          pnlActiveR = false;  // Reset reversal flag
      } else {
          // Default to "U" when no reversal has happened
          pnlActiveType[i] = "U";
      }
  }

    // Post reversal PnL calculation
    if (next_day_zero_pnl) {
      pnlActive[i] = 0;
      next_day_zero_pnl = false;  // Reset flag
     // std::cout << "pnlActiveAfterReversal[" << i << "] = " << pnlActive[i] << std::endl;

  } else {
      if (pnlActiveType[i] == "R") {
          next_day_zero_pnl = true;  // Set flag for the next period
          nopActive[i] = nopActive[i - 1];
          pnlActive[i] = (Close[i - 1] - Close[i]) * position[i] * nopActive[i - 1];
      } else {
          pnlActive[i] = (position[i] == 0) ? 0 : round((Close[i] - Close[i - 1]) * position[i - 1] * nopActive[i - 1] * 100) / 100.0;
      }
  }  
  
    // Calculate portfolio values for Active and Passive strategies
    eqlActiveValue = std::round((eqlActiveValue + pnlActive[i]) * 100.0) / 100.0;
    eqlActive[i] = (eqlActiveValue < 0) ? 0 : eqlActiveValue;

    //equity_growth_factor[i] = (NumericVector::is_na(eqlActive[i] / eqlActive[i - 1]) || eqlActive[i - 1] <= 0) ? 1 : eqlActive[i] / eqlActive[i - 1];
    
    //nopPassive[i] = std::max(0.0, static_cast<double>(eqlPassiveValue * leverage / Close[i]));
    nopPassive[i] = std::max(0.0, (eqlPassiveValue * leverage / Close[i]));
    pnlPassive[i] = (Close[i] - Close[i - 1]) * nopPassive[i - 1];
    eqlPassiveValue = std::round((eqlPassiveValue + pnlPassive[i]) * 100.0) / 100.0;
    eqlPassive[i] = (eqlPassiveValue < 0) ? 0 : eqlPassiveValue;

  }
  
  return DataFrame::create(
    Named("Date") = Date,
    Named("Close") = Close,
    Named("High") = High,
    Named("Low") = Low,
    Named("value") = value,
    Named("signal") = signal,
    Named("position") = position,
    Named("stopLoss") = stopLoss,
    Named("oldStopLoss") = oldStopLoss,
    Named("profitTake") = profitTake,
    Named("eventSL") = eventSL,
    Named("eventSLShift") = eventSLShift,
    Named("equity_growth_factor") = equity_growth_factor,
    Named("eventPT") = eventPT,
    Named("nopActive") = nopActive,
    Named("nopPassive") = nopPassive,
    Named("pnlActive") = pnlActive,
    Named("pnlActiveType") = pnlActiveType,
    Named("eqlActive") = eqlActive,
    Named("pnlPassive") = pnlPassive,
    Named("eqlPassive") = eqlPassive,
    Named("Liquidation") = Liquidation,
    Named("From") = From,
    Named("To") = To
  );
}