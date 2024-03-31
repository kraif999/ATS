This branch aims: 
- to compare the performance of trading strategies (VIX futures contracts investing) built on various GARCH model specifications;
The approach is as follows:
- one day ahead volatility of S&P500 return is forecsated (FV) based on various GARCH model specifications and data sample sizes;
- historical volatility (HV) is estimated by six volatilty measures from TTR package;
- signal generation is as follows: if volatilty forecast (FV) is higher than historical volatility (HV), then VIX futures contract is bought, if lower - sold (in case two or more consequitive correct predictions signal is carried forward);
- the best performing GARCH specificatons and sample sizes are as follows (transactional costs are included):
-- 
--

