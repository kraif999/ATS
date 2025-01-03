This branch contains experiments involving the implementation of a wide range of trading strategies across various asset classes. 
The performance of these strategies, referred to as 'Active,' is compared against a benchmark buy-and-hold strategy, referred to as 'Passive'.
Additionally, there is TSA class which analyses time series patterns and various characteristics.
Also,the estimation of a strategy's trading profile has been introduced (see the example in estimate_trading_profile.R). 
Its purpose is to assess the overall risk profile of a strategy (macroscopic level) and provide a detailed list of trades (microscopic level) across multiple markets and time periods (including the choice of in-sample or out-of-sample data split) under varying market conditions. Furthermore, the framework supports the application of stop-loss and profit-taking mechanisms, enabling users to manage risk and lock in profits.

All strategies are architected using the R6 class system, which provides a modular and flexible framework for incorporating new features or strategies.

The design is structured as follows::

- A parent class, **DataFetcher**, which features methods for overlapping daily data retrieval from Yahoo.
- A parent class, **Strategy**, contains a generic 'signal generation' method, subsequently overridden by child classes, thereby tailoring the signal generation engine to specific strategy implementations.
- The child classes of the Strategy class represent specific trading strategies (based on *Technical  Indicators*, statistical models (*GARCH*, *ARIMA*), or other approaches (*AlphaEngine: coastline counter-trend trading*).
- Following the signal generation phase, next steps involve the provision of performance metrics and equity lines (**Strategy** class methods).
Assets used represent the following classes: *FX*, *Equities*, *Commodities*, *Cryptocurrencies*, *Fixed Income*, *Macro*.

Across 18 strategies around **~37,000** parameters combinations were tested, refer to **'Run_backtest_results'** folder.

The taxonomy of trading strategies implemented is as follows:

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk", "nodeWidth": 1000}}}%%
flowchart LR
    Strategy[Strategy] --> Univariate[Univariate]
    Strategy --> Multivariate[Multivariate]
    Multivariate --> alphaportfolio[FX portfolio based on AlphaEngine]
    Univariate --> Stats[Stats]
    Stats[Statistical models] --> GARCH
    Stats[Statistical models] --> ARIMA
    Univariate --> Tech[Technical indicators]
    Tech --> ma[Moving Averages: different types]
    ma --> sma1[SMA1]
    ma --> sma2[SMA2]
    ma --> sma1m[SMA1 Modified]
    ma --> sma2m[SMA2 Modified]
    ma --> macd[MACD]
    ma --> tt[Turtle Strategy]
    ma --> dc[Donchian Channels]
    ma --> sra[Stop and Reversal 'parabolic SAR']
    ma --> adx[Average Directional Index 'ADX']
    Tech --> rsi[Relative Strength Index]
    Tech --> bb[Bollinger Bounds]
    Tech --> vol[Volatility Mean Reversion]
    Univariate --> other[Trading]
    other --> alpha1[AlphaEngine: counter-trend trading]
```

See below example of classes design.

```mermaid
classDiagram

   class DataFetcher {
        + symbol
        + from_date
        + to_date
        + type
        + initialize(symbol, from_date, to_date, type): Initializes the DataFetcher object.
        + convert_xts_to_wide_df(): Downloads data for multiple symbols and saves tibble data frame in wide format.
        + download_xts_data(): Downloads xts data and computes log returns.
        + plot_close_or_rets(type): Visualizes Close price or returns.
        + compute_NA_close_price_ratio(): Computes missing ratio of values that are not available for Close price.
    }
    class Strategy {
        + data
        + initialize(data) : Initializes the Strategy object with provided data.
        + generate_signals() : Signal generation, specific to each subclass.
        + convert_to_tibble(ts) : Converts time series data to a tibble format.
        + estimate_performance() : Estimates performance for Active and Passive strategies.
        + calculate_cumulative_return() : Calculates cumulative return (to be removed).
        + plot_equity_lines(strategy_name, signal_flag) : Visualizes equity lines for active strategy and passive (buy and hold).
    }

    class AlphaEngine {
        +threshold
        +profit_taking
        +signal_generation
        +position_sizing
        +vol_position_sizing
        +initialize(data, threshold, profit_taking, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE)
        +generate_signals()
        +plot_events(symbol)
        +plot_dc(symbol)
        +plotSignals(data)
        +run_backtest(symbols, thresholds, profit_takings, signal_generations, position_sizings, vol_position_sizings, from_date, to_date, output_df = TRUE)
    }
    class AlphaEnginePrivate {
        +identify_events(data, threshold)
        +estimate_prob_surprise(data)
        +estimate_entropies(data)
        +calculate_sequence_lengths(h)
        +calculate_OS_length(data)
        +generateExitSignals(df, signal_generation = "TH")
    }
    AlphaEngine -- AlphaEnginePrivate

    class AlphaEngineMult {
        +estimate_rolling_correlations(symbol_list, from_date, to_date)
        +plot_rolling_correlations(data, plot_flag = TRUE)
        +estimate_portfolio_performance(cp, symbol_list, capital, leverage)
        +plot_portfolio_components(df, type)
    }
    class AlphaEngineMultPrivate {
        +split_data(cp, symbol_list)
        +process_data(df)
        +compute_performance_metrics(data)
        +estimate_performance_bucket(data)
    }
    AlphaEngineMult -- AlphaEngineMultPrivate

    class TSA {
        original_data: any = NULL
        data: any = NULL
        +initialize(data)
        +estimate_stationarity(freq = "daily", plot_flag = TRUE)
        +estimate_autocorr_rets_vol(test, freq = "daily", plot_flag = TRUE)
        +estimate_seasonality(freq = "daily")
        +estimate_heteroscedasticity(freq = "daily", plot_flag = TRUE)
        +estimate_arch_effects(freq = "daily", p = 1, q = 1, plot_flag = TRUE)
        +estimate_outliers(test, freq = "daily", plot_flag = TRUE, q1 = NULL, q3 = NULL, threshold = 1.5)
        +compute_wkd_rets(freq = "daily")
        +compute_summary_statistics(freq = "daily")
    }
    class TSAPrivate {
        +preprocess_data(freq)
    }
    TSA -- TSAPrivate

    class GARCHbasedStrategy {
        + specification
        + n_start
        + refit_every
        + refit_window
        + distribution_model
        + realized_vol
        + cluster
        + initialize(data, specification, n_start, refit_every, refit_window, distribution_model, realized_vol, cluster) : Initializes the GARCHbasedStrategy object.
        + estimate_realized_volatility(data) : Estimates realized volatility by different approaches.
        + generate_signals() : Specifies signal criteria based on GARCH model volatility forecasts.
        + run_backtest(symbols, specifications, n_starts, refits_every, refit_windows, distribution_models, realized_vols, output_df) : Runs a backtest for GARCH-based strategy.
    }


    Strategy --|> AlphaEngine
    Strategy --|> GARCHbasedStrategy
    AlphaEngine --|> AlphaEngineMult
```

Below is an example of Bitcoin's performance based on the **SMA strategy** with a Hull Moving Average (100-day window) which is superior than the passive (buy-and-hold) strategy. Stop-loss and take-profit limits are set at 1.5% and 37.5% of the closing price throughout the entire trading period.

**Key Visualization:**  

![BTC USD Plot](sma1_btc_usd_plot.png)

**Trading Profile:**  

From **2020-02-12** to **2024-01-01**, the estimated *"in-sample"* trading profile is as follows:


| Metric                          | V1           | V2           | Units  |
|---------------------------------|--------------|--------------|--------|
| ticker                          | BTC-USD      | BTC-USD      |        |
| from                            | 2020-04-21   | 2020-04-21   |        |
| to                              | 2024-01-01   | 2024-01-01   |        |
| data_type                       | in_sample    | in_sample    |        |
| Strategy                        | Active       | Passive      |        |
| AnnualizedProfit                | 54.687       | 40.969       | %      |
| NumberOfTradesPerYear           | 32           | 0            |        |
| PercentageOfWinningTrades       | 30           | NotApplicable| %      |
| LargestWin                      | 7554         | 7293         | USD    |
| LengthOfLargestWin              | 13           | 13           | days   |
| AverageWin                      | 902          | 740          | USD    |
| LengthOfAverageWin              | 13           | 9            | days   |
| LargestLoss                     | -5822        | -7554        | USD    |
| LengthOfLargestLoss             | 6            | 13           | days   |
| AverageLoss                     | -710         | -724         | USD    |
| LengthOfAverageLoss             | 3            | 8            | days   |
| AverageWinningRun               | 1.933        | 1.920        | USD    |
| LargestWinningRun               | 9            | 10           | days   |
| LengthOfTimeInLargestWinningRun | 9            | 10           | days   |
| LengthOfTimeInAverageWinningRun | 2            | 2            | days   |
| AverageLosingRun                | 2            | 1            | USD    |
| LengthOfTimeInAverageLosingRun  | 4            | 2            | days   |
| LargestLosingRun                | 55           | 10           | days   |
| LengthOfTimeInLargestLosingRun  | 55           | 10           | days   |
| MaxDrawdown                     | -33.23838    | -75.80602    | %      |
| StartDateMaxDrawdown            | 2022-06-18   | 2021-11-08   | Date   |
| EndDateMaxDrawdown              | 2023-03-15   | 2022-11-09   | Date   |
| LengthOfMaxDrawdown             | 270          | 366          | days   |
| MaxRunUp                        | 1139.8780    | 842.5893     | %      |
| StartDateMaxRunUp               | 2020-04-21   | 2020-04-21   | Date   |
| EndDateMaxRunUp                 | 2023-12-08   | 2021-11-08   | Date   |
| LengthOfMaxRunUp                | 1326         | 566          | days   |