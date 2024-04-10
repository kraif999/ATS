This branch encompasses forecasting experiments involving trading strategies rooted in different technical indicators and GARCH model specifications, executed across various training sample sizes and innovation distributions. The performance evaluation of these strategies, termed "Active," is compared against a benchmark buy-and-hold strategy, termed "Passive." The utilized dataset comprises univariate time series daily data.

All strategies are architected using the R6 class, enabling flexible incorporation of new strategies. The overarching design comprises:

- A parent class, 'DataFetcher,' which features methods for data retrieval from Yahoo.
- A parent class, 'Strategy,' housing a generic 'signal generation' method, subsequently overridden by child classes, thereby tailoring the signal generation engine to specific strategy implementations.
- The child classes of the Strategy represent specific trading strategies.
- Following the signal generation phase, subsequent steps involve the provision of performance metrics and equity lines.
The taxonomical hierarchy of trading strategies is portrayed as follows:

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk", "nodeWidth": 1000}}}%%
flowchart LR
    Strategy[Strategy] --> Univariate[Univariate]
    Strategy --> Multivariate[Multivariate]
    Univariate --> GARCH[GARCH]
    GARCH -->|Specification| spec[sGARCH, eGARCH, jgrGARCH, fGARCH]
    GARCH -->|Training window size| training[126, 252, 504]
    GARCH -->|Refit frequency| refit[21, 63, 126]
    GARCH -->|Refit window type| type[Moving, Expanding]
    GARCH -->|Innovations| distr[Norm, Snorm, NIG]
    GARCH -->|Realized vol| rv[Close, Yang.Zhang]
    Univariate --> Tech[Technical indicators]
    Tech --> ma[Moving Averages: simple and exponential]
    ma --> SMA1
    ma --> SMA2
    ma --> SMA1Modified
    ma --> SMA2Modified
    Tech --> rsi[Relative Strength Index]
    Tech --> bb[Bollinger Bounds]
    Tech --> vol[Volatility Mean Reversion]
