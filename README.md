This branch endeavors to furnish functions for comparing the performance between a passive (buy and hold) strategy and a strategy grounded on GARCH model specifications across various training sample sizes.

The functional concept involves the creation of custom functions for:

- Estimating historical volatility
- Generating signals (utilizing a separate engine to define criteria for entry and exit points)
- Computing performance metrics encompassing returns and risk
The latter two functions are encapsulated within the main generate_combinations function.

Initially, the implementation focuses on a single instrument. However, future iterations will extend this functionality to incorporate additional instruments.

Also, branch contains strategies based on technical indicators and implemented in R6 class.
The taxonomy of all trading strategies is as follows:

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk", "nodeWidth": 1000}}}%%
flowchart LR
    Strategy[Strategy] --> Univariate[Univariate ]
    Strategy --> Multivariate[Multivariate]
    Univariate --> GARCH[GARCH]
    GARCH -->|Specification| spec[Specification: sGARCH, eGARCH, jgrGARCH, fGARCH]
    GARCH -->|Training window size| training[Training window size: 126, 252, 504]
    GARCH -->|Refit frequency| refit[Refit frequency: 21, 63, 126]
    GARCH -->|Refit window type| type[Refit window type: Moving, Expanding]
    GARCH -->|Innovations| distr[Innovations: Norm, Snorm, NIG]
    GARCH -->|Realized vol| rv[Realized vol: Close, Yang.Zhang]
    Univariate --> Tech[Technical indicators]
    Tech --> |Moving Average| ma[Moving average: simple, exp, Two moving averages, Modified moving average, Modified two moving averages]
    Tech --> Relative_Strength_Index[Relative Strength Index]
