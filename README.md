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
flowchart TD
    A[Strategy] --> B[Univariate]
    A[Strategy] --> mult[Multivariate]
    B --> C[GARCH]
    C --> spec[Specification: sGARCH, eGARCH, jgrGARCH, fGARCH]
    C --> sample[Training window size: 126, 252, 504]
    C --> refit[Refit frequency: 21, 63, 126]
    C --> type[Refit window type: moving, expanding]
    C --> distr[Innovations: norm, snorm, nig]
    C --> hist[Realized vol: close, yang.zhang]
    B --> D[Technical indicators]
    D --> E[Moving average:simple, exp]
    E --> sma1[Moving average]
    E --> sma2[Two moving averages]
    E --> sma1m[Modified moving average]
    E --> sma2m[Modified two moving averages]
    D --> rsi[Relative Strength Index]