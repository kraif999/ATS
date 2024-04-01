This branch endeavors to furnish functions for comparing the performance between a passive (buy and hold) strategy and a strategy grounded on GARCH model specifications across various training sample sizes.

The functional concept involves the creation of custom functions for:

- Estimating historical volatility
- Generating signals (utilizing a separate engine to define criteria for entry and exit points)
- Computing performance metrics encompassing returns and risk
The latter two functions are encapsulated within the main generate_combinations function.

Initially, the implementation focuses on a single instrument. However, future iterations will extend this functionality to incorporate additional instruments.