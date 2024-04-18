This branch encompasses forecasting experiments involving trading strategies rooted in different technical indicators and GARCH model specifications, executed across various training sample sizes and innovation distributions. The performance evaluation of these strategies, termed "Active," is compared against a benchmark buy-and-hold strategy, termed "Passive." The utilized dataset comprises univariate time series overlapping daily data.

All strategies are architected using the R6 class, enabling flexible incorporation of new strategies. The overarching design comprises:

- A parent class, **DataFetcher**, which features methods for data retrieval from Yahoo.
- A parent class, **Strategy**, housing a generic 'signal generation' method, subsequently overridden by child classes, thereby tailoring the signal generation engine to specific strategy implementations.
- The child classes of the Strategy class represent specific trading strategies (*SMA1*, *SMA1M*, *SMA2*, *SMA2M*, *BollingerBreakout*, *VolatilityMeanReversion*, *RSI*, *Random*)
- Following the signal generation phase, subsequent steps involve the provision of performance metrics and equity lines (**Strategy** class methods).
The taxonomical hierarchy of trading strategies is as follows:

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
    Univariate --> r[Random]
```

The taxonomy of assets which represent 11 unique asset classes and used in trading strategies is as follows:

```mermaid
flowchart TB
    asset[Asset] --> fx[FX]
    fx[FX] --> fx1[EUR=X]
    asset[Asset] --> eq[Equities]
    eq[Equities] --> eq1[^GSPC]
    eq[Equities] --> eq2[FTSE]
    eq[Equities] --> eq3[SPY]
    eq[Equities] --> eq4[VUG]
    eq[Equities] --> eq5[VBR]
    asset[Asset] --> fi[Fixed Income]
    fi[Fixed Income] --> fi1[AGG]
    asset[Asset] --> macro[Macro]
    macro[Macro] --> macro1[VTI]
    asset[Asset] --> cr[Credit]
    cr[Credit] --> cr1[LQD]
    asset[Asset] --> lq[Liquidity]
    lq[Liquidity] --> lq1[HYG]
    asset[Asset] --> cmd[Commodities]
    cmd[Commodities] --> cmd1[USO]
    cmd[Commodities] --> cmd2[GC=F]
    cmd[Commodities] --> cmd3[SI=F]
    asset[Asset] --> or[Operational]
    or[Operational] --> or1[HACK]
    asset[Asset] --> rates[Rates]
    rates[Rates] --> rates1[TLT]
    asset[Asset] --> inf[Inflation]
    inf[Inflation] --> inf1[TIP]
    asset[Asset] --> crypto[Cryptocurrency]
    crypto[Cryptocurrency] --> crypto1[BTC-USD]
```