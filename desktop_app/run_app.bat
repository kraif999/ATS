@echo off
cd /d "%~dp0\backtesting_trading_strategies_shinyapp"
"Rscript.exe" app.R
pause