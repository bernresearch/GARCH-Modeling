# GARCH-Modeling

## Forecasting Bitcoin daily volatility using GARCH modeling

This project aims to predict the daily volatility of Bitcoin by utilizing various GARCH models, including Standard GARCH, Exponential GARCH, and RealGARCH. To evaluate the effectiveness of these models, the Realized Variance measure from the highfrequency package is used as a baseline. The project covers the time period ranging from September 9th, 2019 to March 15th, 2023, and the necessary data is sourced from Binance's API. The models' performances are measured based on the pearson correlation and MSE, and the findings reveal that RealGARCH outperforms Standard GARCH and Exponential GARCH while the latter model is found to be the worst performer.

The project was coded in R, and the author plans to improve the models by implementing a Regime-switching algorithm in the future. This project is highly useful for investors and traders who wish to predict Bitcoin's daily volatility as it provides insights into the effectiveness of various GARCH models for forecasting. The detailed analysis of the models' performances using the Realized Variance measure as a baseline, coupled with the use of the latest data from Binance's API, makes this project a valuable resource for anyone interested in Bitcoin volatility forecasting.
