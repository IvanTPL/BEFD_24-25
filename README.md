### Unemployment Rate Analysis
*Business, Economic and Financial Data course*

*UNIPD, Data Science Master's Degree program, 2024-2025*

---

In this project we have performed the analysis of the unemployment rate in the US time series and applied different methods and models in order to predict future values. 

We took the [Unemployment Rate Dataset](https://fred.stlouisfed.org/series/UNRATE) from the [FRED website](https://fred.stlouisfed.org/). We have also used some external data that was also taken mostly from FRED.

We considered 6 different models and model families:
- ARIMA
- Exponential Smoothing
- Prophet
- TSLM
- GAM
- Gradient Boosting

The accuracy of the predictions was calculated using MSE metric. Models were compared to each other using AIC. Among the models considered, GAM has demonstrated the best ability to model the unemployment rate given external data with MSE of 1.773305.
