# 📈 Time Series Analysis: Monthly Births in NYC (1946–1959)

This project performs a comprehensive time series analysis on the number of births per month in New York City from January 1946 to December 1959. It includes decomposition, transformation, and forecasting using statistical techniques in **R**.

---

## 📊 Dataset

- Source: Rob J. Hyndman's Time Series Data Library  
- Data: Monthly number of births in NYC (1946–1959)

---


---

## ⚙️ Techniques Applied

- **Smoothing & Trend Estimation**: Moving averages
- **Deseasonalization**: Monthly means method
- **Transformation**: Log transformation for variance stabilization
- **Modeling**: ARIMA using `auto.arima()` from `forecast` package
- **Residual Analysis**: ACF, PACF, histograms, Q-Q plots
- **Forecasting**: Next 24 months prediction with confidence intervals

---

## 🛠 R Packages Used

```r
library(forecast)
library(tseries)
library(stats)
