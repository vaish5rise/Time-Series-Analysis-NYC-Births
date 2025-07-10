##########NO. of Birth per month in new york city 1946-1959#

# Load the data from the URL
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

plot.ts(birthstimeseries)

###One method of establishing the underlying trend (smoothing out peaks and troughs)
#in a set of data is using the moving averages technique##

# Calculate the 12-month moving average
moving_average <- filter(birthstimeseries, filter= c(1/24, 1/12, 1/8, 1/6, 1/4, 1/3, 1/4, 1/6, 1/8, 1/12, 1/24), sides=2)

# Handle NA values introduced by the filter
moving_average[is.na(moving_average)] <- NA

# Plot the original time series
plot.ts(birthstimeseries, main="Number of Births per Month in NYC (1946-1959) with 12-Month Moving Average", ylab="Number of Births", xlab="Year", col="blue")

# Add the moving average to the plot
lines(moving_average, col="red")

# Add a legend
legend("topright", legend=c("Original Data", "12-Month Moving Average"), col=c("blue", "red"), lty=1)

## Fit a linear model to the time series
time_index <- time(birthstimeseries)
linear_model <- lm(birthstimeseries ~ time_index)

# Get the fitted values (trend component)
trend <- fitted(linear_model)


# Detrend the time series by subtracting the trend component
detrended_series <- birthstimeseries - trend

# Plot the original time series
plot.ts(birthstimeseries, main="Original and Detrended Time Series", ylab="Number of Births", xlab="Year", col="blue")
lines(trend, col="red")

# Plot the detrended series
plot.ts(detrended_series, main="Detrended Time Series", ylab="Detrended Number of Births", xlab="Year", col="green")

# Add legends to both plots
legend("topright", legend=c("Original Data", "Trend"), col=c("blue", "red"), lty=1)
legend("topright", legend=c("Detrended Series"), col=c("green"), lty=1)

#Now remove seasonality

# Calculate the mean for each month across all years
monthly_means <- tapply(birthstimeseries, cycle(birthstimeseries), mean)
monthly_means

# Replicate the monthly means to match the length of the original time series
monthly_means_rep <- rep(monthly_means, length(birthstimeseries) / 12)
monthly_means_rep

# Subtract the monthly means from the original time series to get the deseasonalized series
deseasonalized_series <- birthstimeseries - monthly_means_rep
deseasonalized_series

# Plot the original time series
plot.ts(birthstimeseries, main="Original and Deseasonalized Time Series", ylab="Number of Births", xlab="Year", col="blue")

# Plot the deseasonalized series
lines(deseasonalized_series, col="red")

# Add a legend
legend("topright", legend=c("Original Data", "Deseasonalized Series"), col=c("blue", "red"), lty=1)

# Fit a linear model to the deseasonalized series to estimate the trend
time_index <- time(deseasonalized_series)
trend_model <- lm(deseasonalized_series ~ time_index)

# Get the fitted values (trend component)
trend_component <- fitted(trend_model)

# Subtract the trend component to get the residuals
residuals <- deseasonalized_series - trend_component
residuals

# Plot the trend component on the deseasonalized series plot
plot.ts(deseasonalized_series, main="Deseasonalized Series and Trend Component", ylab="Deseasonalized Number of Births", xlab="Year", col="red")
lines(trend_component, col="blue")
legend("topright", legend=c("Deseasonalized Series", "Trend Component"), col=c("red", "blue"), lty=1)

# Plot the residuals
plot.ts(residuals, main="Residuals (Deseasonalized Series - Trend)", ylab="Residuals", xlab="Year", col="green")

# Analyze the residuals
acf(residuals, main="ACF of Residuals")
pacf(residuals, main="PACF of Residuals")
hist(residuals)
boxplot(residuals)

# Add a legend
legend("topright", legend=c("Residuals"), col=c("green"), lty=1)



#NOW APPLY LOG TRANSFORMATION METHOD


# Apply the log transformation to stabilize variance
log_birthstimeseries <- log(birthstimeseries)
log_birthstimeseries

# Calculate the mean for each month across all years
monthly_means_log <- tapply(log_birthstimeseries, cycle(log_birthstimeseries), mean)

# Replicate the monthly means to match the length of the original time series
monthly_means_rep_log <- rep(monthly_means_log, length(log_birthstimeseries) / 12)
monthly_means_rep_log

# Subtract the monthly means from the log-transformed time series to get the deseasonalized series
deseasonalized_log_series <- log_birthstimeseries - monthly_means_rep_log
deseasonalized_log_series

# Fit a linear model to the deseasonalized series to estimate the trend
time_index <- time(deseasonalized_log_series)
trend_model_log <- lm(deseasonalized_log_series ~ time_index)

# Get the fitted values (trend component)
trend_component_log <- fitted(trend_model_log)

# Subtract the trend component to get the residuals
residuals_log <- deseasonalized_log_series - trend_component_log
residuals_log 

# Plot the original log-transformed time series
plot.ts(log_birthstimeseries, main="Original and Deseasonalized Log-transformed Time Series", ylab="Log(Number of Births)", xlab="Year", col="blue")

# Plot the deseasonalized log-transformed series
lines(deseasonalized_log_series, col="red")

# Add a legend
legend("topright", legend=c("Original Log-transformed Data", "Deseasonalized Series"), col=c("blue", "red"), lty=1)

# Plot the deseasonalized series with the trend component
plot.ts(deseasonalized_log_series, main="Deseasonalized Log-transformed Series and Trend Component", ylab="Deseasonalized Log(Number of Births)", xlab="Year", col="red")
lines(trend_component_log, col="blue")
legend("topright", legend=c("Deseasonalized Series", "Trend Component"), col=c("red", "blue"), lty=1)

# Plot the residuals
plot.ts(residuals_log, main="Residuals (Deseasonalized Log-transformed Series - Trend)", ylab="Residuals", xlab="Year", col="green")

# Analyze the residuals
acf(residuals_log, main="ACF of Residuals")
pacf(residuals_log, main="PACF of Residuals")
hist(residuals_log)
boxplot(residuals_log)
# Add a legend
legend("topright", legend=c("Residuals"), col=c("green"), lty=1)


#Load the necessary library
library(forecast)

# Automatically select the best ARIMA model 
best_arima_model <- auto.arima(log_birthstimeseries)

# Display the summary of the best ARIMA model
summary(best_arima_model)

# Forecast using the best ARIMA model for the next 24 months
arima_forecast <- forecast(best_arima_model, h=24)

# Plot the original log-transformed series and the forecast
plot(arima_forecast, main="ARIMA Forecast with Log-transformed Data", ylab="Log(Number of Births)", xlab="Year")

# Add the original data to the plot
lines(log_birthstimeseries, col="blue")

# Add a legend
legend("topright", legend=c("Original Series", "Forecast"), col=c("blue", "red"), lty=1)

#Analyze
ts.plot(best_arima_model$residuals)

acf(best_arima_model$residuals)
pacf(best_arima_model$residuals)
pacf(best_arima_model$residuals,40)
acf(best_arima_model$residuals,40)
hist(best_arima_model$residuals)
qqnorm(best_arima_model$residuals)

