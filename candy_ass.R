candy_data <- read.csv("candy_production.csv")

#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("tseries")
#install.packages("forecast")

library(ggplot2)
library(lubridate)


# ================================Data Preparation==============================
ls() # Check my data frame name

str(candy_data) # Check the structure

any(is.na(candy_data)) # Check for missing values

# Convert date column to date format
candy_data$observation_date <- as.Date(candy_data$observation_date, format = "%m/%d/%Y")

# Create the time series plot
ggplot(data = candy_data, aes(x = observation_date, y = IPG3113N)) +
  geom_line() +
  labs(x = "Date", y = "IPG3113N") +
  ggtitle("Time Series Plot of IPG3113N")

# ===================== Exploratory Data Analysis (EDA) ========================
summary(candy_data$IPG3113N)

# Examine the distribution 
hist(candy_data$IPG3113N, main = "Histogram of IPG3113N", xlab = "IPG3113N")

# Box plot
boxplot(candy_data$IPG3113N, main = "Box Plot of IPG3113N", ylab = "IPG3113N")

# Examine autocorrelation and partial autocorrelation to identify potential lag values for modeling
acf(candy_data$IPG3113N, main = "Autocorrelation Function (ACF)")
pacf(candy_data$IPG3113N, main = "Partial Autocorrelation Function (PACF)")

#library(forecast)
#ts_data <- ts(candy_data$IPG3113N, frequency = 12) # Create a time series object
#seasonplot(ts_data, year.labels = TRUE, year.labels.left = TRUE) # Create a seasonal subseries plot

# Statistical Tests using adf --> result < 0.05 means likely stationary
library(tseries)
adf.test(candy_data$IPG3113N)

# examine relationships between values at different time points
lag.plot(candy_data$IPG3113N, main = "Lag Plot")

# Perform seasonal decomposition
ts_data <- ts(candy_data$IPG3113N, frequency = 12)  # Assuming monthly data (frequency = 12)
decomposition <- decompose(ts_data)
plot(decomposition) # Plot the decomposition components (trend, seasonal, and remainder)

# ============================= Data Visualization ================================

# Visual Inspection of Trend Component
plot(decomposition$trend, main = "Trend Component", xlab = "Date", ylab = "Trend")

# Visual Inspection of Seasonal Component
plot(decomposition$seasonal, main = "Seasonal Component", xlab = "Date", ylab = "Seasonal")

# Visual Inspection of Residual Component
plot(decomposition$random, main = "Residual Component", xlab = "Date", ylab = "Residual")

# helps to find the most suitable ARIMA model for the data
library(forecast)
fit <- auto.arima(candy_data$IPG3113N)
summary(fit)
auto.arima(candy_data$IPG3113N, ic="aic", trace=TRUE)

# ============================ Analysis for seasonal ===========================
summary(decomposition$seasonal)

# Plot the autocorrelation function (ACF) of the seasonal component
acf(decomposition$seasonal, main = "ACF of Seasonal Component")

# Plot the partial autocorrelation function (PACF) of the seasonal component
pacf(decomposition$seasonal, main = "PACF of Seasonal Component")

# ============================ Analysis for trend ==============================
summary(decomposition$trend)

# check got missing value or not
any(is.na(decomposition$trend))
# remove missing value
decomposition$trend <- na.omit(decomposition$trend)

# Plot the autocorrelation function- ACF & PACF of the trend component
acf(decomposition$trend, main = "ACF of Trend Component")
pacf(decomposition$trend, main = "PACF of Trend Component")

# =========================== Analysis for Residual ============================
summary(decomposition$random)

any(is.na(decomposition$random))
decomposition$random <- na.omit(decomposition$random)

# Plot the autocorrelation function (ACF) of the residual component
acf(decomposition$random, main = "ACF of Residual Component")

# Plot the partial autocorrelation function (PACF) of the residual component
pacf(decomposition$random, main = "PACF of Residual Component")


# =========================== Fit ARIMA model ============================
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate forecasts for the next 24 months
forecast_values <- forecast(arima_model, h = 24)
print(forecast_values)

# plot the graph out
plot(forecast_values, main = "ARIMA Forecast", xlab = "Date", ylab = "IPG3113N")


# ============================= Model Evaluation ===============================
forecasted_values <- forecast_values$mean[time(ts_data)]

mae <- mean(abs(forecast_values$mean - ts_data))
mse <- mean((forecast_values$mean - ts_data)^2)
rmse <- sqrt(mse)

# Print evaluation metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")







