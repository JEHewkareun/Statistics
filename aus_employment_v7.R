#install library
install.packages("ggplot2")
install.packages("lubridate")
install.packages("forecast")
install.packages("ggfortify")
install.packages("tseries")
install.packages("e1071")
install.packages("MASS")
install.packages("lmtest")
install.packages("caTools")

# ------------------------------ Load necessary libraries ----------------------
library(ggplot2)
library(lubridate)
library(forecast)
library(ggfortify)
library(tseries)
library(e1071)
library(MASS)
library(lmtest)
library(caTools)
library(sandwich)

# ------------------------- Step 0: Import and Split Data ----------------------
## import the data set
data <- read.csv("aus_employment.csv",stringsAsFactors = FALSE)

## Data Understanding
ls() # Check my data frame name
str(data) # Check the structure
any(is.na(data)) # Check for missing values

## format the date
data$date <- as.Date(data$date, format = "%Y-%m-%d")

## Create a time series object
data_ts <- ts(data$people_employed, frequency = 12, start = c(1978, 1))

## look the first few rows
head(data_ts)

## Plot Time Series
plot(data_ts, main="Time Series Plot of People Employed",xlab="Year", ylab = "People Employed")

## Split the data into training and testing sets
train <- window(data_ts, start = c(1978, 1), end = c(1988, 8)) # 80% for training
test <- window(data_ts, start = c(1988, 9), end = c(1991, 3)) # 20% for testing

# ------------------------- Step 1 : Visualize the Time Series -------------------------
## Perform seasonal decomposition
ts_data <- ts(data$people_employed, frequency = 12) # monthly data, 12 months a year
decomposition <- decompose(ts_data)
plot(decomposition) # Plot the decomposition components (trend, seasonal, and remainder)

## Visual Inspection of Trend Component
plot(decomposition$trend, main = "Trend Component", xlab = "Date", ylab = "Trend")

## Visual Inspection of Seasonal Component
#### Seasonal from the plot
plot(decomposition$seasonal, main = "Seasonal Component", xlab = "Date", ylab = "Seasonal")

## Visual Inspection of Residual Component
plot(decomposition$random, main = "Residual Component", xlab = "Date", ylab = "Residual")

# ------------------------- Step 2 : Transformations ---------------------------

## Decision: No need after looking at the transformed data time series plot

# ------------------------- Step 3a: Check the stationary of series -------------------------
adf.test(data$people_employed)

checktest <- kpss.test(train, null=c("Level","Trend"))
checktest	

acf(data$people_employed, main = "Autocorrelation Function (ACF)", lag.max = 40)
pacf(data$people_employed, main = "Partial Autocorrelation Function (PACF)", lag.max = 40)

# Analysis for seasonal - - - - - - - - - - - - - - - - - - - - - - - - - 
summary(decomposition$seasonal)

acf(decomposition$seasonal, main = "ACF of Seasonal Component")
pacf(decomposition$seasonal, main = "PACF of Seasonal Component")

# Analysis for trend - - - - - - - - - - - - -  - - - - - - - - - - - - 
summary(decomposition$trend)

## check got missing value or not
any(is.na(decomposition$trend))
decomposition$trend <- na.omit(decomposition$trend)## remove missing value

acf(decomposition$trend, main = "ACF of Trend Component")
pacf(decomposition$trend, main = "PACF of Trend Component")

# Analysis for Residual - - - - - - - - - - - - -  - - - - - - - - - - - -
summary(decomposition$random)

any(is.na(decomposition$random))
decomposition$random <- na.omit(decomposition$random)

acf(decomposition$random, main = "ACF of Residual Component")
pacf(decomposition$random, main = "PACF of Residual Component")

print(decomposition)

# ---------------------------- Step 3b: Differencing ---------------------------
## Check the Seasonal then do the First(Seasonal) Differencing
plot(train)
acf(train, lag.max = 40)
pacf(train, lag.max = 40)
checkresiduals(train, lag=24)

adf.test(data_ts)
# use the original data
ndiffs(data_ts)
nsdiffs(data_ts)

#seasonal differencing
z <- ts(diff(data_ts, lag=12))
ts.plot(z, gpars = list(main="First(seasonal) Differences"))
acf(z, main="z", lag.max = 40)
pacf(z, main="z", lag.max = 40)
ndiffs(z)
adf.test(z)

# non seasonal 
z2 <- ts(diff(z))
ts.plot(z2, gpars = list(main="Second(non seasonal) Differences"))
acf(z2, main="z2", lag.max = 40)
pacf(z2, main="z2", lag.max = 40)
ndiffs(z2)
adf.test(z2)

checkresiduals(z, lag=24)


# ---------------------- Step 4 : Find Optimal Parameters ----------------------

## Auto arima --> ARIMA(1,1,2)(0,1,1)[12] ) -------------------------------------
autoarima_model<- auto.arima(train, ic="aic", trace = TRUE)
summary(autoarima_model)
accuracy(autoarima_model)
checkresiduals(autoarima_model)

# Generate forecasts for the next 2.5 years (31 months)
forecast_AutoArima <- forecast(autoarima_model, h = 31)

# Plot the graph for the forecasts
plot(forecast_AutoArima, main = "Auto ARIMA Forecast", xlab = "Date", ylab = "People Employed")
print(forecast_AutoArima)

##Sarima(p,d,q)(P,D,Q) ---------------------------------------------------------
##### (p,d,q) --> non seasonal part of model ; d = 0
##### (P,D,Q) --> seasonal part of model ; D = 1
##### p is for AR model --> see PACF; q is for MA model --> see ACF
# lets see the PACF 
# arima(1,1,0)(2,1,0)[12]
sarima_model <- arima(x=train, order=c(1,1,0), seasonal = list(order = c(2,1,0), period=12))
sarima_model
summary(sarima_model)
accuracy(sarima_model)
checkresiduals(sarima_model)

# Generate forecasts for the next 2.5 years (31 months)
forecast_SARIMA <- forecast(sarima_model, h = 31)

# Plot the graph for the forecasts
plot(forecast_SARIMA, main = "ARIMA Forecast", xlab = "Date", ylab = "People Employed")
print(forecast_SARIMA)

## Holt Winter Model  ----------------------------------------------------------

## Additive Holt-winters Method 
# Fit the additive Holt-Winters model to the training data

additive_model <- HoltWinters(train,seasonal = "additive")
additive_model 

# Forecast the time series for the testing period
additive_forecast <- forecast(additive_model, h = 31)

# Print the forecast
print(additive_forecast)

# Plot the forecast values
plot(additive_forecast, main = "Additive Forecast", xlab = "Date", ylab = "People Employed")


## Multiplicative Holt-Winters Method
# Fit the multiplicative Holt-Winters model to the training data

multiplicative_model <- HoltWinters(train,seasonal = "multiplicative")
multiplicative_model

# Forecast the time series for the testing period
multiplicative_forecast <- forecast(multiplicative_model, h = 31)

# Print the forecast
print(multiplicative_forecast)

# Plot the forecast values
plot(multiplicative_forecast, main = "Multiplicative Forecast", xlab = "Date", ylab = "People Employed")


## ETS  -----------------------------------------------------------------------
ets_model <- ets(train)
ets_model
summary(ets_model)
autoplot(ets_model)
coef(ets_model)
accuracy(ets_model)
cbind('Residuals' = residuals(ets_model),
      'Forecast errors' = residuals(ets_model,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

forecast_ETS <- forecast(ets_model, h = 31)
print(forecast_ETS)
plot(forecast_ETS)

# ------------------------- Step 5 : Diagnostic Checking (Randomness) -------------------------
## Check Method residuals
residuals <- checkresiduals(additive_model)
residuals2 <- checkresiduals(multiplicative_model)
residuals3 <- checkresiduals(ets_model)          #ETS 
residuals4 <- checkresiduals(sarima_model, lag = 24) #SARIMA
residuals5 <- checkresiduals(autoarima_model, lag=24)  #auto ARIMA

# ------- Step 6 : Consider Alternative Model, Compare and Determine the Best Model  ------------
## Compare accuracy of all models
print("Accuracy of Sarima Model")
print(accuracy(forecast_SARIMA, test))

print("Accuracy of Auto Arima Model")
print(accuracy(forecast_AutoArima, test))

print("Accuracy of ETS Model")
print(accuracy(forecast_ETS, test))

print("Accuracy of Additive Model")
print(accuracy(additive_forecast, test))

print("Accuracy of Multiplicative Model")
print(accuracy(multiplicative_forecast, test))

## Compute SE
# Extract the residuals (forecast errors) from the forecast object
sarima_se <- sd(test - forecast_SARIMA$mean)
autoarima_se <- sd(test - forecast_AutoArima$mean)
ets_se <- sd(test - forecast_ETS$mean)
additive_se <- sd(test - additive_forecast$mean)
multiplicative_se <- sd(test - multiplicative_forecast$mean)

## Compute MSE
# Calculate MSE as the mean squared error
sarima_mse <- mean(sarima_se^2)
autoarima_mse <- mean(autoarima_se^2)
ets_mse <- mean(ets_se^2)
additive_mse <- mean(additive_se^2)
multiplicative_mse <- mean(multiplicative_se^2)

## Compare RMSE
### The lower the RMSE, the best performance of the model based on the accuracy
# Square root of the Mean Squared Error (MSE) to obtain RMSE
sarima_rmse <- sqrt(sarima_mse)
autoarima_rmse <- sqrt(autoarima_mse)
ets_rmse <- sqrt(ets_mse)
additive_rmse <- sqrt(additive_mse)
multiplicative_rmse <- sqrt(multiplicative_mse)

final_comparison <- data.frame(
  Model = c("Additve Holt-Winter","Multiplicative Holt-Winter", "SARIMA", "Auto ARIMA","ETS"),
  SE = c(additive_se, multiplicative_se,sarima_se,autoarima_se,ets_se),
  MSE = c(additive_mse, multiplicative_mse,sarima_mse,autoarima_mse,ets_mse),
  RMSE = c(additive_rmse, multiplicative_rmse,sarima_rmse,autoarima_rmse,ets_rmse)
)
print(final_comparison)

# Plot all forecasts and test data on the same graph
# Convert x_test to a time series object
x_test_ts <- ts(test, frequency = 12,  start = c(1988, 9))
plot(x_test_ts)

pdf("forecast_comparison_plot.pdf", width = 10, height = 6)  # Save the plot to a PDF file

# Create a vector for ylim
y_limits <- c(
  min(c(
    forecast_SARIMA$lower,
    forecast_AutoArima$lower,
    forecast_ETS$lower,
    additive_forecast$lower,
    multiplicative_forecast$lower,
    test
  )),
  max(c(
    forecast_SARIMA$upper,
    forecast_AutoArima$upper,
    forecast_ETS$upper,
    additive_forecast$upper,
    multiplicative_forecast$upper,
    test
  ))
)
plot(
  forecast_SARIMA,
  main = "Forecast: SARIMA vs. Auto_ARIMA vs. ETS vs. Holt",
  xlab = "Date",
  ylab = "Average People Employed",
  col = "blue",
  ylim = y_limits
)
lines(forecast_AutoArima$mean, col = "red")
lines(forecast_ETS$mean, col = "green")
lines(forecast_SARIMA$mean, col = "yellow")  # Sarima Forecast
lines(additive_forecast$mean, col = "brown")  # Additive Forecast model
lines(multiplicative_forecast$mean, col = "orange")  # Multiplicative Forecast model

# Plot the test data
lines(x_test_ts, col = "purple")

dev.off()  # Close the PDF device

## Best Model: ETS
# ----------------- Step 8(a) : Form Equation for the Best Model -----------------

# ----------------- Step 8(b) : Estimate the model's coefficients -----------------
coeftest(sarima_model)
coeftest(autoarima_model)
ets_model                    # ETS (best model)
additive_model 
multiplicative_model

checkresiduals(ets_model)

# ------------------------- Step 9 : Forecasting -------------------------
## Generate forecasts for the next 31 months for the best model ets
plot(forecast_values <- forecast(ets_model, h = 31))
print(forecast_values)

# ------------------------- Step 10 : Evaluate Model -------------------------

# Evaluate the ETS model using accuracy measures
accuracy_metrics <- accuracy(forecast_values, test)

# Print the accuracy metrics
print(accuracy_metrics)
