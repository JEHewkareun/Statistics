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

# ------------------------- Step 0: Import and Split Data ----------------------
## import the data set
data <- read.csv("aus_employment.csv",stringsAsFactors = FALSE)

### Data Understanding ###
ls() # Check my data frame name

str(data) # Check the structure

any(is.na(data)) # Check for missing values

## format the date
data$date <- as.Date(data$date, format = "%m/%d/%Y")

# Create a time series object
data_ts <- ts(data$people_employed, frequency = 12, start = c(1978, 1))

## look the first few rows
head(data_ts)

## Plot Time Series
plot(data_ts, main="Time Series Plot of X.Passengers",xlab="Year", ylab = "X.Passengers")

## Split the data into training and test sets
##### Use the train to do the model then the end use to compare
train <- window(data_ts, start = c(1978, 1), end = c(1988, 12)) # 80% for training
test <- window(data_ts, start = c(1989, 1), end = c(1991, 3)) # 20% for testing

# ------------------------- Step 1 : Visualize the Time Series -------------------------
## Perform seasonal decomposition
ts_data <- ts(data$people_employed, frequency = 12)  # Assuming monthly data (frequency = 12)
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

## CHECK NEED TO DO TRANFORMATION OR NOT  
## check skewness first
skew <- skewness(data$people_employed)
print(skew)

##### Output:  -0.149 indicates that the data is slightly left-skewed. 
##### (-0.149) suggests that the skewness is relatively small, which means the departure from a perfectly symmetric distribution is not severe

## Q-Q plot closely follow the line of equality--> suggests that the dataset is approximately normally distributed
qqnorm(data$people_employed)
qqline(data$people_employed)

## Shapiro-Wilk test --> whether a dataset follows a normal distribution
shapiro.test(data$people_employed)

##### Output: W-statistic is close to 1, which suggests that the data is relatively close to a normal distribution.
##### p-value (p < 0.0001) --> does not follow a normal distribution --> need to do transformation

## DO THE TRANSFORMATION 
original_data <- data$people_employed

## Find the optimal lambda (λ) for Box-Cox transformation
optimal_lambda <- optimize(function(lambda) -sum(log(abs((original_data^lambda - 1) / lambda))), c(-2, 2))$minimum
cat("Optimal lambda (λ) =", optimal_lambda, "\n")

##### Output: Optimal lambda (λ) = 1.99994 

## Transform the data using the optimal lambda
transformed_data <- if (abs(optimal_lambda) > 0.001) {
  (original_data^optimal_lambda - 1) / optimal_lambda
} else {
  log(original_data)
}

## Assign the transformed data back to the dataset
data$people_employed <- transformed_data

## Plot the original and transformed data
par(mfrow = c(1, 2))  # Create a 1x2 grid for side-by-side plots
hist(original_data, main = "Original Data", xlab = "Value", col = "violet")
hist(transformed_data, main = "Transformed Data", xlab = "Transformed Value", col = "pink")
par(mfrow = c(1, 1))  # Reset plotting parameters

ggplot(data = NULL, aes(x = time(data_ts), y = data_ts)) +
  geom_line() +
  labs(x = "Date", y = "X.Passengers") +
  ggtitle("Time Series Plot of X.Passengers")

ggplot(data = NULL, aes(x = time(transformed_data), y = data_ts)) +
  geom_line() +
  labs(x = "Date", y = "X.Passengers") +
  ggtitle("Time Series Plot of X.Passengers")

# ------------------------- Step 3: Check the stationary of series -------------------------
## Examine autocorrelation and partial autocorrelation to identify potential lag values for modeling
acf(data$people_employed, main = "Autocorrelation Function (ACF)", lag.max = 40)
pacf(data$people_employed, main = "Partial Autocorrelation Function (PACF)", lag.max = 40)

## Statistical Tests using adf --> result < 0.05 means stationary
adf.test(data$people_employed)

## examine relationships between values at different time points
lag.plot(data$people_employed, main = "Lag Plot") # using the original data

## Decompose again 
ts_data2 <- ts(data$people_employed, frequency = 12)  # Assuming monthly data (frequency = 12)
decomposition2 <- decompose(ts_data2)
plot(decomposition2) 

checktest <- kpss.test(train, null=c("Level","Trend"))
checktest

# Analysis for seasonal - - - - - - - - - - - - - - - - - - - - - - - - - 
summary(decomposition2$seasonal)

## Plot the autocorrelation function (ACF) of the seasonal component
acf(decomposition2$seasonal, main = "ACF of Seasonal Component")

## Plot the partial autocorrelation function (PACF) of the seasonal component
pacf(decomposition2$seasonal, main = "PACF of Seasonal Component")

# Analysis for trend - - - - - - - - - - - - -  - - - - - - - - - - - - 
summary(decomposition2$trend)

## check got missing value or not
any(is.na(decomposition2$trend))

## remove missing value
decomposition2$trend <- na.omit(decomposition2$trend)

## Plot the autocorrelation function- ACF & PACF of the trend component
acf(decomposition2$trend, main = "ACF of Trend Component")
pacf(decomposition2$trend, main = "PACF of Trend Component")

# Analysis for Residual - - - - - - - - - - - - -  - - - - - - - - - - - -
summary(decomposition2$random)

any(is.na(decomposition2$random))
decomposition2$random <- na.omit(decomposition2$random)

## Plot the autocorrelation function- ACF & PACF of the residual component
acf(decomposition2$random, main = "ACF of Residual Component")
pacf(decomposition2$random, main = "PACF of Residual Component")

print(decomposition2)

acf(data$people_employed, main = "Autocorrelation Function (ACF)")
pacf(data$people_employed, main = "Autocorrelation Function (PACF)")

# ------------------------- Step 4 : Find Optimal Parameters -------------------------
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


##Guess Arima(p,d,q)(P,D,Q)
##### (p,d,q) --> non seasonal part of model ; d = 0
##### (P,D,Q) --> seasonal part of model ; D = 1
##### p is for AR model --> see PACF; q is for MA model --> see ACF
# lets see the PACF 
# arima(1,0,0)(1, 0, 0)[12]
fit <- arima(x=train, order=c(1,1,0), seasonal = list(order = c(0,1,0), period=12))
fit
summary(fit)

## Diagnostic checking 
checkresiduals(fit, lag = 24)

# auto arima --> ARIMA(1,1,2)(0,1,1)[12] 
fit2<- auto.arima(train, ic="aic", trace = TRUE)
summary(fit2)
auto.arima(train, ic="aic", trace=TRUE)
checkresiduals(fit2, lag=24)

accuracy(fit)
accuracy(fit2)

coeftest(fit2)

## Generate forecasts for the next 108 months
forecast_values <- forecast(fit2, h = 108)
print(forecast_values)

## plot the graph out
pdf("larger_plot.pdf", width = 10, height = 6)
plot(forecast_values, main = "ARIMA Forecast", xlab = "Date", ylab = "IPG3113N")
dev.off()

## Holt-Winter Model
## Additive Holt-winters Method 
# Fit the additive Holt-Winters model to the training data
additive_model <- hw(train, seasonal = "additive")

# Forecast the time series for the testing period
additive_forecast <- predict(additive_model, 3)

# Print the forecast
print(additive_forecast)

## Multiplicative Holt-Winters Method
# Fit the multiplicative Holt-Winters model to the training data
multiplicative_model <- hw(train, seasonal = "multiplicative")

# Forecast the time series for the testing period
multiplicative_forecast <- predict(multiplicative_model, 12)

# Print the forecast
print(multiplicative_forecast)

##### Holt-Winters exponential smoothing with trend and multiplicative seasonal component.


# ------------------------- Step 5 : Diagnostic Checking (Randomness) -------------------------
## Check the Holt-Winter Method residuals
residuals <- checkresiduals(additive_model)
residuals2 <- checkresiduals(multiplicative_model)

## Convert the residuals object to a numeric vector
numeric_residuals <- as.numeric(residuals)
numeric_residuals2 <- as.numeric(residuals2)

## Perform the Box-Pierce test
bp.test <- Box.test(numeric_residuals, lag = 12)
bp.test2 <- Box.test(numeric_residuals2, lag = 12)

## Print the results of the Box-Pierce test
print(bp.test)
print(bp.test2)

## Perform the Ljung-Box test (same as Box-Pierce test)
ljung_box_test <- Box.test(numeric_residuals, lag = 12, type = "Ljung")
ljung_box_test2 <- Box.test(numeric_residuals2, lag = 12, type = "Ljung")

## Print the results of the Ljung-Box test
print(ljung_box_test)
print(ljung_box_test2)

# ---Step 7 : Consider Alternative Model, Compare and Determine the Best Model ---
## helps to find the most suitable ARIMA model for the data
fit3 <- auto.arima(candy_data)
summary(fit3)
auto.arima(data, ic="aic", trace=TRUE)

# ----------------- Step 8(a) : Form Equation for the Best Model -----------------



# ----------------- Step 8(b) : Estimate the model's coefficients -----------------


# ------------------------- Step 9 : Forecasting -------------------------


# ------------------------- Step 10 : Evaluate Model -------------------------
