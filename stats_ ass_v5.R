# ------------------------------ Load necessary libraries ----------------------
library(ggplot2)
library(lubridate)
library(forecast)
library(ggfortify)
library(tseries)
library(e1071)
library(MASS)

# ------------------------- Step 0: Import and Split Data ----------------------
## import the data set
candy_data <- read.csv("candy_production.csv", stringsAsFactors = FALSE)

### Data Understanding ###

ls() # Check my data frame name

str(candy_data) # Check the structure

any(is.na(candy_data)) # Check for missing values

## format the date
candy_data$as.date = as.Date(candy_data$observation_date, format = "%m/%d/%Y")

candy_data = candy_data[order(candy_data$as.date),]

## Find the min date and max date to easy know how many years
min_date = min(candy_data$as.date)
max_date = max(candy_data$as.date)

## Create a time series object
candy_ts <- ts(candy_data$IPG3113N, start = c(1972, 1), end=c(2017,8), frequency = 12)

## look the first few rows
head(candy_ts)

## Plot Time Series
plot(candy_ts, xlab="Year", ylab = "IPG3113N")

## Split the data into training and test sets
##### Use the train to do the model then the end use to compare
train <- window(candy_ts, start = c(1972, 1)) # 80% for training
test <- window(candy_ts, start = c(2009, 1)) # 20% for testing

##### below the code we use the candy time series to do the modeling but we need use the train
##### to do the modeling not meh? so how to use the train to do the modeling?

# ------------------------- Step 1 : Visualize the Time Series -------------------------
## Additive -> Ts = Seasonal + Trend + Random
## Multiplicative -> Ts = Seasonal *Trend*Random

##### need compare the multiplicative vs the additive? or just do normal can ady? 
##### if do the comparison will add mark?

## Compare the Multiplicative vs Additive 
### see random
plot(decompose(candy_ts))
plot(decompose(candy_ts,type = "multiplicative"))

## Set decomposed model
decomposition <- decompose(candy_ts,type = "multiplicative")

## Visual Inspection of Trend Component
plot(decomposition$trend, main = "Trend Component", xlab = "Date", ylab = "Trend")

## Visual Inspection of Residual Component
plot(decomposition$random, main = "Residual Component", xlab = "Date", ylab = "Residual")


## Visual Inspection of Seasonal Component
plot(decomposition$seasonal, main = "Seasonal Component", xlab = "Date", ylab = "Seasonal")

candyTrend <- window(decomposition$trend,start=1972,end=c(2017,8))

NewCandy <- decomposition$trend * decomposition$seasonal * decomposition$random
plot(NewCandy)
lines(candy_ts,col=4)

# ------------------------- Step 2 : Transformations ---------------------------
## CHECK NEED TO DO TRANFORMATION OR NOT  
### check skewness first
skew <- skewness(candy_data$IPG3113N)
print(skew)

##### Output:  -0.149 indicates that the data is slightly left-skewed. 
##### (-0.149) suggests that the skewness is relatively small, 
##### which means the departure from a perfectly symmetric distribution is not severe

## Q-Q plot closely follow the line of equality--> 
## suggests that the dataset is approximately normally distributed
qqnorm(candy_data$IPG3113N)
qqline(candy_data$IPG3113N)

## Shapiro-Wilk test --> whether a data set follows a normal distribution
shapiro.test(candy_data$IPG3113N)

##### Output: W-statistic is close to 1, which suggests that the data is relatively close to a normal distribution.
##### p-value (p < 0.0001) --> does not follow a normal distribution --> need to do transformation

## DO THE TRANSFORMATION BY USING BOX-COX
original_data <- candy_data$IPG3113N

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
candy_data$Transformed_IPG3113N <- transformed_data

## Plot the original and transformed data
par(mfrow = c(1, 2))  # Create a 1x2 grid for side-by-side plots
hist(original_data, main = "Original Data", xlab = "Value", col = "violet")
hist(transformed_data, main = "Transformed Data", xlab = "Transformed Value", col = "pink")
par(mfrow = c(1, 1))  # Reset plotting parameters

## check again whether original data or transformed data better
## Perform Augmented Dickey-Fuller (ADF) test on the original data
adf_original <- adf.test(original_data)
cat("Augmented Dickey-Fuller (ADF) Test on Original Data:\n")
cat("ADF Statistic:", adf_original$statistic, "\n")
cat("P-value:", adf_original$p.value, "\n")
cat("Critical Values:", adf_original$critical, "\n")

## Perform Augmented Dickey-Fuller (ADF) test on the transformed data
adf_transformed <- adf.test(transformed_data)
cat("\nAugmented Dickey-Fuller (ADF) Test on Transformed Data:\n")
cat("ADF Statistic:", adf_transformed$statistic, "\n")
cat("P-value:", adf_transformed$p.value, "\n")
cat("Critical Values:", adf_transformed$critical, "\n")

##### Result: 
##### For the Original Data:
##### ADF Statistic: -3.851111
##### P-value: 0.01644398
##### For the Transformed Data:
##### ADF Statistic: -3.986767
##### P-value: 0.01             --> better 

print(candy_data)

##### use candy_data do or use candy_ts do?
##### why our blue line so down ah?
# ------------------------- Step 3: Check the stationary of series -------------------------
## Examine autocorrelation and partial autocorrelation to identify potential lag values for modeling
acf(candy_data$Transformed_IPG3113N, main = "Autocorrelation Function (ACF)")
pacf(candy_data$Transformed_IPG3113N, main = "Partial Autocorrelation Function (PACF)")

##### The ACF show the dataset is seasonal
##### The ACF and PACF show will use the AR [AR(5)] model to build the ARIMA 

## Statistical Tests using adf --> result < 0.05 means stationary
adf.test(candy_data$Transformed_IPG3113N)

##### The adf test show the datset is Stationary

## examine relationships between values at different time points
lag.plot(candy_data$IPG3113N, main = "Lag Plot") # using the original data
lag.plot(candy_data$Transformed_IPG3113N, main = "Lag Plot") # using the transformed

## Analysis for seasonal - - - - - - - - - - - - - - - - - - - - - - - - - 
summary(decomposition$seasonal)

### Plot the autocorrelation function (ACF) of the seasonal component
acf(decomposition$seasonal, main = "ACF of Seasonal Component")

### Plot the partial autocorrelation function (PACF) of the seasonal component
pacf(decomposition$seasonal, main = "PACF of Seasonal Component")

##### error???
### First Differencing 
z <- candy_ts(diff(candy_data, lag=12))
candy_ts.plot(z,gpars=list(main="First Differences", 
                           xlab="Year", ylab="IPG3113N",lty=1))
acf(z,main="z")
pacf(z,main="z")
Box.test(z,lag=24) # Box - pierce Q test
Box.test(z,lag=24,type = "Ljung") # Ljung-Box test

## Analysis for trend - - - - - - - - - - - - -  - - - - - - - - - - - - 
summary(decomposition$trend)

### check got missing value or not
any(is.na(decomposition$trend))
# remove missing value
decomposition$trend <- na.omit(decomposition$trend)

### Plot the autocorrelation function- ACF & PACF of the trend component
acf(decomposition$trend, main = "ACF of Trend Component")
pacf(decomposition$trend, main = "PACF of Trend Component")

## Analysis for Residual - - - - - - - - - - - - -  - - - - - - - - - - - -
summary(decomposition$random)

any(is.na(decomposition$random))
decomposition$random <- na.omit(decomposition$random)

### Plot the autocorrelation function (ACF) of the residual component
acf(decomposition$random, main = "ACF of Residual Component")

### Plot the partial autocorrelation function (PACF) of the residual component
pacf(decomposition$random, main = "PACF of Residual Component")

# ------------------------- Step 4 : Find Optimal Parameters -------------------------
## Additive Holt-winters Method 
Yt <- ts(candy_data, frequency=4) #function “ts” is used to create time series object
Yt

plot(Yt, xlab="Date", ylab="Candy Production")
m<- HoltWinters(Yt, alpha=0.2, beta=0.1, gamma=0.1, seasonal="additive")
m
##### S1-4 = 1-4 quarter

MSE <- m$"SSE"/(NROW(candy_data)-3)
MSE

predict(object=m, n.ahead=3,prediction.interval=T, level=.95) # error???
##### Holt-Winters exponential smoothing with trend and additive seasonal component.

## Multiplicative Holt-Winters Method
Yt <- ts(candy_data, frequency=4)
plot(Yt, xlab="Date", ylab="IPG3113N")
m<- HoltWinters(Yt, alpha=0.2, beta=0.1, gamma=0.1, seasonal="mult")
m

predict(object=m, n.ahead=4,prediction.interval=T, level=.95)
##### Holt-Winters exponential smoothing with trend and multiplicative seasonal component.

## ARIMA(5,1,0) or AR(5)
candy_ts <- ts(candy_data$IPG3113N)

## Calculate the first differencing of the time series
candy_data_diff <- diff(candy_data$IPG3113N, seasonal = 12)

## Fit the AR(5) seasonal ARIMA model
arima_model_seasonal <- arima(candy_data_diff, order = c(5, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))

## Print the summaries of the models
summary(arima_model_seasonal)

## Fit the ARIMA model
arima_model <- arima(candy_ts_diff, order = c(5, 1, 0), seasonal = list(order = c(5, 1, 0), period = 12))

## Print the summary of the model
summary(arima_model)

# Forecast the next 12 values of the time series
forecast_ts <- forecast(arima_model, h = 12)

# Print the forecast
print(forecast_ts)

# plot the graph out
plot(forecast_ts, main = "ARIMA Forecast", xlab = "Year", ylab = "IPG3113N")

# Fit ARIMA Model - - - - - - - - - - - - -  - - - - - - - - - - - -
autoarima_model <- auto.arima(candy_ts)
summary(autoarima_model)

# Generate forecasts for the next 108 months
forecast_values <- forecast(autoarima_model, h = 108)
print(forecast_values)

# plot the graph out
plot(forecast_values, main = "ARIMA Forecast", xlab = "Date", ylab = "IPG3113N")

# ------------------------- Step 5 : Diagnostic Checking (Randomness) -------------------------
## Check the residuals
residuals <- checkresiduals(arima_model)

## Convert the residuals object to a numeric vector
numeric_residuals <- as.numeric(residuals)

##Perform the Box-Pierce test
bp.test <- Box.test(numeric_residuals, lag = 12)

## Print the results of the Box-Pierce test
print(bp.test)

## Perform the Ljung-Box test (same as Box-Pierce test)
ljung_box_test <- Box.test(numeric_residuals, lag = 12, type = "Ljung")

## Print the results of the Ljung-Box test
print(ljung_box_test)

# ---Step 7 : Consider Alternative Model, Compare and Determine the Best Model ---
## helps to find the most suitable ARIMA model for the data
fit <- auto.arima(candy_data)
summary(fit)
auto.arima(candy_data, ic="aic", trace=TRUE)
##### error?

## Calculate AIC for the auto.arima model
aic_auto_arima <- AIC(autoarima_model)

## Calculate AIC for the custom ARIMA model
aic_custom_arima <- AIC(arima_model)

## Compare AIC values
if (aic_auto_arima < aic_custom_arima) {
  print("Auto ARIMA model is better.")
} else {
  print("Custom ARIMA model is better.")
}

# ----------------- Step 8(a) : Form Equation for the Best Model -----------------




