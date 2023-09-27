# ------------------------------ Load necessary libraries ----------------------
library(ggplot2)
library(lubridate)
library(forecast)
library(ggfortify)
library(tseries)
library(e1071)
library(MASS)
library(lmtest)

# ------------------------- Step 0: Import and Split Data ----------------------
candy_data <- read.csv("candy_production.csv")

ls() # Check my data frame name

str(candy_data) # Check the structure

any(is.na(candy_data)) # Check for missing values

# Convert the date column to the correct format
candy_data$observation_date <- as.Date(candy_data$observation_date, format = "%m/%d/%Y")

# Check for missing values
if (any(is.na(candy_data))) {
  print("There are missing values in your dataset. Please handle them.")
} else {
  # Create a time series object
  candy_ts <- ts(candy_data$IPG3113N, frequency = 12, start = c(1972, 1))
  
  # Plot Time Series
  ggplot(data = NULL, aes(x = time(candy_ts), y = candy_ts)) +
    geom_line() +
    labs(x = "Date", y = "IPG3113N") +
    ggtitle("Time Series Plot of IPG3113N")
  
  # Split the data into training and test sets
  # Use the train to do the model then the end use to compare
  train <- window(candy_ts, start = c(1972, 1), end = c(2008, 12))
  test <- window(candy_ts, start = c(2009, 1), end = c(2017, 12))
}

# ------------------------- Step 1 : Visualize the Time Series -------------------------
summary(candy_data$IPG3113N)

# Examine the distribution 
hist(candy_data$IPG3113N, main = "Histogram of IPG3113N", xlab = "IPG3113N")

# Box plot
boxplot(candy_data$IPG3113N, main = "Box Plot of IPG3113N", ylab = "IPG3113N")

# Perform seasonal decomposition
ts_data <- ts(candy_data$IPG3113N, frequency = 12)  # Assuming monthly data (frequency = 12)
decomposition <- decompose(ts_data)
plot(decomposition) # Plot the decomposition components (trend, seasonal, and remainder)

# Visual Inspection of Trend Component
plot(decomposition$trend, main = "Trend Component", xlab = "Date", ylab = "Trend")

# Visual Inspection of Seasonal Component
plot(decomposition$seasonal, main = "Seasonal Component", xlab = "Date", ylab = "Seasonal")

# Visual Inspection of Residual Component
plot(decomposition$random, main = "Residual Component", xlab = "Date", ylab = "Residual")

# ------------------------- Step 2 : Transformations ---------------------------

# CHECK NEED TO DO TRANFORMATION OR NOT  
# check skewness first
skew <- skewness(candy_data$IPG3113N)
print(skew)
# Output:  -0.149 indicates that the data is slightly left-skewed. 
# (-0.149) suggests that the skewness is relatively small, which means the departure from a perfectly symmetric distribution is not severe

# Q-Q plot closely follow the line of equality--> suggests that the dataset is approximately normally distributed
qqnorm(candy_data$IPG3113N)
qqline(candy_data$IPG3113N)

# Shapiro-Wilk test --> whether a dataset follows a normal distribution
shapiro.test(candy_data$IPG3113N)
# Output: W-statistic is close to 1, which suggests that the data is relatively close to a normal distribution.
# p-value (p < 0.0001) --> does not follow a normal distribution --> need to do transformation


# DO THE TRANSFORMATION 
original_data <- candy_data$IPG3113N

# Find the optimal lambda (λ) for Box-Cox transformation
optimal_lambda <- optimize(function(lambda) -sum(log(abs((original_data^lambda - 1) / lambda))), c(-2, 2))$minimum
cat("Optimal lambda (λ) =", optimal_lambda, "\n")
# Output: Optimal lambda (λ) = 1.99994 

# Transform the data using the optimal lambda
transformed_data <- if (abs(optimal_lambda) > 0.001) {
  (original_data^optimal_lambda - 1) / optimal_lambda
} else {
  log(original_data)
}

# Assign the transformed data back to the dataset
candy_data$Transformed_IPG3113N <- transformed_data

# Plot the original and transformed data
par(mfrow = c(1, 2))  # Create a 1x2 grid for side-by-side plots
hist(original_data, main = "Original Data", xlab = "Value", col = "violet")
hist(transformed_data, main = "Transformed Data", xlab = "Transformed Value", col = "pink")
par(mfrow = c(1, 1))  # Reset plotting parameters

#check again whether ori data or transformed data better
# Perform Augmented Dickey-Fuller (ADF) test on the original data
adf_original <- adf.test(original_data)
cat("Augmented Dickey-Fuller (ADF) Test on Original Data:\n")
cat("ADF Statistic:", adf_original$statistic, "\n")
cat("P-value:", adf_original$p.value, "\n")
cat("Critical Values:", adf_original$critical, "\n")

# Perform Augmented Dickey-Fuller (ADF) test on the transformed data
adf_transformed <- adf.test(transformed_data)
cat("\nAugmented Dickey-Fuller (ADF) Test on Transformed Data:\n")
cat("ADF Statistic:", adf_transformed$statistic, "\n")
cat("P-value:", adf_transformed$p.value, "\n")
cat("Critical Values:", adf_transformed$critical, "\n")

# Result: 
# For the Original Data:
# ADF Statistic: -3.851111
# P-value: 0.01644398
# For the Transformed Data:
# ADF Statistic: -3.986767
# P-value: 0.01             --> better 

print(candy_data)

# ------------------------- Step 3: Check the stationary of series -------------------------
# Examine autocorrelation and partial autocorrelation to identify potential lag values for modeling
acf(candy_data$Transformed_IPG3113N, main = "Autocorrelation Function (ACF)", lag.max = 40)
pacf(candy_data$Transformed_IPG3113N, main = "Partial Autocorrelation Function (PACF)", lag.max = 40)

# Statistical Tests using adf --> result < 0.05 means stationary
adf.test(candy_data$Transformed_IPG3113N)

# examine relationships between values at different time points
lag.plot(candy_data$IPG3113N, main = "Lag Plot") # using the original data
lag.plot(candy_data$Transformed_IPG3113N, main = "Lag Plot") # using the transformed 

#Decompose again 
ts_data2 <- ts(candy_data$Transformed_IPG3113N, frequency = 12)  # Assuming monthly data (frequency = 12)
decomposition2 <- decompose(ts_data2)
plot(decomposition2) 

# Analysis for seasonal - - - - - - - - - - - - - - - - - - - - - - - - - 
summary(decomposition2$seasonal)

# Plot the autocorrelation function (ACF) of the seasonal component
acf(decomposition2$seasonal, main = "ACF of Seasonal Component")

# Plot the partial autocorrelation function (PACF) of the seasonal component
pacf(decomposition2$seasonal, main = "PACF of Seasonal Component")

# Analysis for trend - - - - - - - - - - - - -  - - - - - - - - - - - - 
summary(decomposition2$trend)

# check got missing value or not
any(is.na(decomposition2$trend))
# remove missing value
decomposition2$trend <- na.omit(decomposition2$trend)

# Plot the autocorrelation function- ACF & PACF of the trend component
acf(decomposition2$trend, main = "ACF of Trend Component")
pacf(decomposition2$trend, main = "PACF of Trend Component")

# Analysis for Residual - - - - - - - - - - - - -  - - - - - - - - - - - -
summary(decomposition2$random)

any(is.na(decomposition2$random))
decomposition2$random <- na.omit(decomposition2$random)

# Plot the autocorrelation function- ACF & PACF of the residual component
acf(decomposition2$random, main = "ACF of Residual Component")
pacf(decomposition2$random, main = "PACF of Residual Component")

print(decomposition2)


acf(candy_data$Transformed_IPG3113N, main = "Autocorrelation Function (ACF)")
pacf(candy_data$Transformed_IPG3113N, main = "Autocorrelation Function (PACF)")

# ------------------------- Step 4 : Find Optimal Parameters -------------------------
plot(candy_ts)
candy_ts <- ts(candy_data$IPG3113N)
acf(candy_ts, lag.max = 40)
pacf(candy_ts, lag.max = 40)
checkresiduals(candy_ts, lag=24)

adf.test(candy_ts) 
ndiffs(candy_ts)
ndiffs(candy_ts)

z <- ts(diff(candy_ts, lag=12))
ts.plot(z, gpars = list(main="First(seasonal) Differences"))
acf(z, main="z", lag.max = 40)
pacf(z, main="z", lag.max = 40)
adf.test(z)
checkresiduals(z, lag=24)

##Guess Arima(p,d,q)(P,D,Q)
##### (p,d,q) --> non seasonal part of model ; d = 0
##### (P,D,Q) --> seasonal part of model ; D = 1
##### p is for AR model --> see PACF; q is for MA model --> see ACF
# lets see the PACF 
# arima(2, 0, 0)(3, 1, 0)[12]
fit <- arima(candy_ts, order=c(2,0,0), seasonal = list(order = c(3,1,0), period=12))
fit
summary(fit)

## Diagnostic checking 
checkresiduals(fit, lag = 24)

# Identification revisited
fit<- auto.arima(candy_ts, ic="aic", trace = TRUE)
summary(fit)
auto.arima(candy_ts, ic="aic", trace=TRUE)
checkresiduals(fit, lag=24)

coeftest(fit)

# Generate forecasts for the next 108 months
forecast_values <- forecast(fit, h = 108)
print(forecast_values)

# plot the graph out
pdf("larger_plot.pdf", width = 10, height = 6)
plot(forecast_values, main = "ARIMA Forecast", xlab = "Date", ylab = "IPG3113N")
dev.off()
# ------------------------- Step 5 : Diagnostic Checking (Randomness) -------------------------
## Portmanteau Tests
## Interpreting Autocorrelation Chart
candy_data <- ts(candy_data) # The function ts is used to create time-series objects
z <- ts(diff(candy_data, lag=1)) # function diff = Returns suitably lagged and iterated

differences
z2 <- ts(diff(z, lag=1))
ts.plot(y,gpars=list(main="Original Values", xlab="Date", ylab="IPG3113N", lty=1))
acf(candy_data) #acf computes estimates of autocorrelation function
pacf(candy_data) #pacf computes estimates of partial autocorrelation function

ts.plot(z,gpars=list(main= "First Differences", xlab="Date", ylab="IPG3113N", lty=1))
acf(z, main="z")
pacf(z, main="z")

Box.test(z, lag=24) #Box-pierce Q test
Box.test(z, lag=24,type="Ljung") #Ljung-Box test

ts.plot(z2,gpars=list(main= "Second Differences", xlab="Date", ylab="IPG3113N", lty=1))
acf(z2, main="z2")
pacf(z2, main="z2")

Box.test(z2, lag=24)
Box.test(z2, lag=24,type="Ljung")

# ---Step 7 : Consider Alternative Model, Compare and Determine the Best Model ---
# helps to find the most suitable ARIMA model for the data
library(forecast)
fit <- auto.arima(candy_data)
summary(fit)
auto.arima(candy_data, ic="aic", trace=TRUE)

# ----------------- Step 8(a) : Form Equation for the Best Model -----------------
# Assuming you have a data frame df with variables X and Y
model <- lm(Y ~ X, data = df)
summary(model)
Y = a + b1*X1 + b2*X2 + ... + bn*Xn
coefficients <- coef(model)
intercept <- coefficients[1]
slope <- coefficients[2]
equation <- paste("Y =", round(intercept, 2), "+", round(slope, 2), "* X")

# ----------------- Step 8(b) : Estimate the model's coefficients -----------------
arima(candy_data, order=c(0,1,1,),seasonal = list(order=c(0,1,1), period=12))

# Step 8(c) : Test the significance of the coefficients
library(lmtest)
fit <-arima(candy_data,order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
coeftest(fit)

# ------------------------- Step 9 : Forecasting -------------------------
## ETS Approach
library(forecast)
fit<-ets(candy_data)
plot(forecast(fit))

## ARIMA Approach
fit<-arima(candy_data,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
plot(forecast(fit,h=12))

# ------------------------- Step 10 : Evaluate Model -------------------------
