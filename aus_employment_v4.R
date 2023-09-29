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
train <- window(data_ts, start = c(1978, 1), end = c(1988, 12)) # 80% for training
test <- window(data_ts, start = c(1989, 1), end = c(1991, 3)) # 20% for testing

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


# ------------------------- Step 4 : Find Optimal Parameters -------------------------


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
## Check Residuals
checkresiduals(fit, lag = 24)

## Identification revisited
# auto arima --> ARIMA(1,1,2)(0,1,1)[12]
fit2<- auto.arima(train, ic="aic", trace = TRUE)
summary(fit2)
auto.arima(train,stepwise = F,trace = T, ic=c(("aic")))
checkresiduals(fit2, lag=24)

accuracy(fit)
accuracy(fit2)		 


coeftest(fit2)

## Generate forecasts for the next 108 months
forecast_values <- forecast(fit2, h = 84)
print(forecast_values)


## plot the graph out
pdf("larger_plot.pdf", width = 10, height = 6)
plot(forecast_values, main = "ARIMA Forecast", xlab = "Date", ylab = "IPG3113N")
dev.off()

## SARIMA model
fitSarima <- arima(train,order=c(1,1,2),seasonal = list(order=c(0,1,1),period=12))

## Check Residuals for SARIMA model
tsdisplay(residuals(fitSarima),lag.max = 24, main = "Residual For SARIMA ")
# Shapiro test看残差是否是常态
shapiro.test(fitSarima$residuals)
# Box test 看残差之间是否是独立的
Box.test(fitSarima$residuals, lag=10,type ="Ljung-Box")
checkresiduals(fitSarima, lag = 24)

# check 
summary(fitSarima)

# predict/forecast
p <- forecast(fitSarima)
plot(p)
forecast_Sarima <- as.data.frame(p)
evalute <- cbind(forecast_Sarima,test)

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


#ets
fit4 <- ets(train)
fit4
summary(fit4)
autoplot(fit4)
coef(fit4)
accuracy(fit4)
cbind('Residuals' = residuals(fit4),
      'Forecast errors' = residuals(fit4,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

forecast_values2 <- forecast(fit4, h = 84)
print(forecast_values2)
plot(forecast_values2)

# ------------------------- Step 5 : Diagnostic Checking (Randomness) -------------------------
## Check the Holt-Winter Method residuals
residuals <- checkresiduals(additive_model)
residuals2 <- checkresiduals(multiplicative_model)
residuals3 <- checkresiduals(fit4)



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
summary(fit2)
summary(fit4)
forecast(fit4)
auto.arima(data, ic="aic", trace=TRUE)


# ----------------- Step 8(a) : Form Equation for the Best Model -----------------



# ----------------- Step 8(b) : Estimate the model's coefficients -----------------


# ------------------------- Step 9 : Forecasting -------------------------


# ------------------------- Step 10 : Evaluate Model -------------------------
