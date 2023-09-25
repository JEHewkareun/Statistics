# ------------------------------ Load necessary libraries --------------------
library(ggplot2)
library(lubridate)
library(forecast)
library(ggfortify)
library(tseries)

# ------------------------- Step 0: Import and Split Data --------------------
candy_data <- read.csv("candy_production.csv")

# Convert the date column to the correct format
candy_data$observation_date <- as.Date(candy_data$observation_date, format = "%m/%d/%Y")

# Check for missing values
if (any(is.na(candy_data))) {
  print("There are missing values in your dataset. Please handle them.")
} else {
  # Create a time series object
  candy_ts <- ts(candy_data$IPG3113N, frequency = 12, start = c(1972, 1))
  
  # Plot Time Series
  plot(candy_ts, main = "Time Series Plot of IPG3113N")
  
  # Split the data into training and test sets
  train <- window(candy_ts, start = c(1973, 1), end = c(1993, 12))
  test <- window(candy_ts, start = c(1994, 1), end = c(2017, 12))
}
print(train)
# By percentage
library(forecast)
length(candy_data)
train <- head(candy_data, round(length(candy_data) * 0.67))
h <- length(candy_data) - length(train)
test1 <- tail(candy_data,h) 
train
test1
autoplot(train) + autolayer(test1)

ls() # Check my data frame name

str(candy_data) # Check the structure

any(is.na(candy_data)) # Check for missing values

# Convert date column to date format
candy_data$observation_date <- as.Date(candy_data$observation_date, format = "%m/%d/%Y")

# ------------------------- Step 1 : Visualize the Time Series -------------------------
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

# ------------------------- Step 2 : Transformations -------------------------
candy_data <- ts(candy_data, frequency = 12, start = c(1973,1))
plot(candy_data, ylab="IPG3113N", main="Candy Production from 1973
to 2017")

candy_data <- log(candy_data); candy_data
candy_data <- ts(candy_data, frequency = 12, start = c(1973,1))
plot(candy_data, ylab="Log(IPG3113N)", main="Candy Production from 1973 to 2017")

# Filtering (Moving average)
## computer simple moving average of order 5 then plot it in time series graph
t <- 1973:(1973+length(candy_data)-1)
sma <- function(x,n) {filter(x,rep(1/n,n), sides = 2L)}
X <- sma(candy_data,5); X
df <- data.frame(t,candy_data,X)
ggplot(df, aes(x=t)) +
  geom_point(aes(y=candy_data), colour="blue", size = 2) +
  geom_point(aes(y=X), colour="red", size = 2) +
  geom_line(aes(y=candy_data), colour="blue", size = 1) +
  geom_line(aes(y=X), colour="red", size = 1) +
  ggtitle("Graph of moving mean (order 5)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date") + ylab("IPG3113N")

# ------------------------- Step 3(a) : Differencing -------------------------
## calculate the first difference and second difference of dataset the plot the first difference
candy_data <- ts(candy_data)
plot(candy_data, xlab="Date", ylab="IPG3113N", main="Candy Production from 1973 to 2017")

candy_data <- ts(diff(candy_data))
plot(candy_data, xlab="Date", ylab="candy_data(t)-candy_data(t-1)", main="First
Difference")

ndiffs(candy_data)
nsdiffs(candy_data)

## Correlogram
### Plot the correlogram for lag 0 to lag12
ACF <- acf(candy_data, main="Correlogram for Candy Production")
ACF$acf

# ------------------------- Step 3 (b) : Check the stationary of series -------------------------
# Examine autocorrelation and partial autocorrelation to identify potential lag values for modeling
acf(candy_data$IPG3113N, main = "Autocorrelation Function (ACF)")
pacf(candy_data$IPG3113N, main = "Partial Autocorrelation Function (PACF)")

# Statistical Tests using adf --> result < 0.05 means likely stationary
library(tseries)
adf.test(candy_data$IPG3113N)

# examine relationships between values at different time points
lag.plot(candy_data$IPG3113N, main = "Lag Plot")

# Analysis for seasonal
summary(decomposition$seasonal)

# Plot the autocorrelation function (ACF) of the seasonal component
acf(decomposition$seasonal, main = "ACF of Seasonal Component")

# Plot the partial autocorrelation function (PACF) of the seasonal component
pacf(decomposition$seasonal, main = "PACF of Seasonal Component")

# Analysis for trend 
summary(decomposition$trend)

# check got missing value or not
any(is.na(decomposition$trend))
# remove missing value
decomposition$trend <- na.omit(decomposition$trend)

# Plot the autocorrelation function- ACF & PACF of the trend component
acf(decomposition$trend, main = "ACF of Trend Component")
pacf(decomposition$trend, main = "PACF of Trend Component")

# Analysis for Residual 
summary(decomposition$random)

any(is.na(decomposition$random))
decomposition$random <- na.omit(decomposition$random)

# Plot the autocorrelation function (ACF) of the residual component
acf(decomposition$random, main = "ACF of Residual Component")

# Plot the partial autocorrelation function (PACF) of the residual component
pacf(decomposition$random, main = "PACF of Residual Component")

# ------------------------- Step 4 : Find Optimal Parameters -------------------------
### Additive Holt-winters Method
Yt <- ts(candy_data, frequency=4) #function “ts” is used to create time series object
Yt

plot(Yt, xlab="Date", ylab="Candy Production")
m<- HoltWinters(Yt, alpha=0.2, beta=0.1, gamma=0.1, seasonal="additive")
m
##### S1-4 = 1-4 quarter

MSE <- m$"SSE"/(NROW(candy_data)-3)
MSE

predict(object=m, n.ahead=3,prediction.interval=T, level=.95) # error???
#Holt-Winters exponential smoothing with trend and additive seasonal component.

### Multiplicative Holt-Winters Method
Yt <- ts(candy_data, frequency=4)
plot(Yt, xlab="Date", ylab="IPG3113N")
m<- HoltWinters(Yt, alpha=0.2, beta=0.1, gamma=0.1, seasonal="mult")
m

predict(object=m, n.ahead=4,prediction.interval=T, level=.95)
#Holt-Winters exponential smoothing with trend and multiplicative seasonal component.

#### ARMA Model
##### ARMA (1,1)
set.seed(1)
xt <- arima.sim(list(order=c(1,0,1), ar=c(0.8), ma=c(-0.9)), n=500)
yt <- xt + 10
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
plot.ts(yt)
acf(yt, main='Autocorrelations')
pacf(yt, main='Partial Autocorrelations')

#### ARIMA
##### ARIMA(1,1,1)
##### (p,d,q) --> non seasonal part of model
##### (P,D,Q) --> seasonal part of model

# Fit ARIMA Model
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate forecasts for the next 24 months
forecast_values <- forecast(arima_model, h = 108)
print(forecast_values)

# plot the graph out
plot(forecast_values, main = "ARIMA Forecast", xlab = "Date", ylab = "IPG3113N")

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
