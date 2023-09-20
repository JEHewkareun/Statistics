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


