rm(list=ls())   
# Clear the workspace to ensure we start with a clean environment without leftover variables.

# Install libraries ####
# Loading all the required packages. These cover data manipulation (dplyr, tidyr), 
# date handling (lubridate), missing data imputation (mice), and time series/forecasting models (forecast, rugarch, etc.)
library(readr)
library(dplyr) 
library(tidyr)
library(psych)
library(ggplot2)
library(forecast) 
library(lubridate)
library(mice)
library(zoo)
library(urca)
library(uroot)
library(xts)
library(timeSeries)
library(rugarch)
library(forecast)
library(tseries)
library(scoringRules)

# Import dataset and preprocess ####
# Reading the dataset from a CSV file.
Flight_delay <- read_delim("Espresso_macchiato_Data.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Structure of the dataset
str(Flight_delay)

# Managing date - Convert character to POSIXct
# Convert the date column from plain text to a proper Date-Time object for R to understand time progression.
Flight_delay$Date<- as.POSIXct(Flight_delay$Date, format="%Y-%m-%d")

# Managing time - Changing flights with midnight departures to 0h
# Standardizing midnight representation to start of the day (0) instead of 2400.
Flight_delay$DepTime[Flight_delay$DepTime == 2400] <- 0
Flight_delay$ArrTime[Flight_delay$ArrTime == 2400] <- 0


### Manage hourly data  ##

# Compute mean departure delay per hour
# Grouping the raw flight data by hour to create a more manageable time series.
hourly_delay <- Flight_delay %>%
  mutate(
    Dephour = floor(DepTime / 100),           # Extract hour from HHMM format
    DateTime = Date + hours(Dephour)          # Create full timestamp
  ) %>%
  group_by(DateTime) %>%
  #aggregate departure delays and carrier delay by mean for each hourly interval
  summarise(MeandepDelay = mean(DepDelay), na.rm= TRUE, .group="drop") %>%
  arrange(DateTime)

#Create a complete time series by adding all hourly timestamps (missing filled with NA)
# Ensures our time series has no gaps in the timeline, which is strictly required for accurate forecasting.
hourly_delay_full <- hourly_delay %>%
  complete(DateTime = seq(min(DateTime), max(DateTime), by = "hour"))

# Extract hour (0–23) from timestamp
hourly_delay_full$hour <- hour(hourly_delay_full$DateTime)
print(class(hourly_delay_full))
print(str(hourly_delay_full))

# Count missing values (NA) by hour
total_by_hour <- hourly_delay_full %>%
  count(hour, name = "total_obs")

# Identify which hours in the interval had days without any flights 
# This helps us understand if certain hours (like late night) consistently lack data.
missing_by_hour <- hourly_delay_full %>%
  dplyr::filter(is.na(MeandepDelay)) %>%
  dplyr::count(hour, name = "n_missing") %>%
  tidyr::complete(hour = 0:23, fill = list(n_missing = 0)) %>%
  dplyr::left_join(total_by_hour, by = "hour") %>%
  dplyr::mutate(
    perc_missing = round((n_missing / total_obs) * 100, 2)
  ) %>%
  dplyr::arrange(hour)

# Overall missing value statistics
total_missing <- sum(is.na(hourly_delay_full$MeandepDelay))
total_obs <- nrow(hourly_delay_full)
perc_total_missing <- round((total_missing / total_obs) * 100, 2)

# Remove hours with more than 40% missing data (e.g., 3 AM and 4 AM)
# Too much missing data makes imputation unreliable, so we just remove these hours.
hourly_delay_adj <- hourly_delay_full %>%
  dplyr::filter(!(hour %in% c(3, 4)))

# Select only the variables needed for imputation
mice_data <- hourly_delay_adj %>%
  dplyr::select(MeandepDelay, hour)

# Run MICE with one imputation (m = 1), using predictive mean matching (PMM)
# MICE is a machine learning technique to intelligently guess and fill in (impute) missing values based on patterns in the data.
imputed <- mice::mice(mice_data, m = 1, method = "pmm", seed = 123)

# Replace missing values with the imputed ones
hourly_delay_adj$MeandepDelay <- mice::complete(imputed)$MeandepDelay

# Check that no missing values remain
missing_check <- hourly_delay_adj %>%
  dplyr::filter(is.na(MeandepDelay)) %>%
  dplyr::count(hour)

print(missing_check)

# Plot average departure delay by hour
Sys.setlocale("LC_TIME", "C")
plot(hourly_delay_adj$DateTime, hourly_delay_adj$MeandepDelay, type = "l",
     col = "blue", xlab = "Date", ylab = "Average Departure Delay (minutes)",
     main = "Average Departure Delay per Hour", xaxt = "n")

axis.POSIXct(1,
             at = seq(from = floor_date(min(hourly_delay_adj$DateTime), "month"),
                      to = ceiling_date(max(hourly_delay_adj$DateTime), "month"),
                      by = "1 month"),
             format = "%b", las = 2, cex.axis = 0.8)

# Quantile 25% and 75% for each hour
quantile_data <- hourly_delay_adj %>%
  group_by(hour) %>%
  summarise(
    Q25 = quantile(MeandepDelay, 0.25, na.rm = TRUE),   # 25th percentile
    Q75 = quantile(MeandepDelay, 0.75, na.rm = TRUE),   # 75th percentile
    MeanDelay = mean(MeandepDelay, na.rm = TRUE)        
  )

# Plot the quantiles using ggplot2
ggplot(hourly_delay_adj, aes(x = hour, y = MeandepDelay)) +
  geom_line(data = quantile_data, aes(x = hour, y = Q25), color = "blue", size = 1) + 
  geom_line(data = quantile_data, aes(x = hour, y = Q75), color = "red", size = 1) +   
  geom_line(data = quantile_data, aes(x = hour, y = MeanDelay), color = "green", size = 1)+
  geom_point(data = quantile_data, aes(x = hour, y = MeanDelay), color = "darkgreen", size = 2)+
  labs(
    title = "25th and 75th Percentiles of Mean Departure Delay by Hour",
    x = "Hour of the Day",
    y = "Mean Departure Delay"
  ) +
  theme_minimal()
  
# Plot average carrier delay by hour
  plot(hourly_delay_adj$DateTime, hourly_delay_adj$MeancarrDelay, type = "l",
       col = "blue", xlab = "Date", ylab = "Average Departure Delay (minutes)",
       main = "Average Departure Delay per Hour", xaxt = "n")
  
  axis.POSIXct(1,
               at = seq(from = floor_date(min(hourly_delay_adj$DateTime), "month"),
                        to = ceiling_date(max(hourly_delay_adj$DateTime), "month"),
                        by = "1 month"),
               format = "%b", las = 2, cex.axis = 0.8)
  
### Hourly time series-using ts ##

# Compute and plot ts of average departure delay by hour
# Convert data into a formal 'time series' (ts) object for statistical modeling, defining a 22-hour cycle (since 2 hours were dropped).
ts_hourly <- ts(hourly_delay_adj$MeandepDelay, frequency = 22)
plot(ts_hourly, col = "blue", main = "Base Time Series (22-hour cycle)", ylab = "Average Departure Delay", xlab = "Time")

##Daily and Weekly data ##

# compute daily data 
daily_data <- hourly_delay_adj %>%
  mutate(
    Date = as.Date(DateTime),
    Week = isoweek(DateTime),               #Week number
    DayOfWeek = wday(DateTime, label = TRUE, abbr = TRUE)  # Day of the week
  ) %>%
  group_by(Date, Week, DayOfWeek) %>%
  summarise(MeandepDelay = mean(MeandepDelay, na.rm = TRUE), .groups = "drop")

# plot Daily data
ggplot(daily_data, aes(x = Week, y = MeandepDelay, group = DayOfWeek, color = as.factor(DayOfWeek))) +
  geom_line(size = 0.9) +
  labs(title = "Departure Delay by day of the week",
       x = "Week",
       y = "Average Departure Delay (minutes)",
       color = "Daily") +
  theme_minimal() +
  theme(legend.position = "right")

# Daily time series
ts_daily<- ts(daily_data$MeandepDelay, frequency = 7)
plot(ts_daily, col="blue", ylab = "Average Departure Delay", xlab = "Time", main="Daily Departure Delay")

### Weekly data and time series ##

# compute weekly data
data_weekly <- hourly_delay_adj %>% 
  mutate(Week = floor_date(DateTime, unit = "week")) %>%  # Start of the week
  group_by(Week) %>% 
  summarise(MeandepDelay = mean(MeandepDelay, na.rm = TRUE)) %>%
  arrange(Week)

# Plot weekly data 
ggplot(data_weekly, aes(x = Week, y = MeandepDelay)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Weekly Average Departure Delay",
       x = "Week",
       y = "Average Delay (minutes)") +
  theme_minimal()

# Weekly time series
ts_weekly <- ts(data_weekly$MeandepDelay, frequency = 26)
plot(ts_weekly,  col="blue", ylab = "Average Departure Delay", xlab = "Time", main="Weekly Departure Delay")

### Analysis of departure delay time series ####

## Departure delay ts
ts_hourly <- ts(hourly_delay_adj$MeandepDelay, frequency = 22)

# ACF AND PACF
# Auto-Correlation Function (ACF) and Partial Auto-Correlation Function (PACF) plots. 
# They show how much current delays are influenced by past delays.
par(mfrow = c(1, 2))
acf(ts_hourly, lag.max=66) # Wavy persistent pattern indicative of a seasonality 
pacf(ts_hourly, lag.max = 66) #Less persistent and decreasing over lags 

# decomposition ts 
# STL decomposition breaks the time series down into 3 components: Trend (long term direction), Seasonality (repeating cycles), and Remainder (random noise).
par(mfrow = c(1, 1))
decomposition <- stl(ts_hourly, s.window = 12)

# Plot the full STL decomposition (trend, seasonal, remainder)
plot(decomposition)

# Print the names of the components in the time series
print(colnames(decomposition$time.series))  # Return: "seasonal", "trend", "remainder"

# Plot individual components
par(mfrow = c(1, 1)) 
plot(decomposition$time.series[,"trend"], main = "Trend Component")
plot(decomposition$time.series[,"seasonal"], main = "Seasonal Component")
plot(decomposition$time.series[,"remainder"], type = "l", main = "Residuals (Remainder)")

### Check stationarity and seasonality ####

## Constancy of mean and variance
# We need to verify if the properties of our series are stable over time (stationary). Most models require this.

# Rolling mean
rolling_mean <- rollmean(ts_hourly, k = 100, align = "right", fill = NA)
plot(ts_hourly, type = "l", main = "Rolling Mean", 
     ylab = "Value", xlab = "Time")     #The red moving average line appears relatively stable, approximately constant over time.
lines(rolling_mean, col = "red", lwd = 2)

# Rolling variance
rolling_var <- rollapply(ts_hourly, width = 100, FUN = var, align = "right", fill = NA)
plot(rolling_var, type = "l", main = "Rolling Variance",
     ylab = "Variance", xlab = "Time", col = "blue")    #Strong instability of variance over time, suggesting heteroskedasticity: the variance is not constant
 
#The series is not stationary in the weak sense, because the variance is not constant over time.

# Perform ADF Test
# Augmented Dickey-Fuller test mathematically checks for a "unit root" (non-stationarity).
summary(ur.df(ts_hourly, type = "trend", selectlags = "BIC"))

adf.test(ts_hourly,alternative = c("stationary")) # Reject the null hypothesis of the series having a unit root.

# Perform CH test for seasonal variation
# Helper function to resample data to a frequency of 12 for the test.
aggregate_to_freq12 <- function(ts_hourly, orig_freq = 22) {
  
  n <- length(ts_hourly)
  block_size <- orig_freq / 12
  
  if (block_size %% 1 != 0) {
    warning("downsampling introduce approx.")
  }
  
  indices <- floor(seq(1, n, by = block_size))
  indices <- indices[indices + floor(block_size) - 1 <= n]
  
  aggregated <- sapply(indices, function(i) {
    mean(ts_hourly[i:(i + floor(block_size) - 1)])
  })
  new_ts <- ts(aggregated, frequency = 12, start = c(1, 1))
  return(new_ts)
}
aggregated_ts <- aggregate_to_freq12(ts_hourly, orig_freq = 22)

# Canova-Hansen test
ch_result <- ch.test(aggregated_ts)
print(ch_result) # There is a dynamics in the parameters associated with the dummies, not constant over time.
nsdiffs(aggregated_ts) # 1 differentiation required to make a given time series stationary.

# Models and Evaluation ####
### Exponential Smoothing ####
# ETS models (Error, Trend, Seasonal) are classic forecasting models that weigh recent observations more heavily than older ones.

# Train Test Split
# We split the data into training (for the algorithm to learn) and test (to evaluate how well it predicts unseen data).
n <- length(ts_hourly)
train_size <- 22 * 140
train <- window(ts_hourly, end = time(ts_hourly)[train_size])
test <- window(ts_hourly, start = time(ts_hourly)[train_size + 1])
h <- length(test)

##Exponential smoothing

# Fit ETS model restricted to Simple Exponential Smoothing 
fit_ets_ses <- ets(train, model = "ANN")

# Fit ETS model restricted to Holt-Winter
fit_ets_hw <- ets(train, model = "ANA")

# Fit optimal EST
fit_ets <- ets(train)

# Info criteria
# Comparing AIC values helps us pick the best mathematical fit. Lower is better.
AIC(fit_ets_ses,fit_ets_hw, fit_ets) # Best one: ets (M,N,M)

# Fitted values
fitted_ets_ses  <- fitted(fit_ets_ses)
fitted_ets_hw   <- fitted(fit_ets_hw)
fitted_ets      <- fitted(fit_ets)

# Plot fitted values
autoplot(train, series = "Train") + 
  autolayer(fitted_ets_ses, series = "SES")

autoplot(train, series = "Train") + 
  autolayer(fitted_ets_hw, series = "HW")

autoplot(train, series = "Train") + 
  autolayer(fitted_ets, series = "ETS")

# Forecast for 12 months
forecast_ets_ses  <- forecast(fit_ets_ses, h = h)
forecast_ets_hw   <- forecast(fit_ets_hw, h = h)
forecast_ets      <- forecast(fit_ets, h= h)

# Plot forecast
autoplot(forecast_ets_ses) + autolayer(test)
autoplot(forecast_ets_hw) + autolayer(test)
autoplot(forecast_ets) + autolayer(test)

# Calculate Residuals
# Residuals are the "errors" - the difference between what the model predicted and the actual true values.
residuals_ets_ses = residuals(fit_ets_ses)
residuals_ets_hw = residuals(fit_ets_hw)
residuals_ets = residuals(fit_ets)

# Plot Residuals
plot(residuals_ets_ses)
plot(residuals_ets_hw)
plot(residuals_ets)

# Check acf and histogram of residuals
# Ideally, residuals should look like random noise. If there are patterns, the model missed something.
checkresiduals(residuals_ets_ses)
checkresiduals(residuals_ets_hw)
checkresiduals(residuals_ets)

### Linear Regression ####
# Using sine and cosine waves (harmonics) as predictors to capture complex seasonal cycles.

# Function to generate Fourier harmonics up to a given order
create_harmonics <- function(time, period, max_order) {
  harmonics <- data.frame(Time = time)
  for (i in 1:max_order) {
    harmonics[[paste0("sin_", i)]] <- sin(2 * pi * i * time / period)
    harmonics[[paste0("cos_", i)]] <- cos(2 * pi * i * time / period)
  }
  return(harmonics)
}

# Grid search over harmonic orders to minimize AIC
results <- data.frame(order = integer(), AIC = numeric())

max_order <- 15
period <- 22  # daily periodicity (22 observations per day)

df_model <- hourly_delay_adj %>%
  mutate(
    Time = 1:nrow(hourly_delay_adj),
    month = factor(month(DateTime)),
    week = factor(week(DateTime)),
    day_of_month = factor(mday(DateTime))
  )

df_model$DateTime <- parse_date_time(
  hourly_delay_adj$DateTime,
  orders = c("ymd HMS", "ymd HM", "ymd", "ymdT", "ymdTz")
)

df_model <- df_model %>%
  mutate(weekday = wday(DateTime, week_start = 1))

df_model$hour <- factor(hour(df_model$DateTime))
df_model$day <- factor(wday(df_model$DateTime, week_start = 1))

# Split data into training and test sets
n_per_day <- 22
period <- n_per_day
n_days <- 181
train_df <- df_model[1:3080, ]
test_df  <- df_model[3081:nrow(df_model), ]

for (order in 1:max_order) {
  harm <- create_harmonics(train_df$Time, period, order)
  model_data <- cbind(train_df, harm[, -1])  # exclude duplicate Time column
  formula_str <- paste("MeandepDelay ~ Time +", paste(colnames(harm)[-1], collapse = " + "))
  model <- lm(as.formula(formula_str), data = model_data)
  results <- rbind(results, data.frame(order = order, AIC = AIC(model)))
}

# Select the best order based on AIC
best_order <- results[which.min(results$AIC), "order"]
print(results)

# Generate daily harmonics
for (k in 1:11) {
  train_df[[paste0("sin_day", k)]] <- sin(2 * pi * k * train_df$Time / 22)
  train_df[[paste0("cos_day", k)]] <- cos(2 * pi * k * train_df$Time / 22)
  
  test_df[[paste0("sin_day", k)]] <- sin(2 * pi * k * test_df$Time / 22)
  test_df[[paste0("cos_day", k)]] <- cos(2 * pi * k * test_df$Time / 22)
}

# Add weekly harmonics
train_df$sin_week1 <- sin(2 * pi * train_df$Time / (22 * 7))
train_df$cos_week1 <- cos(2 * pi * train_df$Time / (22 * 7))
test_df$sin_week1 <- sin(2 * pi * test_df$Time / (22 * 7))
test_df$cos_week1 <- cos(2 * pi * test_df$Time / (22 * 7))

# Final model using time and harmonics
final_formula <- as.formula(paste(
  "MeandepDelay ~ Time +",
  paste(c(
    paste0("sin_day", 1:11), paste0("cos_day", 1:11),
    "sin_week1", "cos_week1"
  ), collapse = " + ")
))

harm_model <- lm(final_formula, data = train_df)
summary(harm_model)

# Make point forecast on the test set
test_df$predicted_delay <- predict(harm_model, newdata = test_df)
predicted_delay_ts <- ts(test_df$predicted_delay, start = time(ts_hourly)[3081], frequency = 22)

# Plot: Observed vs Predicted delays on test set only
plot(
  test_df$Time,
  test_df$MeandepDelay,
  type = "l", col = "blue", lwd = 1,
  ylim = c(0, max(test_df$MeandepDelay, na.rm = TRUE)),
  ylab = "Delay", xlab = "Time", main = "Observed vs Predicted (Test Set)"
)
lines(
  test_df$Time,
  predicted_delay_ts,
  col = "red", lwd = 2
)
legend("topright", legend = c("Observed", "Predicted"), col = c("blue", "red"), lty = 1)


# Plot full observed series (blue) and test predictions (red)
train_plot <- train_df %>%
  dplyr::select(Time, MeandepDelay) %>%
  mutate(predicted_delay = NA, set = "Train")

test_plot <- test_df %>%
  dplyr::select(Time, MeandepDelay, predicted_delay) %>%
  mutate(set = "Test")

full_data <- rbind(train_plot, test_plot)

plot(
  full_data$Time,
  full_data$MeandepDelay,
  type = "l", col = "blue", lwd = 1,
  ylim = c(0, max(full_data$MeandepDelay, na.rm = TRUE)),
  ylab = "Delay", xlab = "Time", main = "Observed vs Predicted (Full Series)"
)

lines(
  test_df$Time,
  predicted_delay_ts,
  col = "red", lwd = 2
)

legend("topright", legend = c("Observed", "Predicted"), col = c("blue", "red"), lty = 1)

### SARMA on Harmonic Linear Regression Residuals ####
# Here we fit a SARMA model on the *errors* of the linear model to capture remaining autocorrelations (patterns left behind).

harm_model <- lm(final_formula, data = train_df)
reg_residuals <- residuals(harm_model)
ggtsdisplay(reg_residuals) # In the ACF the residuals show some correlations around lag 20.
                           # Similarly in the PACF
reg_residuals_ts <- ts(reg_residuals ,frequency = 22)
reg_fitted <- fitted(harm_model)
reg_fitted_ts <- ts(reg_fitted, frequency=22)

# Choosing the optimal sarma model
# Testing different parameter combinations (AR and MA lags) to see which fits the residuals best.
a<-Arima(reg_residuals_ts, order = c(1,0,1), seasonal = list(order = c(0,0,1), period = 22))
b<-Arima(reg_residuals_ts, order = c(2,0,1), seasonal = list(order = c(1,0,0), period = 22))
c<-Arima(reg_residuals_ts, order = c(3,0,2), seasonal = list(order = c(2,0,0), period = 22))
d<-Arima(reg_residuals_ts, order = c(4,0,1), seasonal = list(order = c(2,0,0), period = 22))
e<-Arima(reg_residuals_ts, order = c(4,0,1), seasonal = list(order = c(1,0,2), period = 22))  

AIC(a,b,c,d,e) # optimal one: SARMA(4,0,1)(2,0,0)[22]

# SARMA model
sarma_model <- d
checkresiduals(sarma_model) # do not reject the null hypothesis that the residuals are white noise.
pacf(residuals(sarma_model), lag.max = 66)
qqnorm(residuals(sarma_model), main = "", col = "blue") #heavy tails on the right and left side.
qqline(residuals(sarma_model), col = "red", lwd = 1.5)

#forecast regression part
reg_forecast <- predict(harm_model, newdata = test_df)
reg_forecast_ts<- ts(reg_forecast, start=time(ts_hourly)[3081], frequency=22)

#SARMA forecast
h <- length(test)
sarma_forecast <- forecast(sarma_model, h=h)

#combine forecast
# The final prediction is the sum of the linear trend/seasonality prediction + the SARMA error prediction.
combined_forecast <- reg_forecast + sarma_forecast$mean
combined_forecast_ts <- ts(combined_forecast,start=time(ts_hourly)[3081], frequency = 22 )

plot(ts_hourly,xlim=c(145,180),
     main = "Forecast: Harmonic Regression + SARMA Residuals",
     ylab = "Mean Departure Delay", col = "black", lwd = 2)
lines(combined_forecast_ts, col = "red", lwd = 2)
legend("topleft", legend = c("Real", "Forecast"), col = c("black", "red"), lty = 1)


#Bootstrap prediction interval 
# Generating multiple simulations to create confidence intervals (uncertainty bounds) around our forecasts.
B <- 1000
h <- length(test) # 902 obs
y_star <- matrix(NA, B, h)

for (b in 1:B) {
  arima_sim <- simulate(sarma_model, nsim = h)
  arima_sim_vec <- as.numeric(arima_sim)
  
  if (length(arima_sim_vec) == h) {
    y_star[b, ] <- reg_forecast + arima_sim_vec
  } else {
    warning(paste("Iteration", b, "different length:", length(arima_sim_vec)))
  }
}

boot_ub <- apply(y_star, 2, quantile, 0.975)
boot_lb <- apply(y_star, 2, quantile, 0.025)
boot_ub_ts <- ts(boot_ub, start = time(ts_hourly)[3081], frequency = 22)
boot_lb_ts <- ts(boot_lb, start = time(ts_hourly)[3081], frequency = 22)

plot(ts_hourly, xlim=c(145,181), ylim=c(-30,450))
lines(combined_forecast_ts, col = "red", lwd = 2)
polygon(c(time(combined_forecast_ts), rev(time(combined_forecast_ts))),
        c(boot_lb, rev(boot_ub)), col = rgb(1, 0, 0, alpha = 0.1), border = NA)
lines(boot_lb_ts, col = "darkred", lty = 2)
lines(boot_ub_ts, col = "darkred", lty = 2)
legend("topleft", legend = c("Forecast", "Bootstrap PI"), 
       col = c("red", "black"), lty = c(1, 3), bty = "n")

### Garch ####
# GARCH models are typically used to predict and manage changing volatility (variance) over time in a series.

resid_sarma <- residuals(sarma_model) # residual after LM + SARMA
plot(resid_sarma) # Centred around zero
par(mfrow=c(1,2))
qqnorm(resid_sarma)
qqline(resid_sarma, col = "red")
# Modified from Italian: "ACF dei residui al quadrato" -> "ACF of squared residuals"
acf(resid_sarma^2, main = "ACF of squared residuals")
# Modified from Italian: "Densità dei residui" -> "Density of residuals"
hist(resid_sarma, breaks = 50, probability = TRUE, main = "Density of residuals")
lines(density(resid_sarma), col = "blue", lwd = 2)

# Define GARCH(1,1) model using ugarchspec #
spec_garch <- ugarchspec( 
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), #The ugarchspec helps us to specify the variance model, in this case Garch(1,1) 
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),  #Mean already around zero, fixed with LM+ARIMA 
  distribution.model = "snorm"
)

fit_garch <- ugarchfit(spec = spec_garch, data = resid_sarma) #Tells which model has been fitted, omega= constant variance process, alpha1 often close to 1 and beta1 to 0 
plot(fit_garch, which = "all")  # Complete diagnostics
# We can see in plot 9, that the residuals has havier tail, suggesting the Norm distrib may not be good.

show(fit_garch)

resid_garch <- residuals(fit_garch, standardize = TRUE)
par(mfrow=c(1,2))
acf(resid_sarma^2, lag.max = 66) 
acf(resid_garch^2, lag.max = 66) # Do not show significant autocorrelation
Box.test(resid_garch^2, lag = 22, type = "Ljung-Box") 

garch_forecast <- ugarchforecast(fit_garch, n.ahead = h)
sigma_forecast <- sigma(garch_forecast)  # Expected standard deviation
sigma_garch <- sigma(fit_garch)

### SARIMA ####
# SARIMA (Seasonal AutoRegressive Integrated Moving Average) integrates trend and seasonal patterns natively without needing linear regression.
ts_hourly
acf(ts_hourly)
pacf(ts_hourly)
checkresiduals(ts_hourly)

# Split into training and test sets
train_size <- 22 * 140  # 140 days of training data (22 observations per day)
train_sarima <- window(ts_hourly, end = time(ts_hourly)[train_size])
test_sarima <- window(ts_hourly, start = time(ts_hourly)[train_size + 1])

# Fit SARIMA(4,0,1)(2,1,0)[22] model on the training set
a1 <- Arima(train_sarima,order = c(3, 0, 1), seasonal = list(order = c(1, 1, 0), period = 22))
a2 <- Arima(train_sarima,order = c(4, 0, 1), seasonal = list(order = c(2, 1, 0), period = 22))
a3 <- Arima(train_sarima,order = c(4, 0, 1), seasonal = list(order = c(2, 1, 1), period = 22))

AIC(a1,a2,a3) # Optimal one is SARIMA(4,0,1)(2,1,1)[22]
sarima_model <- a3
checkresiduals(sarima_model) #Do not reject H0 residual not correlated (white noise).

# Forecast the same length as the test set
sarima_forecast <- forecast(sarima_model, h = length(test_sarima))

# Combined forecast from LM + SARMA residuals to the test period for comparison
combined_fc <- window(combined_forecast_ts, 
                      start = time(ts_hourly)[3081], 
                      end = time(ts_hourly)[3982])

# Comparative plot: Actual vs SARIMA vs Combined Model
plot(test_sarima, 
     main = "Forecast Comparison", 
     ylab = "Average Delay (Minutes)", 
     xlab = "Hour", 
     col = "black", 
     lwd = 2)

lines(sarima_forecast$mean, col = "blue", lwd = 2) #SARIMA forecasts a little better than the combined model
lines(combined_fc, col = "red", lwd = 2)

legend("topleft", 
       legend = c("Observed", "SARIMA", "LM + SARMA"), 
       col = c("black", "blue", "red"), 
       lty = 1, 
       lwd = 2)

# Extract forecast values and 95% prediction intervals 
forecast_values <- sarima_forecast$mean
lower_95 <- sarima_forecast$lower[, 2]  # 95% lower bound
upper_95 <- sarima_forecast$upper[, 2]  # 95% upper bound

# Convert to time series for plotting
# Start at 141 because training ends at day 140 
forecast_ts <- ts(forecast_values, start = 141, frequency = 22)
lower_95_ts <- ts(lower_95, start = 141, frequency = 22)
upper_95_ts <- ts(upper_95, start = 141, frequency = 22)

# Convert training and test sets to plain numeric series
train_ts <- as.numeric(train_sarima)
test_ts <- as.numeric(test_sarima)

# Plot full SARIMA forecast with 95% prediction interval
plot(train_ts,
     type = "l",
     xlim = c(1, length(train_ts) + length(test_ts)),
     ylim = range(c(train_ts, test_ts, lower_95, upper_95)),
     main = "SARIMA Forecast with 95% Prediction Interval",
     ylab = "Average Delay (Minutes)",
     xlab = "Time (Hourly Observations)",
     col = "black",
     lwd = 1.5)
lines(seq(length(train_ts) + 1, length(train_ts) + length(test_ts)), test_ts, col = "blue", lwd = 1.5)
lines(seq(length(train_ts) + 1, length(train_ts) + length(forecast_ts)), forecast_ts, col = "red", lwd = 1.5)
polygon(c(seq(length(train_ts) + 1, length(train_ts) + length(lower_95)), 
          rev(seq(length(train_ts) + 1, length(train_ts) + length(upper_95)))),
        c(lower_95, rev(upper_95)),
        col = rgb(1, 0, 0, alpha = 0.3), 
        border = NA)
legend("topright",
       legend = c("Training Set", "Test Set", "Forecast", "95% PI"),
       col = c("black", "blue", "red", rgb(1, 0, 0, alpha = 0.3)),
       lty = c(1, 1, 1, NA),
       lwd = c(1.5, 1.5, 1.5, NA),
       pch = c(NA, NA, NA, 15),
       bty = "n")

# Obtain forecast with rolling window #####
# A rolling window evaluates the model over continuous overlapping intervals to robustly test predictive power.
 
# Set the parameters
window_size <- 22 * 140
h <- 22
start_index <- window_size + 1
end_index <- length(ts_hourly) - h + 1
n.forecasts <- end_index - start_index + 1 

# Create empty vectors to save forecasts and intervals
lm_sarma_garch_forecasts <- rep(NA, n.forecasts * h)
lm_sarma_garch_lower <- rep(NA, n.forecasts * h)
lm_sarma_garch_upper <- rep(NA, n.forecasts * h)

ets_forecasts <- rep(NA, n.forecasts * h)
ets_lower <- rep(NA, n.forecasts * h)
ets_upper <- rep(NA, n.forecasts * h)

point_forecast_sarima <- rep(NA, n.forecasts * h)
lower_forecast_sarima <- rep(NA, n.forecasts * h)
upper_forecast_sarima <- rep(NA, n.forecasts * h)

# Function to generate harmonics
add_harmonics <- function(df) {
  for (k in 1:11) {
    df[[paste0("sin_day", k)]] <- sin(2 * pi * k * df$Time / 22)
    df[[paste0("cos_day", k)]] <- cos(2 * pi * k * df$Time / 22)
  }
  df$sin_week1 <- sin(2 * pi * df$Time / (22*7))
  df$cos_week1 <- cos(2 * pi * df$Time / (22*7))
  return(df)
}

# Function Winkler Score
# Winkler Score penalizes prediction intervals (the shaded bounds) that are either too wide or fail to capture the actual values.
winkler_score <- function(lower, upper, true, alpha=0.05){
  width <- upper - lower
  score <- width
  score[true < lower] <- score[true < lower] + 2/alpha * (lower[true < lower] - true[true < lower])
  score[true > upper] <- score[true > upper] + 2/alpha * (true[true > upper] - upper[true > upper])
  return(score)
}

# Loop rolling window
for(i in 1:n.forecasts) {
  train_start <- i
  train_end <- train_start + window_size - 1
  test_start <- train_end + 1
  test_end <- test_start + h - 1
  
  # Subset train and test
  train_data <- ts_hourly[train_start:train_end]
  test_data <- ts_hourly[test_start:test_end]
  
  # Prepare data.frame with Time and target
  train_df <- data.frame(Time = 1:length(train_data), MeandepDelay = as.numeric(train_data))
  test_df <- data.frame(Time = (length(train_data)+1):(length(train_data)+h), MeandepDelay = as.numeric(test_data))
  
  # Add harmonics function
  train_df <- add_harmonics(train_df)
  test_df <- add_harmonics(test_df)
  
  # Linear regression with harmonics
  formula_harm <- as.formula(paste("MeandepDelay ~ Time +", 
                                   paste(c(paste0("sin_day", 1:11), paste0("cos_day", 1:11), "sin_week1", "cos_week1"), collapse = " + ")))
  
  lm_fit <- lm(formula_harm, data = train_df)
  
  # Residuals for lm
  resid_lm <- residuals(lm_fit)
  resid_lm_ts <- ts(resid_lm, frequency = 22)
  
  # Fit SARMA model
  sarma_fit <- Arima(resid_lm_ts, order = c(4,0,1), seasonal = list(order=c(2,0,0), period=22))
  
  # Residuals for GARCH
  resid_sarma <- residuals(sarma_fit)
  
  # GARCH fit
  spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                     distribution.model="norm")
  
  garch_fit <- tryCatch(
    ugarchfit(spec, resid_sarma, solver = "hybrid"),
    error = function(e) NULL
  )
  
  # Forecast LM part
  reg_forecast <- predict(lm_fit, newdata = test_df)
  
  # Forecast SARMA part
  sarima_forecast <- forecast(sarma_fit, h=h)
  
  # Forecast GARCH residual volatility and simulate error distribution
  if(!is.null(garch_fit)){
    garch_forecast <- ugarchforecast(garch_fit, n.ahead=h)
    sigma_forecast <- sigma(garch_forecast)
    
    # Bootstrap residual simulation for intervals
    B <- 500
    sim_matrix <- matrix(NA, nrow=B, ncol=h)
    for(b in 1:B){
      sim_matrix[b,] <- simulate(sarma_fit, nsim=h) + rnorm(h, 0, sigma_forecast)
    }
    
    # Calculate prediction intervals
    lower_bound <- apply(sim_matrix, 2, quantile, probs=0.025) + reg_forecast
    upper_bound <- apply(sim_matrix, 2, quantile, probs=0.975) + reg_forecast
  } else {
    sigma_forecast <- rep(sd(resid_sarima), h)
    lower_bound <- reg_forecast + sarima_forecast$lower[,2]  # 95% lower
    upper_bound <- reg_forecast + sarima_forecast$upper[,2]  # 95% upper
  }
  
  combined_forecast <- reg_forecast + sarma_forecast$mean
  
  # Save results in vectors
  idx <- ((i-1)*h + 1):(i*h)
  lm_sarma_garch_forecasts[idx] <- combined_forecast
  lm_sarma_garch_lower[idx] <- lower_bound
  lm_sarma_garch_upper[idx] <- upper_bound
  
  # ETS model
  ets_fit <- ets(train_data)
  ets_fc <- forecast(ets_fit, h=h)
  
  # Save results in vectors
  ets_forecasts[idx] <- ets_fc$mean
  ets_lower[idx] <- ets_fc$lower[,2]
  ets_upper[idx] <- ets_fc$upper[,2]
  
  # SARIMA model
  train_sarima <- window(ts_hourly, end = time(ts_hourly)[train_size])
  sarima_model <- tryCatch({
    Arima(train_sarima, order = c(4,0,1), seasonal = list(order = c(2,1,1), period = 22))
  }, error = function(e) {
    warning(paste("Error SARIMA at step", i, ":", e$message))
    return(NULL)
  })
  
  if (!is.null(sarima_model)) {
    sarima_fc <- forecast(sarima_model, h = h)
    point_forecast_sarima[idx] <- sarima_fc$mean
    lower_forecast_sarima[idx] <- sarima_fc$lower[, 2]
    upper_forecast_sarima[idx] <- sarima_fc$upper[, 2]
  } else {
    point_forecast_sarima[idx] <- NA
    lower_forecast_sarima[idx] <- NA
    upper_forecast_sarima[idx] <- NA
  }
  
  cat(sprintf("Rolling window %d/%d done\n", i, n.forecasts))
}

### Calculate performance metrics

# Build observed vector
obs_test <- window(ts_hourly, c(161, 1), end = c(181, 22))
obs_test_vec <- as.numeric(obs_test[1:(n.forecasts * h)])

# Calculate Unconditional Coverage (percentage of true values within interval)
uncov_lm <- mean((obs_test_vec >= lm_sarma_garch_lower) & (obs_test_vec <= lm_sarma_garch_upper), na.rm=TRUE)
uncov_ets <- mean((obs_test_vec >= ets_lower) & (obs_test_vec <= ets_upper), na.rm=TRUE)
uncov_sarima <- mean((obs_test_vec >= lower_forecast_sarima) & (obs_test_vec <= upper_forecast_sarima), na.rm=TRUE)

valid_idx <- !(is.na(lm_sarma_garch_lower) | is.na(lm_sarma_garch_upper) | is.na(obs_test_vec))

winkler_lm <- mean(winkler_score(
  lm_sarma_garch_lower[valid_idx],
  lm_sarma_garch_upper[valid_idx],
  obs_test_vec[valid_idx]
))

valid_idx_ets <- !(is.na(ets_lower) | is.na(ets_upper) | is.na(obs_test_vec))

winkler_ets <- mean(winkler_score(
  ets_lower[valid_idx_ets],
  ets_upper[valid_idx_ets],
  obs_test_vec[valid_idx_ets]
))

valid_idx_sarima <- !(is.na(lower_forecast_sarima) | is.na(upper_forecast_sarima) | is.na(obs_test_vec))

winkler_sarima<- mean(winkler_score(
  lower_forecast_sarima[valid_idx_sarima],
  upper_forecast_sarima[valid_idx_sarima],
  obs_test_vec[valid_idx_sarima]
))

# Forecast errors
error_lm <- obs_test_vec - lm_sarma_garch_forecasts
error_ets <- obs_test_vec - ets_forecasts
error_sarima <- obs_test_vec - point_forecast_sarima

# Remove NA
valid_dm_lm_ets <- complete.cases(error_lm, error_ets)
valid_dm_lm_sarima <- complete.cases(error_lm, error_sarima)
valid_dm_ets_sarima <- complete.cases(error_ets, error_sarima)

# Diebold-Mariano test (two-sided)
# This test statistically evaluates if one model's forecasts are significantly better than another's.
dm_lm_ets <- dm.test(error_lm[valid_dm_lm_ets], error_ets[valid_dm_lm_ets], alternative = "two.sided", h = 1, power = 2) #t-test negative, so the first (LR+SARMA+GARCH) model is better
dm_lm_sarima <- dm.test(error_lm[valid_dm_lm_sarima], error_sarima[valid_dm_lm_sarima], alternative = "two.sided", h = 1, power = 2) #t-test positive, so the second (SARIMA) model is better
dm_ets_sarima <- dm.test(error_ets[valid_dm_ets_sarima], error_sarima[valid_dm_ets_sarima], alternative = "two.sided", h = 1, power = 2) #t-test positive, so the second (SARIMA) model is better


# Compute MSE for each model
# Mean Squared Error: Lower is better. Tells us how far off the predictions were from the actuals.
mse_lm <- mean(error_lm^2, na.rm = TRUE)
mse_ets <- mean(error_ets^2, na.rm = TRUE)
mse_sarima <- mean(error_sarima^2, na.rm = TRUE) #lower one

# Output
cat("\n--- Diebold-Mariano Tests ---\n")
cat("LM+SARMA+GARCH vs ETS: t-test =", dm_lm_ets$statistic, "\n") # t-test negative, so the first (LR+SARMA+GARCH) model is better
cat("LM+SARMA+GARCH vs SARIMA: t-test =", dm_lm_sarima$statistic, "\n") # t-test positive, so the second (SARIMA) model is better
cat("ETS vs SARIMA: t-test =", dm_ets_sarima$statistic, "\n")  # t-test positive, so the second (SARIMA) model is better
cat("Unconditional Coverage LM+SARMA+GARCH:", uncov_lm, "\n") # 86.15%
cat("Unconditional Coverage ETS:", uncov_ets, "\n")           # 89.39%
cat("Unconditional Coverage SARIMA:", uncov_sarima, "\n")     # 95.67% SARIMA has the coverage that is most in line with the expected theoretical value → better confidence intervals.
cat("Winkler Score LM+SARMA+GARCH:", winkler_lm, "\n")
cat("Winkler Score ETS:", winkler_ets, "\n")
cat("Winkler Score SARIMA:", winkler_sarima, "\n") # SARIMA produces more accurate and efficient intervals.
cat("MSE LM+SARMA+GARCH:", mse_lm, "\n")
cat("MSE ETS:", mse_ets, "\n")
cat("MSE SARIMA:", mse_sarima, "\n")   # Lower = more precise in predicting point values.

### Plots

# Number of points to display (e.g. 3 days = 66 observations)
n_plot <- n.forecasts
# Observed data for the first n_plot
obs_plot <- obs_test_vec[1:n_plot]
# Corresponding time
time_plot <- (start_index):(start_index + n_plot - 1)

# --- 1) LM + SARIMA + GARCH forecast ---

par(mfrow=c(2,2))
df_lm <- data.frame(
  Time = time_plot,
  Observed = obs_plot,
  Forecast = lm_sarma_garch_forecasts[1:n_plot],
  Lower = lm_sarma_garch_lower[1:n_plot],
  Upper = lm_sarma_garch_upper[1:n_plot]
)

p1 <- ggplot(df_lm, aes(x = Time)) +
  geom_line(aes(y = Observed), color = "black") +
  geom_line(aes(y = Forecast), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  labs(title = "LM + SARMA + GARCH Forecast with 95% PI",
       y = "Mean Departure Delay", x = "Time") +
  theme_minimal()

print(p1)

# --- 2) ETS forecast ---

df_ets <- data.frame(
  Time = time_plot,
  Observed = obs_plot,
  Forecast = ets_forecasts[1:n_plot],
  Lower = ets_lower[1:n_plot],
  Upper = ets_upper[1:n_plot]
)

p2 <- ggplot(df_ets, aes(x = Time)) +
  geom_line(aes(y = Observed), color = "black") +
  geom_line(aes(y = Forecast), color = "darkgreen") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "darkgreen", alpha = 0.2) +
  labs(title = "ETS Forecast with 95% PI",
       y = "Mean Departure Delay", x = "Time") +
  theme_minimal()

print(p2)

# --- 3) SARIMA forecast ---

# Plot complete time series with SARIMA forecast
par(mfrow = c(2, 1))
plot(train_ts,
     type = "l",
     xlim = c(1, length(train_ts) + length(test_ts)),
     ylim = range(c(train_ts, test_ts, lower_95, upper_95)),
     main = "SARIMA Forecast",
     ylab = "Average Delay (Minutes)",
     xlab = "Time (Hourly Observations)",
     col = "black",
     lwd = 1)
lines(seq(length(train_ts) + 1, length(train_ts) + length(test_ts)), test_ts, col = "blue", lwd = 1.5)
lines(seq(length(train_ts) + 1, length(train_ts) + length(forecast_ts)), forecast_ts, col = "red", lwd = 1.5)
polygon(c(seq(length(train_ts) + 1, length(train_ts) + length(lower_95)), 
          rev(seq(length(train_ts) + 1, length(train_ts) + length(upper_95)))),
        c(lower_95, rev(upper_95)),
        col = rgb(1, 0, 0, alpha = 0.3), 
        border = NA)
legend("topleft",
       legend = c("Training Set", "Test Set", "Forecast", "95% PI"),
       col = c("black", "blue", "red", rgb(1, 0, 0, alpha = 0.3)),
       lty = c(1, 1, 1, NA),
       lwd = c(1.5, 1.5, 1.5, NA),
       pch = c(NA, NA, NA, 15),
       bty = "n")

# Zoom on the forecast with 95% PI
plot(df_sarima$Time, df_sarima$Observed, type = "l", col = "black",
     main = "SARIMA Forecast with 95% PI",
     ylab = "Mean Departure Delay", xlab = "Time")
lines(df_sarima$Time, df_sarima$Forecast, col = "red")
polygon(c(df_sarima$Time, rev(df_sarima$Time)),
        c(df_sarima$Lower, rev(df_sarima$Upper)),
        col = rgb(1, 0, 0, alpha = 0.2), border = NA)


## Comparison of point forecast for the 3 models

# Create a data.frame for 881 obs (40 days)
agg_df <- data.frame(
  Time = (start_index):(start_index + 881 - 1),
  Observed = obs_test_vec[1:881],
  LM_SARMA_GARCH = lm_sarma_garch_forecasts[1:881],
  ETS = ets_forecasts[1:881],
  SARIMA = point_forecast_sarima[1:881]
)
agg_long <- agg_df %>%
  pivot_longer(cols = -Time, names_to = "Model", values_to = "Value")

# Plot
ggplot(agg_long, aes(x = Time, y = Value, color = Model, linetype = Model)) +
  geom_line(size = 1) +
  labs(title = " Comparison of point forecast ",
       x = "Time", y = "Mean Departure Delay") +
  scale_color_manual(values = c("Observed" = "black", 
                                "LM_SARMA_GARCH" = "blue", 
                                "ETS" = "darkgreen", 
                                "SARIMA" = "red")) +
  scale_linetype_manual(values = c("Observed" = "solid", 
                                   "LM_SARMA_GARCH" = "dashed", 
                                   "ETS" = "dashed", 
                                   "SARIMA" = "dashed")) +
  theme_minimal()

# Analysis adding a covariate #####
# A covariate is an extra variable that might help explain the delays (in this case, Carrier Delay).

# Manage hourly data ##

# Compute mean departure delay per hour
hourly_delay <- Flight_delay %>%
  mutate(
    Dephour = floor(DepTime / 100),           # Extract hour from HHMM format
    DateTime = Date + hours(Dephour)          # Create full timestamp
  ) %>%
  group_by(DateTime) %>%
  #aggregate departure delays and carrier delay by mean for each hourly interval
  summarise(MeandepDelay = mean(DepDelay), MeancarrDelay = mean(CarrierDelay), na.rm= TRUE, .group="drop") %>%
  arrange(DateTime)

#Create a complete time series by adding all hourly timestamps (missing filled with NA)
hourly_delay_full <- hourly_delay %>%
  complete(DateTime = seq(min(DateTime), max(DateTime), by = "hour"))

# Extract hour (0–23) from timestamp
hourly_delay_full$hour <- hour(hourly_delay_full$DateTime)
print(class(hourly_delay_full))
print(str(hourly_delay_full))

# Count missing values (NA) by hour
total_by_hour <- hourly_delay_full %>%
  count(hour, name = "total_obs")

# Identify which hours in the interval had days without any flights 
missing_by_hour <- hourly_delay_full %>%
  dplyr::filter(is.na(MeandepDelay)) %>%
  dplyr::filter(is.na(MeancarrDelay)) %>%
  dplyr::count(hour, name = "n_missing") %>%
  tidyr::complete(hour = 0:23, fill = list(n_missing = 0)) %>%
  dplyr::left_join(total_by_hour, by = "hour") %>%
  dplyr::mutate(
    perc_missing = round((n_missing / total_obs) * 100, 2)
  ) %>%
  dplyr::arrange(hour)

# Overall missing value statistics
total_missing <- sum(is.na(hourly_delay_full$MeandepDelay), is.na(hourly_delay_full$MeancarrDelay))
total_obs <- nrow(hourly_delay_full)
perc_total_missing <- round((total_missing / total_obs) * 100, 2)

# Remove hours with more than 40% missing data (e.g., 3 AM and 4 AM)
hourly_delay_adj <- hourly_delay_full %>%
  dplyr::filter(!(hour %in% c(3, 4)))

# Select only the variables needed for imputation
mice_data <- hourly_delay_adj %>%
  dplyr::select(MeandepDelay, MeancarrDelay, hour)

# Run MICE with one imputation (m = 1), using predictive mean matching (PMM)
imputed <- mice::mice(mice_data, m = 1, method = "pmm", seed = 123)

# Replace missing values with the imputed ones
hourly_delay_adj$MeandepDelay <- mice::complete(imputed)$MeandepDelay
hourly_delay_adj$MeancarrDelay <- mice::complete(imputed)$MeancarrDelay

# Check that no missing values remain
missing_check <- hourly_delay_adj %>%
  dplyr::filter(is.na(MeandepDelay)) %>%
  dplyr::filter(is.na(MeancarrDelay)) %>%
  dplyr::count(hour)

print(missing_check)

# Plot average carrier delay by hour
plot(hourly_delay_adj$DateTime, hourly_delay_adj$MeancarrDelay, type = "l",
     col = "blue", xlab = "Date", ylab = "Average Departure Delay (minutes)",
     main = "Average Departure Delay per Hour", xaxt = "n")

axis.POSIXct(1,
             at = seq(from = floor_date(min(hourly_delay_adj$DateTime), "month"),
                      to = ceiling_date(max(hourly_delay_adj$DateTime), "month"),
                      by = "1 month"),
             format = "%b", las = 2, cex.axis = 0.8)

# Time series of the carrier delay 

ts_CarrDelay <- ts(hourly_delay_adj$MeancarrDelay, frequency = 22)
plot(ts_CarrDelay, col = "blue", main = "Base Time Series (22-hour cycle)",     # Invastigate the time series searching for possible seasonality, trend, structural breaks 
     ylCov = "Average Carrier Delay", xlCov = "Time")

# Decompose the ts: 
plot(decompose(ts_CarrDelay)) # Presence of a seasonality and there seems to be a deterministic trend

# ADF Test
ur.df(ts_CarrDelay) # Reject the null hypothesis of a unit root (stochastic trend)

# Harmonic linear regression model with Carrier delay

# Create harmonic formula 
create_harmonics <- function(time, period, max_order) {
  harmonics <- data.frame(Time = time)
  for (i in 1:max_order) {
    harmonics[[paste0("sin_", i)]] <- sin(2 * pi * i * time / period)
    harmonics[[paste0("cos_", i)]] <- cos(2 * pi * i * time / period)
  }
  return(harmonics)
}

results <- data.frame(order = integer(), AIC = numeric())
max_order <- 15
period <- 22  # daily periodicity (22 observations per day)


# Create a dataset to manage the models 
df_model <- hourly_delay_adj %>%
  mutate(
    Time = 1:nrow(hourly_delay_adj),
    month = factor(month(DateTime)),
    week = factor(week(DateTime)),
    CarrDel = factor(MeancarrDelay),
    season_dummies = factor(rep(0:22, length.out = nrow(df_model))), # Create the seasonal dummies
    day_of_month = factor(mday(DateTime))
  )

df_model$DateTime <- parse_date_time(
  hourly_delay_adj$DateTime,
  orders = c("ymd HMS", "ymd HM", "ymd", "ymdT", "ymdTz")
)

df_model <- df_model %>%
  mutate(weekday = wday(DateTime, week_start = 1))

df_model$hour <- factor(hour(df_model$DateTime))
df_model$day <- factor(wday(df_model$DateTime, week_start = 1))
df_model$CarrDel <- as.numeric(as.character(df_model$CarrDel))

# Add the dummies to the data frame
dummies <- model.matrix(~ season_dummies, data = df_model)
df_model <- cbind(df_model, dummies)
dummy_vars <- colnames(dummies)

# Split data into training and test sets
n_per_day <- 22
period <- n_per_day
n_days <- 181
train_data <- df_model[df_model$DateTime <= as.POSIXct("2019-05-21 01:00:00"), ]
test_data <- df_model[df_model$DateTime > as.POSIXct("2019-05-21 01:00:00"), ]

# Fit the harmonics 
for (order in 1:max_order) {
  harm <- create_harmonics(train_data$Time, period, order)
  model_data <- cbind(train_data, harm[, -1])  # exclude duplicate Time column
  formula_str <- paste("MeandepDelay ~ CarrDel + Time +", paste(colnames(harm)[-1], collapse = " + "))
  model <- lm(as.formula(formula_str), data = model_data)
  results <- rbind(results, data.frame(order = order, AIC = AIC(model)))
}

# Select the best order based on AIC
best_order <- results[which.min(results$AIC), "order"]
print(results)

# Generate daily harmonics
for (k in 1:11) {
  train_data[[paste0("sin_day", k)]] <- sin(2 * pi * k * train_data$Time / 22)
  train_data[[paste0("cos_day", k)]] <- cos(2 * pi * k * train_data$Time / 22)
  
  test_data[[paste0("sin_day", k)]] <- sin(2 * pi * k * test_data$Time / 22)
  test_data[[paste0("cos_day", k)]] <- cos(2 * pi * k * test_data$Time / 22)
}

# Add weekly harmonics
train_data$sin_week1 <- sin(2 * pi * train_data$Time / (22 * 7))
train_data$cos_week1 <- cos(2 * pi * train_data$Time / (22 * 7))
test_data$sin_week1 <- sin(2 * pi * test_data$Time / (22 * 7))
test_data$cos_week1 <- cos(2 * pi * test_data$Time / (22 * 7))

# Final model using time and harmonics

super.final_formula <- as.formula(paste(
  "MeandepDelay ~ CarrDel + Time +",             # Add carrier delay
  paste(c(
    paste0("sin_day", 1:11), paste0("cos_day", 1:11),
    "sin_week1", "cos_week1"
  ), collapse = " + ")
))

# Harmonic linear regression model
cov.harm_model <- lm(super.final_formula, data = train_data)
summary(cov.harm_model)

# Seasonal dummies Linear regression model with Carrier delay
cov.dummies_model <- lm(MeandepDelay ~ Time + CarrDel+ season_dummies1 + season_dummies2 + season_dummies3 + season_dummies4 
                        + season_dummies5 + season_dummies6 + season_dummies7 + season_dummies8 + season_dummies9 
                        + season_dummies10 + season_dummies11 + season_dummies12 + season_dummies13 + season_dummies14 
                        + season_dummies15 + season_dummies16 + season_dummies17 + season_dummies18 + season_dummies19 
                        + season_dummies20 + season_dummies21 + season_dummies22 -1 , data = train_data)

summary(cov.dummies_model)

# Info criteria
AIC(cov.harm_model, cov.dummies_model) # Lower AIC for the harm_model
BIC(cov.harm_model, cov.dummies_model) # Lower BIC for the harm_model

# Bootstrap forecast for harm model
cov.harm_forecast <- predict(cov.harm_model, newdata = test_data)
ts_cov.harm_forecast<- ts(cov.harm_forecast, start = 140 , end= 182, frequency = 22)
cov.harm_residuals<-  test_data$MeandepDelay - cov.harm_forecast

B <- 1000
h <- nrow(test_data)
cov.harm_star <- matrix(NA, nrow = B, ncol = h)

for (b in 1:B) {
 cov.harm_residuals_star <- sample(cov.harm_residuals, h, replace = TRUE)
  cov.harm_star[b, ] <-  cov.harm_forecast + cov.harm_residuals_star
}

cov.harm.interval_ub <- apply(cov.harm_star, 2, quantile, 0.975)
cov.harm.interval_lb <- apply(cov.harm_star, 2, quantile, 0.025)
ts_cov.harm.interval_ub <- ts(cov.harm.interval_ub, start = 140, end= 182, frequency = 22)
ts_cov.harm.interval_lb <- ts(cov.harm.interval_lb, start = 140, end= 182, frequency = 22)

# Plot forecast
plot(ts_hourly, xlim=c(145,181), ylim=c(-30,600))
lines(ts_cov.harm_forecast, col = "red", lwd = 2)
polygon(c(time(ts_cov.harm_forecast), rev(time(ts_cov.harm_forecast))),
        c(ts_cov.harm.interval_lb, rev(ts_cov.harm.interval_ub)), col = rgb(1, 0, 0, alpha = 0.1), border = NA)
lines(ts_cov.harm.interval_lb, col = "darkred", lty = 2)
lines(ts_cov.harm.interval_ub, col = "darkred", lty = 2)
legend("topleft", legend = c("Forecast", "intervalstrap PI"), 
       col = c("red", "black"), lty = c(1, 3), bty = "n")

### SARMA on Harmonic Linear Regression Residuals ###

# Check residuals of harm model
cov.mod_res <- residuals(cov.harm_model)
ts_cov.mod_res <- ts(cov.mod_res, frequency = 22)
ggtsdisplay(cov.mod_res)
cov.mod_fitt <- fitted(cov.harm_model)
ts_cov.mod_fitt <- ts(cov.mod_fitt, frequency=22)
ggtsdisplay(cov.mod_fitt)

# SARMAX model 
a1<-Arima(cov.mod_res, order = c(1,0,1), seasonal = list(order = c(0,0,1), period = 22))
b2<-Arima(cov.mod_res, order = c(2,0,1), seasonal = list(order = c(0,0,1), period = 22))
c3<-Arima(cov.mod_res, order = c(1,0,2), seasonal = list(order = c(1,0,2), period = 22))
d4<-Arima(cov.mod_res, order = c(4,0,1), seasonal = list(order = c(2,0,0), period = 22))
e5<-Arima(cov.mod_res, order = c(2,0,0), seasonal = list(order = c(0,0,2), period = 22))  

# Info criteria 
AIC(a1,b2,c3,d4,e5) # Lower AIC for SARMA (2,0,0)(0,0,2)[22]
BIC(a1,b2,c3,d4,e5) # Lower BIC for SARMA (2,0,0)(0,0,2)[22]

arma_cov.model <- e5
cov.arma.res<- residuals(arma_cov.model)
checkresiduals(arma_cov.model) # Ljung-Box test p-value > 0,05 we do not reject the null hypotesis (not autocorrelated)
pacf(cov.arma.res, lag.max = 66)
qqnorm(cov.arma.res, main = "", col = "blue") # It doesn't show a normal distribution, heavy tails 
qqline(cov.arma.res, col = "red", lwd = 1.5)

# SARMA forecast
h=nrow(test_data)
cov.arma_forecast <- forecast(arma_cov.model, h=h)

# Combine SARMA forecast with harmonic lr forecast
combined_cov.forecast <- cov.harm_forecast + cov.arma_forecast$mean
ts_combined_cov.forecast <- ts(combined_cov.forecast, start = 141, frequency = 22 )

plot(ts_hourly,xlim=c(145,181),
     main = "Forecast: Harmonic Regression + SARIMA Residuals",
     ylCov = "Mean Departure Delay", col = "black", lwd = 2)
lines(ts_combined_cov.forecast, col = "red", lwd = 2)
legend("topleft", legend = c("Real", "Forecast"), col = c("black", "red"), lty = 1)


# Bootstrap forecast for the SARMAX
B <- 1000
h <- nrow(test_data)

cov.arma_star <- matrix(NA, nrow = B, ncol = h)

for (b in 1:B) {
  cov.arma_sim <- simulate(arma_cov.model, nsim = h)
  cov.arma_star[b, ] <- cov.harm_forecast + cov.arma_sim
}

cov.arma.interval_ub <- apply(cov.arma_star, 2, quantile, 0.975)
cov.arma.interval_lb <- apply(cov.arma_star, 2, quantile, 0.025)
ts_cov.arma.interval_ub <- ts(cov.arma.interval_ub, start=c(141, 1),
                               frequency = 22)
ts_cov.arma.interval_lb <- ts(cov.arma.interval_lb, start=c(141, 1), 
                               frequency = 22)

plot(ts_hourly, xlim=c(145,181), ylim=c(-30,600))
lines(ts_combined_cov.forecast, col = "red", lwd = 2)
polygon(c(time(ts_combined_cov.forecast), rev(time(ts_combined_cov.forecast))),
        c(ts_cov.arma.interval_lb, rev(ts_cov.arma.interval_ub)), 
        col = rgb(1, 0, 0, alpha = 0.1), border = NA)
lines(ts_cov.arma.interval_lb, col = "darkred", lty = 2)
lines(ts_cov.arma.interval_ub, col = "darkred", lty = 2)
legend("topleft", legend = c("Forecast", "intervalstrap PI"), 
       col = c("red", "black"), lty = c(1, 3), bty = "n")

# Residuals SARMAX
resid_cov.arma <- residuals(arma_cov.model)  
plot(resid_cov.arma) # There is room for improvement to manage volatility of residuals with GARCH

### Garch ##

# Define GARCH(1,1) model using rugarch #
spec_garch <- ugarchspec( 
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), # Specify the variance model, in this case Garch(1,1) 
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),    #Mean already around zero
  distribution.model = "ged"
)

fit_cov.garch <- ugarchfit(spec = spec_garch, data = resid_cov.arma)

plot(fit_cov.garch, which = "all")

show(fit_cov.garch)

resid_cov.garch <- residuals(fit_cov.garch, standardize = TRUE)
acf(resid_cov.garch^2, lag.max = 66)
Box.test(resid_cov.garch^2, lag = 66, type = "Ljung") 
cov.garch_forecast <- ugarchforecast(fit_cov.garch, n.ahead = h)
cov.sigma_forecast <- sigma(cov.garch_forecast)  
cov.sigma_garch <- sigma(fit_cov.garch)