library(readxl)
library(dplyr)
library(forecast)
library(aTSA)
library(tseries)
library(stringr)
library(lmtest)
library(rugarch)


rm(list = ls())


######################################################################################################
# 1)  understanding the data about inflation, transforming them



data <- list()

pocet_sheetu <- 4

for (i in 1:pocet_sheetu) {
  data[[i]] <- read_excel("inflace.xlsx", sheet = i)
}

data[[2]] %>% head

for (i in 1:pocet_sheetu) {
  x <- data[[i]]
  y <- data.frame(x[,-1], row.names = x$Year)
  data[[i]] <- y
  
}

for (i in 1:pocet_sheetu) {
  print(i)
  print(data[[i]] %>% head)
  print(data[[i]] %>% dim)
}

data[[4]] <- data[[4]][,-13] # remove aggregate column nebo co to je

inf_cpm     <- data[[3]] %>% t %>% unlist %>% as.vector
inf_cpm     <- ts(inf_cpm, frequency = 12, start = c(1997, 1))  # inf, corresponding preceeding month
inf_cpm
plot(inf_cpm)

######################################################################################################
# 2) Google trends data

gt <- read.csv("search_trends.csv", row.names = 1)
gt %>% head
gt%>% dim

start <- rownames(gt)[1]
rok   <- as.numeric(substr(start, 1, 4))
mesic <- as.numeric(substr(start, 6, 7))
start <- c(rok, mesic)
print(start)
end   <- c(2022, 4)





gt_inf   <- ts(gt$inflace, frequency = 12, start = start)
plot(gt_inf)
tseries::adf.test(gt_inf)


######################################################################################################
# Seasonality


s_inf_cpm <- decompose(inf_cpm, "multiplicative")$random
plot(s_inf_cpm)

s_gt_inf <- decompose(gt_inf, "multiplicative")$random
plot(s_gt_inf)


######################################################################################################
# Subsetting, Granger, adjustment of GT

s_inf_cpm_s <- ts(s_inf_cpm, frequency = 12, start = c(rok, mesic + 1), end = end)
s_gt_inf_s <- ts(s_gt_inf, frequency = 12, start = c(rok, mesic + 1), end = end)

length(s_inf_cpm_s)
length(s_gt_inf_s)

s_gt_inf_s <- s_gt_inf_s / mean(s_gt_inf_s, na.rm = TRUE) * mean(s_inf_cpm_s, na.rm = TRUE)


s_inf_cpm_s
s_gt_inf_s

plot(s_inf_cpm_s)
plot(s_gt_inf_s)

lmtest::grangertest(s_gt_inf_s, s_inf_cpm_s, order = 1)
lmtest::grangertest(s_gt_inf_s, s_inf_cpm_s, order = 2)


######################################################################################################
# Arima

# Load the necessary packages
library(forecast)

# Convert the data to time series format
ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
ts_gt_inf <- ts(data = s_gt_inf_s, start = c(2004, 1), frequency = 12)

# Fit the ARIMA(1,1,0) model to the data
arima_model <- forecast::Arima(ts_real_inf, order = c(1,1,1), xreg = ts_gt_inf)
summary(arima_model)

acf(arima_model)
pacf(arima_model)


# Generate the fitted values
fitted_values <- arima_model$fitted

# Plot the actual and fitted values
plot(ts_real_inf, main = "ARIMA(1,1,0) Fitted Values for Inflation",
     xlab = "Time", ylab = "Real Inflation")
lines(fitted_values, col = "red")
legend("topleft", legend = c("Actual", "Fitted"), lty = c(1,1), col = c("black", "red"))



######################################################################################################
# Load the necessary packages
library(vars)

# Load the necessary packages
library(vars)

# Convert the data to time series format
ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
ts_gt_inf <- ts(data = s_gt_inf_s, start = c(2004, 1), frequency = 12)

# Combine the time series into a matrix
data_matrix <- cbind(ts_real_inf, ts_gt_inf)

complete_matrix <- data_matrix[complete.cases(data_matrix), ]

# Fit a VAR model with lag length 1
var_model <- vars::VAR(complete_matrix, p = 1, type = "const")

# Print a summary of the model
summary(var_model)

# Plot the impulse response function for the Google Trends variable
irf_plot <- plot(vars::irf(var_model, impulse = "ts_gt_inf", response = "ts_real_inf", n.ahead = 12))


# Convert each column of the data frame to a time series and remove seasonality
for (i in 1:ncol(gt)) {
  ts_data <- ts(gt[, i], start = c(2000, 1), frequency = 12)
  decomp_data <- decompose(ts_data)
  detrended_data <- decomp_data$x #+ decomp_data$trend #trend #seasonal
  ts_detrended_data <- ts(detrended_data, start = start, end = end, frequency = 12)
  ts_detrended_data <- ts_detrended_data[-length(ts_detrended_data)]
  cor_value <- cor(ts_detrended_data, ts_real_inf, use = "pairwise.complete.obs")
  print(paste0("Correlation for column ", i - 1, ": ", cor_value))
  print(ts_detrended_data[1:30])
}






























# Nahodny nesmysly
gt_inf_s <- s_gt_inf
gt_inf_s
gt_inf_s <- gt_inf_s / mean(gt_inf_s) * mean(inf_cpm_s)
gt_inf_s 
gt_inf_s <- ts(gt_inf_s, frequency = 12, start = c(rok, mesic + 1))














