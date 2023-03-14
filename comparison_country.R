library(readxl)
library(dplyr)
library(forecast)
library(aTSA)
library(tseries)
library(stringr)
library(lmtest)
library(rugarch)
library(readxl)

inflation_data <- read_excel("deutschland_mom.xlsx")

View(inflation_data)

inflation_data <- inflation_data[,-1]

inflation_data <- as.data.frame(inflation_data)

deu <- t(inflation_data)

deu <- as.vector(deu)
deu <- gsub(pattern = "\\.\\.", replacement = "NA", x = deu)
deu <- as.numeric(deu)


ts_deu <- ts(deu, start = c(2005, 1), end = c(2023, 12), frequency = 12)

start <- c(2006, 1)
end <- c(2022, 4)


ts_deu_s <- window(ts_deu, start = c(2006, 1), end = c(2022, 4))
plot.ts(ts_deu_s)
ts_deu_s %>% class()
#ts_deu_s <- as.numeric(ts_deu_s)
ts_deu_s

ts_deu_sd <- decompose(ts_deu_s, "multiplicative")$random
ts_deu_sd
plot(ts_deu_sd)


gt <- read.csv("search_trends_de.csv", row.names = 1)
gt %>% head
gt%>% dim


decompose(ts_deu)

data_frames <- list()
g <- 4
# Convert each column of the data frame to a time series and remove seasonality
for (i in 1:ncol(gt)) {
  ts_data <- ts(gt[, g], start = c(2006, 1), frequency = 12)
  ts_data <- window(ts_data, start = start, end = end)
  decomp_data <- decompose(ts_data)
  detrended_data <- decomp_data$random #+ decomp_data$trend #trend #seasonal
  ts_detrended_data <- ts(detrended_data, start = start, end = end, frequency = 12)
  # ts_detrended_data <- ts_detrended_data[-length(ts_detrended_data)]
  m <- matrix(c(ts_deu_sd, ts_detrended_data), ncol = 2)
  cor_value <- cor(m[,1], m[,2], use = "pairwise.complete.obs")#, use = "pairwise.complete.obs")
  print(paste0("Correlation for column ", i - 1, ": ", cor_value, colnames(gt)[g]))
  #print(ts_detrended_data[1:30])
  data_frames[[paste0("df_", g)]] <- ts_detrended_data
}



######################################################################################################
# Arima

# Load the necessary packages
library(forecast)

# Convert the data to time series format
#ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
#ts_gt_inf <- ts(data = s_gt_inf_s, start = c(2004, 1), frequency = 12)





explanatory <- ts(as.numeric(as.vector(data_frames[4])), start = start, frequency = 12)
explanatory <- as.matrix(as.numeric(data_frames[4]))
# Fit the ARIMA(1,1,0) model to the data
arima_model <- forecast::Arima(ts_deu_sd, order = c(1,1,1), xreg = ts_detrended_data)
summary(arima_model)

acf(arima_model)
pacf(arima_model)


# Generate the fitted values
fitted_values <- arima_model$fitted

# Plot the actual and fitted values
plot(ts_deu_sd, main = "ARIMA(1,1,0) Fitted Values for Inflation",
     xlab = "Time", ylab = "Real Inflation")
lines(fitted_values, col = "red")
legend("topleft", legend = c("Actual", "Fitted"), lty = c(1,1), col = c("black", "red"))



######################################################################################################
# Load the necessary packages
library(vars)

# Load the necessary packages
library(vars)

# Convert the data to time series format
#ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
#ts_gt_inf <- ts(data = s_gt_inf_s, start = c(2004, 1), frequency = 12)

# Combine the time series into a matrix
data_matrix <- cbind(ts_deu_sd, ts_detrended_data)

complete_matrix <- data_matrix[complete.cases(data_matrix), ]

# Fit a VAR model with lag length 1
var_model <- vars::VAR(complete_matrix, p = 1, type = "const")

# Print a summary of the model
summary(var_model)

# Plot the impulse response function for the Google Trends variable
irf_plot <- plot(vars::irf(var_model, impulse = "ts_gt_inf", response = "ts_real_inf", n.ahead = 12))
