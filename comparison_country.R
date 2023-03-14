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


# Convert each column of the data frame to a time series and remove seasonality
for (i in 1:ncol(gt)) {
  ts_data <- ts(gt[, 1], start = c(2004, 1), frequency = 12)
  ts_data <- window(ts_data, start = start, end = end)
  decomp_data <- decompose(ts_data)
  detrended_data <- decomp_data$random #+ decomp_data$trend #trend #seasonal
  ts_detrended_data <- ts(detrended_data, start = start, end = end, frequency = 12)
  # ts_detrended_data <- ts_detrended_data[-length(ts_detrended_data)]
  cor_value <- cor(ts_detrended_data, ts_real_inf, use = "pairwise.complete.obs")
  print(paste0("Correlation for column ", i - 1, ": ", cor_value))
  print(ts_detrended_data[1:30])
}









ts_deu
View(ts_inflation)

