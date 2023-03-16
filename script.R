library(readxl)
library(dplyr)
library(forecast)
library(aTSA)
library(tseries)
library(stringr)
library(lmtest)
library(rugarch)


rm(list = ls())

setwd("C:/Users/jsuch/Desktop/diploma") #set your working directory

# Data o inflaci, pristup:
# https://www.czso.cz/csu/czso/inflation_rate

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



inf_fd      <- data[[1]] %>% t %>% unlist %>% as.vector
inf_cmpy    <- data[[2]] %>% t %>% unlist %>% as.vector
inf_cpm     <- data[[3]] %>% t %>% unlist %>% as.vector
inf_classic <- data[[4]] %>% t %>% unlist %>% as.vector

inf_fd      <- ts(inf_fd, frequency = 12, start = c(2000, 1))
inf_cmpy    <- ts(inf_cmpy, frequency = 12, start = c(1997, 1)) # inf, corresponding month preceeding year
inf_cpm     <- ts(inf_cpm, frequency = 12, start = c(1997, 1))  # inf, corresponding preceeding month
inf_classic <- ts(inf_classic, frequency = 12, start = c(1998, 1))


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
gt_inf_s <- gt_inf
plot(gt_inf)
tseries::adf.test(gt_inf)



######################################################################################################
# 3) Inflation and stationarity

start <- c(2004, 1)
end   <- c(2022, 4)

# 3.1) 

inf_fd_s <- window(inf_fd,  start = start, end = end)
plot(inf_fd_s) 
tseries::adf.test(inf_fd_s)

inf_sd_s <- diff(inf_fd_s)
plot(inf_sd_s)
tseries::adf.test(inf_sd_s)

# 3.2)

inf_cmpy_s <- window(inf_cmpy, start = start, end = end)
plot(inf_cmpy_s)
tseries::adf.test(inf_cmpy_s)

inf_cmpy_s_sd <- diff(inf_cmpy_s)
plot(inf_cmpy_s_sd)
tseries::adf.test(inf_cmpy_s_sd)


# 3.3)

inf_cpm_s <- window(inf_cpm, start = start, end = end)
plot(inf_cpm_s)
tseries::adf.test(inf_cpm_s)

inf_cpm_s_sd <- diff(inf_cpm_s)
plot(inf_cpm_s_sd)
tseries::adf.test(inf_cpm_s_sd)

# 3.4)

inf_classic_s <- window(inf_classic, start = start, end = end) #subsetuju podle gt
inf_classic_s <- inf_classic_s * 100 / inf_classic_s[1]        #preskaluju aby base year



# first difference

inf_classic_fd <- diff(inf_classic_s)
plot(inf_classic_fd)
tseries::adf.test(inf_classic_fd)

tseries::kpss.test(inf_classic_fd, null = c("Level", "Trend"))
Box.test(inf_classic_fd, type = "Ljung-Box")

#second difference - not finished
inf_classic_sd <- diff(inf_classic_fd)

plot(inf_classic_sd)
tseries::adf.test(inf_classic_sd_s)






######################################################################################################
# 4) modelling

# 4.1) indian paper

gt_inf_s <- gt_inf
gt_inf_s
gt_inf_s <- gt_inf_s / mean(gt_inf_s) * mean(inf_cpm_s)
gt_inf_s 
gt_inf_s <- ts(gt_inf_s, frequency = 12, start = c(rok, mesic + 1))

model <- lm(inf_cpm_s ~ gt_inf_s)
summary(model)


plot(gt_inf_s, as.vector(inf_cpm_s), xlim = c(-1,4), ylim = c(-1,4))
abline(model$coefficients[1], model$coefficients[2])

plot(as.vector(inf_cpm_s), model$fitted.values, xlim = c(-1,3), ylim = c(-1,3))
abline(0, 1)


plot.ts(inf_cpm_s, main = "Predikce a inflace")
x <- ts(model$fitted.values, frequency = 12, start = c(rok, mesic + 1))
#lines(x, col = "red")
lines(gt_inf_s, col = "blue")









inf_classic_s <- window(inf_classic, start = start, end = end)
inf_classic_s <- inf_classic_s / inf_classic_s[1] * 100
inf_classic_s_fd <- diff(inf_classic_s)
plot(inf_classic_s_fd)

gt_inf_s <- window(gt_inf, start = start, end = end)
gt_inf_s <- gt_inf_s * 100 / gt_inf_s[1]
plot(gt_inf_s)
gt_inf_s <- gt_inf_s[-1]

model <- lm(inf_classic_s_fd ~ gt_inf_s)
summary(model)








######################################################################################################
# 4.2) granger causality


gt_inf_s <- ts(gt_inf_s, frequency = 12, start = c(rok, mesic + 1))
lmtest::grangertest(inf_cpm_s, gt_inf_s, order = 3)




model <- lm(inf_cpm_s ~ gt_inf_s)
summary(model)

plot(inf_cpm_s, gt_inf_s)


plot(gt_inf_s)
plot(inf_cpm_s)

s_gt_inf_s <- decompose(gt_inf_s, "multiplicative")$random
plot(s_gt_inf_s)

s_inf_cpm_s <- decompose(inf_cpm_s, "multiplicative")$random
plot(s_inf_cpm_s)


lmtest::grangertest(s_gt_inf_s, s_inf_cpm_s, order = 1)
lmtest::grangertest(s_gt_inf_s, s_inf_cpm_s, order = 2)


length(s_inf_cpm_s)
length(inf_cpm_s)
length(gt_inf_s)


tseries::adf.test(s_gt_inf_s[7:213])
tseries::adf.test(s_inf_cpm_s[7:213])





# Load the necessary packages
library(forecast)

s_inf_cpm_s <- s_inf_cpm_s[-1]

# Convert the data to time series format
ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
ts_gt_inf <- ts(data = s_gt_inf_s, start = c(2004, 1), frequency = 12)

# Fit the ARIMA(1,1,0) model to the data
arima_model <- Arima(ts_real_inf, order = c(1,1,0), xreg = as.matrix(data.frame(ts_real_inf, s_gt_inf = ts_gt_inf)[, "s_gt_inf"]))

# Generate the forecasted values for the next 12 months
forecast_data <- forecast(arima_model, xreg = as.matrix(data.frame(ts_real_inf, s_gt_inf = ts_gt_inf)[, "s_gt_inf"]), h = 12)

# Plot the actual and forecasted values
plot(forecast_data, main = "ARIMA(1,1,0) Forecast for Inflation",
     xlab = "Time", ylab = "Real Inflation")
lines(ts_real_inf, col = "blue")
legend("topleft", legend = c("Actual", "Forecast"), lty = c(1,1), col = c("blue", "black"))






















# Load required packages
install.packages("forecast")
library(forecast)

# Generate sample data
set.seed(123)
n <- 100
y <- ts(rnorm(n))
x1 <- rnorm(n)
x2 <- rnorm(n)

# Create a matrix of additional regressors
X <- cbind(x1, x2)

# Fit the ARIMA model with additional regressors
arima_model <- auto.arima(y, xreg = X)

# Display the model
print(arima_model)


























s_gt_inf_s[1:212]



H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H51 <- c(1, -1, -1, 0, 0)
H52 <- c(0, 0, 0, 1, -1)
summary(bh5lrtest(H1, H=H51, r=1))




data(UKpppuip)
attach(UKpppuip)
dat1 <- cbind(p1, p2, e12, i1, i2)
dat2 <- cbind(doilp0, doilp1)
H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H51 <- c(1, -1, -1, 0, 0)
H52 <- c(0, 0, 0, 1, -1)
summary(bh5lrtest(H1, H=H51, r=2))
summary(bh5lrtest(H1, H=H52, r=2))





data(UKpppuip)
attach(UKpppuip)
dat1 <- cbind(p1, p2)
H1 <- ca.jo(dat1, type='trace', K=2)
H51 <- c(1, -1)
summary(bh5lrtest(H1, H=H51, r=1))


######################################################################################################
# Modelling

inf <- inf_cpm_s
exter_reg <- gt_inf_s











# Arima
m_arima <- forecast::Arima(inf,  order = c(1,1,1), xreg = exter_reg)
#summary(m_arima)


# Realized Garch
real_garchspec <- ugarchspec(variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)),
                             external.regressors = NULL, 
                             mean.model = list(armaOrder=c(0, 0), include.mean=TRUE))
real_garch_fit <- ugarchfit(real_garchspec, amt$ret, solver = 'hybrid', realizedVol = sqrt(amt$RV))    
real_garch_fit


# arma-garch
p_max <- 5
q_max <- 5
aic_min <- Inf
best_p <- 0
best_q <- 0

matice <- matrix(, ncol = 4)
for (i1 in 1:p_max) {
  for (i2 in 1:q_max) {
    model_specification <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                                      variance.model = list(garchOrder = c(i1, i2)))
    fit <- ugarchfit(spec = model_specification, data = amt$ret)
    inf_crit <- infocriteria(fit)[1] # 1 for aic, 2 for bic,..
    aic_min <- ifelse(inf_crit < aic_min, inf_crit, aic_min)
    
    vektor <- c(i1, i2, infocriteria(fit)[1], infocriteria(fit)[2])
    matice <- rbind(matice, vektor)
    
    best_p <- ifelse(inf_crit == aic_min, i1, best_p)
    best_q <- ifelse(inf_crit == aic_min, i2, best_q)
  }
} 

c(best_p, best_q)
colnames(matice) <- c("p", "q", "aic", "bic")
matice



# 4.1) cointegrace a johansenuv test

library(urca)

inf_cpm_s <- inf_cpm_s[-1]
inf_cpm_s <- ts(inf_cpm_s, frequency = 12, start = c(rok, mesic + 1))

pomocny_df <- data.frame(inf_cpm_s, gt_inf_s)

pomocny_df$x <- rnorm(nrow(pomocny_df))

jotest <- urca::ca.jo(pomocny_df, type = "trace", K = 5, ecdet = "none", spec = "transitory")
summary(jotest)
restrikce <- c(-1, 1, 3)
summary(urca::bh5lrtest(jotest, H = restrikce, r = 2))

for (i in 1:100){
  try(print(summary(bh5lrtest(jotest, H = restrikce, r = i))))
  print(i)
}








