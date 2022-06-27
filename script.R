library(readxl)
library(dplyr)
library(forecast)
library(aTSA)
library(tseries)
library(stringr)

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
gt_inf_s <- gt_inf_s[-1] 
gt_inf_s <- ts(gt_inf_s, frequency = 12, start = c(rok, mesic + 1))

model <- lm(inf_classic_fd ~ gt_inf_s)
summary(model)


plot(gt_inf_s, as.vector(inf_classic_fd), xlim = c(-1,4), ylim = c(-1,4))
abline(model$coefficients[1], model$coefficients[2])

plot(as.vector(inf_classic_fd), model$fitted.values, xlim = c(-1,3), ylim = c(-1,3))
abline(0, 1)


plot.ts(inf_classic_fd, main = "Predikce a inflace")
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
# 4) cointegrace a johansenuv test

library(urca)

inf_cpm_s <- inf_cpm_s[-1]
inf_cpm_s <- ts(inf_cpm_s, frequency = 12, start = c(rok, mesic + 1))

pomocny_df <- data.frame(inf_cpm_s, gt_inf_s)
jotest <- urca::ca.jo(pomocny_df, type = "eigen", K = 2, ecdet = "none", spec = "longrun")
summary(jotest)
restrikce <- c(1, -1)
bh5lrtest(jotest, H = restrikce, r = 2)


















H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H51 <- c(1, -1, -1, 0, 0)
H52 <- c(0, 0, 0, 1, -1)
summary(bh5lrtest(H1, H=H51, r=1))




data(UKpppuip)
attach(UKpppuip)
dat1 <- cbind(p1, p2) #, e12, i1, i2)
dat2 <- cbind(doilp0, doilp1)
H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H51 <- c(1, -1) #, -1, 0, 0)
H52 <- c(0, 0, 0, 1, -1)
summary(bh5lrtest(H1, H=H51, r=2))
summary(bh5lrtest(H1, H=H52, r=2))






