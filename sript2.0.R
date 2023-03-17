library(readxl)
library(dplyr)
library(forecast)
library(aTSA)
library(tseries)
library(stringr)
library(lmtest)
library(rugarch)
library(xts)
library(stats)
library(crayon)
library(vars)


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

rownm <- as.Date(rownames(gt), format = "%Y-%d-%m")
rownm <- format(rownm, "%Y-%m-%d")
start <- rownames(gt)[1]
rok   <- as.numeric(substr(start, 1, 4))
mesic <- as.numeric(substr(start, 6, 7))
start <- c(rok, mesic)
print(start)
end   <- c(2022, 2)


# Convert all columns to numeric
gt_numeric <- data.frame(lapply(gt, as.numeric))

# Remove seasonality from the data
gt_deseasonalized <- data.frame(lapply(gt_numeric, function(x) {
  stl_model <- stl(ts(x, frequency = 12), s.window = "periodic")
  deseasonalized <- stl_model$time.series[, "remainder"]
  return(deseasonalized)
}))

# Print the deseasonalized data
row.names(gt_deseasonalized) <- rownm
print(gt_deseasonalized)

for (i in 1:ncol(gt_deseasonalized)) {
  plot(gt_deseasonalized[,i], ylab = colnames(gt_deseasonalized)[i])
  tseries::adf.test(gt_deseasonalized[,i])
}


head(gt_deseasonalized)
tail(gt_deseasonalized)

gt_dss <- gt_deseasonalized
gt_dss <- as.matrix(gt_dss)


######################################################################################################
# Seasonality u inflace


ts_data <- na.omit(inf_cpm)



ts_decomposed <- stl(ts_data, s.window = "periodic")

# Subtract the seasonal component from the original time series to create the deseasonalized time series
ts_deseasonalized <- ts_data - ts_decomposed$time.series[, "seasonal"]

# Plot the original time series and the deseasonalized time series
par(mfrow = c(2, 1))
plot(ts_data, main = "Original Time Series")
plot(ts_deseasonalized, main = "Deseasonalized Time Series")


######################################################################################################
# Subsetting, Granger, adjustment of GT

s_inf_cpm_s <- window(ts_deseasonalized, frequency = 12, start = c(rok, mesic), end = end)

for (i in 1:ncol(gt_dss)) {
  x <- ts(gt_dss[, i], start = start, frequency = 12)
  for (j in 1:3) {
    y<- lmtest::grangertest(s_inf_cpm_s, x, order = j)
    print(y)
    print(colnames(gt_dss)[i])
  }
}


######################################################################################################
# PCA

pca <- prcomp(gt_dss, scale. = TRUE)

# Print PCA results (eigenvalues, eigenvectors, and standard deviations)
print(pca)

index <- 0
for (i in 1:length(pca$sdev)) {
  print(i)
  if (pca$sdev[i] > 1){
    index <- index + 1
  }
}

gt_dss <- cbind(gt_dss, pca$x[,c(1:index)])
View(gt_dss)







######################################################################################################
# Arima


# Convert the data to time series format
ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)

pocet_ss <- 0
for (i in 1:ncol(gt_dss)) {
  print(i)
  regresor <- ts(data = gt_dss[,i], start = c(2004, 1), end = end, frequency = 12)
  regresor <- regresor / mean(regresor) * mean(ts_real_inf)
  arima_model <- forecast::Arima(ts_real_inf, order = c(1,1,1), xreg = regresor)
  summary(arima_model) %>% print
  
  se_coef <- sqrt(diag(arima_model$var.coef))[3]
  co <- arima_model$coef[3]
  ss <- round(co/ se_coef, digits = 2)
  p_value <- round(2 * (1 - pnorm(abs(ss))), digits = 4)
  print(p_value)
  if (p_value < 0.1){
    pocet_ss <- pocet_ss + 1
    b <- colnames(gt_dss)[i]
    cat(red(b))
    
    #grafika
    fitted_values <- arima_model$fitted
    # Generate the fitted values
    fitted_values <- arima_model$fitted
    
    # Plot the actual and fitted values
    plot(ts_real_inf, main = "ARIMA(1,1,1) Fitted Values for Inflation",
         xlab = "Time", ylab = b)
    lines(fitted_values, col = "red")
    legend("topleft", legend = c("Actual", "Fitted", as.character(p_value), 
                                 as.character(b), as.character(end)), lty = c(1,1), col = c("black", "red", "blue", "red", "purple", "purple"))
  } else {
    print(colnames(gt_dss)[i])
  }
}

print(pocet_ss)








######################################################################################################
# VAR model


for (i in 1:ncol(gt_dss)) {
  print(i)
  b <- colnames(gt_dss)[i]
  print(b)
  
  var_regresor <- ts(gt_dss[,i], start = start, frequency = 12)
  var_regresor <- window(var_regresor, start = start, end = end, frequency = 12)
  for (j in 1:1) {
    data_matrix <- cbind(ts_real_inf, var_regresor)
    complete_matrix <- data_matrix[complete.cases(data_matrix), ]
    var_model <- vars::VAR(complete_matrix, p = j, type = "const")
    summary(var_model) %>% print
  }
}



summary(var_model)



# Plot the impulse response function for the Google Trends variable
irf_plot <- plot(vars::irf(var_model, impulse = "ts_gt_inf", response = "ts_real_inf", n.ahead = 12))






















# Nahodny nesmysly
gt_inf_s <- s_gt_inf
gt_inf_s
gt_inf_s <- gt_inf_s / mean(gt_inf_s) * mean(inf_cpm_s)
gt_inf_s 
gt_inf_s <- ts(gt_inf_s, frequency = 12, start = c(rok, mesic + 1))














