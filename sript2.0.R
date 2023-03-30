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




jmena_sloupecku <- c("promenna", "p-value", "AIC","AICc","BIC", "AR", "I", "MA", "lag")
pocet_sloupecku <- length(jmena_sloupecku)



tabulka_arima_modelu <- data.frame(matrix(ncol = pocet_sloupecku, nrow = 0))
colnames(tabulka_arima_modelu) <- jmena_sloupecku

my_matrix <- matrix(c(2019, 2, 2022, 2, 2023, 2), nrow = 3, ncol = 2, byrow = TRUE)
my_matrix_n <- nrow(my_matrix)

# Create an empty list to store the row vectors
row_vectors <- vector("list", length = my_matrix_n)


for (rocnik in 1:my_matrix_n) {
  
  row_vectors[[rocnik]] <- my_matrix[rocnik, ]
  end <- row_vectors[[rocnik]]
  print(end)


#end   <- c(2022, 2)


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
#par(mfrow = c(2, 1))
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


######################################################################################################
# Arima


# Convert the data to time series format
ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)


ar <- 1
d  <- 1
ma <- 1 #jednicka idealni

# Srovnani s arimou bez external regresoru

arima_model <- forecast::Arima(ts_real_inf, order = c(ar,d,ma))
ssm <- matrix(c("originalni model", NA, arima_model$aic, arima_model$aicc, arima_model$bic, ar, d, ma, "lag"), nrow = 1)
colnames(ssm) <- jmena_sloupecku

pocet_ss <- 0

originalni_modely <-matrix()



opposite_lag <- function(x, k) {
  c(tail(x, -k), rep(NA, k))
}

# Remove the last observation from the data as well
ts_real_inf_2 <- ts_real_inf[-length(ts_real_inf)]
ts_real_inf_2 <- ts(ts_real_inf_2, start = start, frequency = 12)

cifry <- 9


######################################################################################################
for (ar in 1:3) {
for (d in 0:1) {
for (ma in 1:3) {
for (i in 1:ncol(gt_dss)) {

  print(i)
  
  regresor <- ts(data = gt_dss[,i], start = c(2004, 1), end = end, frequency = 12)
  regresor <- regresor / mean(regresor) * mean(ts_real_inf)
  
  for (promenna in c("regresor","delay")) {
    
    rozeznani_do_tabulky <- promenna
    
    if (promenna == "regresor") {
      
      arima_model <- try(forecast::Arima(ts_real_inf, order = c(ar,d,ma), xreg = regresor))
      
      
    } else {
      
      ext_regressor <- regresor
      future_values <- opposite_lag(ext_regressor, 1)
      
      # Remove the last row, as it contains NA for future_values
      ext_regressors <- ts(future_values[-length(future_values)], start = start, frequency = 12) #ext_regressors[-nrow(ext_regressors), ]
      
      arima_model <- try(forecast::Arima(ts_real_inf_2, order = c(ar,d,ma), xreg = ext_regressors))
    }
  
  
  # Pro pripad multiple regression
  # Combine the original external regressor and its future values into a matrix
  #ext_regressors <- cbind(ext_regressor, future_values)
  #colnames(ext_regressors) <- c("ext_regressor", "future_values")
  
  

  #
  # Check if there was an error
  if (!inherits(arima_model, "try-error")) {
    # Store the ARIMA model in the list if no error occurred
  } else {
    # Print a message and continue to the next iteration if an error occurred
    cat("Error encountered for ARIMA(", i, ", 0, 0). Skipping this model.\n")
    next  # Continue to the next iteration
  }
  

  
  se_coef <- sqrt(diag(arima_model$var.coef))["xreg"]
  co <- arima_model$coef["xreg"]
  ss <- co/ se_coef
  
  if (is.nan(se_coef) == TRUE | is.nan(co) == TRUE) {
    p_value <- 1 
  } else {
    p_value <- round(2 * (1 - pnorm(abs(ss))), digits = 4) 
  }


  
  if (p_value < 0.1 ) {
    
    print("p_value")
    print(p_value)
    
    
    pocet_ss <- pocet_ss + 1
    print(i)
    b <- colnames(gt_dss)[i]
    cat(red(b))
    

    
    moje_aic <- round(arima_model$aic, digits = cifry)
    moje_aicc <- round(arima_model$aicc, digits = cifry)
    moje_bic <- round(arima_model$bic, digits = cifry)
    
    informace <- c(as.character(colnames(gt_dss)[i]), p_value, moje_aic, moje_aicc, moje_bic, ar, d, ma, rozeznani_do_tabulky)
    ssm <- rbind(ssm, informace)
    
    
    #grafika
    
    # Generate the fitted values
    fitted_values <- arima_model$fitted
    
    if (rozeznani_do_tabulky == "regresor"){
      barvicka <- "blue"
    } else {
      barvicka <- "red"
    }
    
    
    # Plot the actual and fitted values
    plot(ts_real_inf, main = paste("ARIMA(",ar,",",d,",",ma,") Fitted Values for Inflation"),
         xlab = "Time", ylab = b)
    lines(fitted_values, col = barvicka)
    legend("topleft",
           legend = c("Actual", "Fitted", 
                      paste("p-value =", as.character(p_value)),
                      paste("b =", as.character(b)),
                      paste("end =", as.character(end)),
                      paste("AIC =", as.character(moje_aic)),
                      paste("AICc =", as.character(moje_aicc)),
                      paste("BIC =", as.character(moje_bic))),
           lty = c(1, 1),
           col = c("black", barvicka))
    
    
  } else {
    print(colnames(gt_dss)[i])
  }
  }
}
}
}
}
print(paste("Pocet statisticky signifikantnich promennych je", pocet_ss))
print(ssm)
tabulka_arima_modelu <- rbind(tabulka_arima_modelu, ssm)
}

tabulka_arima_modelu
nrow(tabulka_arima_modelu)

old_row_names <- rownames(tabulka_arima_modelu)

# Create a vector of new row names with the changes you want
new_row_names <- old_row_names
new_row_names[old_row_names == "X"] <- "2019,12"
new_row_names[old_row_names == "X1"] <- "2022,2"
new_row_names[old_row_names == "X2"] <- "2023,2"

# Set the new row names in the data frame
rownames(tabulka_arima_modelu) <- new_row_names

# Switching second row index with first one
row.names(tabulka_arima_modelu) <- c(row.names(tabulka_arima_modelu)[2], row.names(tabulka_arima_modelu)[1], row.names(tabulka_arima_modelu)[-c(1,2)])

# Removing first observation
tabulka_arima_modelu <- tabulka_arima_modelu[-1, ]
















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










data1 <- data.frame(A = 1:3, B = 4:6)
data2 <- data.frame(A = 7:9, B = 10:12)

# Use rbind() to combine the data frames by rows
combined_data <- rbind(data1, data2)

# Print the combined data frame
print(combined_data)











# Nahodny nesmysly
gt_inf_s <- s_gt_inf
gt_inf_s
gt_inf_s <- gt_inf_s / mean(gt_inf_s) * mean(inf_cpm_s)
gt_inf_s 
gt_inf_s <- ts(gt_inf_s, frequency = 12, start = c(rok, mesic + 1))






  if (i == 1){
    
    arima_model <- forecast::Arima(ts_real_inf, order = c(ar,d,ma))
    
    plot(ts_real_inf, main = paste("ARIMA(",ar,",",d,",",ma,") Fitted Values for Inflation"),
         xlab = "Time", ylab = colnames(gt_dss)[i])
    fitted_values <- arima_model$fitted
    lines(fitted_values, col = "red")
    legend("topleft", legend = c("Actual", "Fitted", as.character(end)), lty = c(1,1), col = c("black", "red", "blue", "blue", "purple", "purple"))
    
  }









