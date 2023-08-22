library(readxl)
library(data.table)
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
library(tibble)
library(xtable)
library(ggplot2)
library(tidyr)
library(tidyverse)

new_data <- 0

if (new_data == 0){
  rm(list = ls())
  new_data <- 0
}

posun <- 1

DEBUG <- FALSE

debug_print <- function(arg) {
  if(DEBUG == TRUE){
    print(arg) 
  }
}


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
  debug_print(i)
  debug_print(data[[i]] %>% head)
  debug_print(data[[i]] %>% dim)
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
gt <- gt[ , !(names(gt) %in% c("cena.bydleni", "nakup.zlata"))]


gt_up <- read.csv("search_trends_up.csv", row.names = 1)
gt <- cbind(gt, gt_up[,14:ncol(gt_up)])


rownm <- as.Date(rownames(gt), format = "%Y-%d-%m")
rownm <- format(rownm, "%Y-%m-%d")
start <- rownames(gt)[1]
rok   <- as.numeric(substr(start, 1, 4))
mesic <- as.numeric(substr(start, 6, 7))
start <- c(rok, mesic)





jmena_sloupecku <- c("promenna", "p-value", "AIC","AICc","BIC", "AR", "I", "MA", "coef","lag", "sAIC", "sAICc","sBIC", 
                     "mae", "mse", "rmse", "b_mae", "b_mse", "b_rmse")
pocet_sloupecku <- length(jmena_sloupecku)



tabulka_arima_modelu <- data.frame(matrix(ncol = pocet_sloupecku, nrow = 0))
colnames(tabulka_arima_modelu) <- jmena_sloupecku

my_matrix <- matrix(c(2020, 2, 2022, 2, 2023, 2), nrow = 3, ncol = 2, byrow = TRUE)
my_matrix_n <- nrow(my_matrix)

# Create an empty list to store the row vectors
row_vectors <- vector("list", length = my_matrix_n)

originalni_modely <- matrix(c(1:6), nrow = 1)
colnames(originalni_modely) <- c("dAR","dI","dMA","dAIC","dAICc","dBIC") 

testy <- data.frame(time_interval = character(),
                    column_name = character(),
                    adf_p_value = numeric(),
                    ljung_box_p_value = numeric(),
                    stringsAsFactors = FALSE)

granger <- data.frame(time_interval = character(),
                      column_name = character(), 
                      n_lags = numeric(), 
                      granger_p_value = numeric())


bum <- 0

if (new_data == 0){
  for (rocnik in 1:my_matrix_n) {
    
    row_vectors[[rocnik]] <- my_matrix[rocnik, ]
    end <- row_vectors[[rocnik]]
    debug_print(end)
    
    
    #end   <- c(2022, 2)
    
    
    if(rocnik == 1){
      gt_f <- gt[1:(nrow(gt)-36),]
    }else if(rocnik == 2){
      gt_f <- gt[1:(nrow(gt)-12),]
    }else if(rocnik == 3){
      gt_f <- gt
    }
    
    debug_print("tvoje mama")
    
    # Convert all columns to numeric
    gt_numeric <- data.frame(lapply(gt_f, as.numeric))
    
    # Remove seasonality from the data
    gt_deseasonalized <- data.frame(lapply(gt_numeric, function(x) {
      stl_model <- stl(ts(x, frequency = 12), s.window = "periodic")
      deseasonalized <- stl_model$time.series[, "remainder"]
      return(deseasonalized)
    }))
    
    debug_print("moje mama")
    
    # Print the deseasonalized data
    
    debug_print("markova mama")
    
    
    
    if(rocnik == 1){
      row.names(gt_deseasonalized) <- rownm[1:(length(rownm)-36)]
    }else if(rocnik == 2){
      row.names(gt_deseasonalized) <- rownm[1:(length(rownm)-12)]
    }else if(rocnik == 3){
      row.names(gt_deseasonalized) <- rownm
    }
    
    
    debug_print(gt_deseasonalized)
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
    # Subsetting, tests and adjustment of GT
    
    s_inf_cpm_s <- window(ts_deseasonalized, frequency = 12, start = c(rok, mesic), end = end)
    
 
    
    for (i in 1:ncol(gt_dss)) {
      
      # ADF Test
      adf_test <- adf.test(gt_dss[,i])
      
      # Ljung-Box Test
      lb_test <- Box.test(gt_dss[,i], lag = 12, type = "Ljung-Box")
      
      # Append results to testy dataframe
      testy <- rbind(testy, data.frame(time_interval = rocnik,
                                       column_name = as.character(colnames(gt_dss)[i]), 
                                       adf_p_value = adf_test$p.value, 
                                       ljung_box_p_value = lb_test$p.value))
    }
    ######################################################################################################
    # PCA
    
    pca <- prcomp(gt_dss, scale. = TRUE)
    
    print(pca)
    
    if (rocnik == 2){
      
      pca_2022 <- pca
      
    }
    # Print PCA results (eigenvalues, eigenvectors, and standard deviations)
    debug_print(pca)
    
    index <- 0
    for (i in 1:length(pca$sdev)) {
      debug_print(i)
      if (pca$sdev[i] > 1){
        index <- index + 1
      }
    }
    
    gt_dss <- cbind(gt_dss, pca$x[,c(1:index)])
    
    
    # granger
    
    for (i in 1:ncol(gt_dss)) {
      x <- ts(gt_dss[, i], start = start, frequency = 12)
      for (j in 1:12) {

        
        y <- lmtest::grangertest(s_inf_cpm_s, x, order = j)
        debug_print(y)
        debug_print(colnames(gt_dss)[i])

        

        granger <- rbind(granger, data.frame(time_interval = rocnik,
                                             column_name = as.character(colnames(gt_dss)[i]), 
                                             n_lags = j, 
                                             granger_p_value = y$`Pr(>F)`))

      }
    }

    
    ######################################################################################################
    # Arima
    
    
    # Convert the data to time series format
    ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
    
    
    ar <- 1
    d  <- 1
    ma <- 1 #jednicka idealni
    
    # Srovnani s arimou bez external regresoru
    
    arima_model <- forecast::Arima(ts_real_inf, order = c(ar,d,ma))
    ssm <- matrix(c("originalni model", NA, arima_model$aic, arima_model$aicc, arima_model$bic, ar, d, ma, 0, "lag", "Vojta", "je", "debil",
                    "mae", "mse", "rmse", "b_mae", "b_mse", "b_rmse"), nrow = 1, ncol = length(jmena_sloupecku))
    colnames(ssm) <- jmena_sloupecku
    
    pocet_ss <- 0
    
    
    
    opposite_lag <- function(x, k) {
      c(tail(x, -k), rep(NA, k))
    }
    
    # Remove the last observation from the data as well
    ts_real_inf_2 <- ts_real_inf[-length(ts_real_inf)]
    ts_real_inf_2 <- ts(ts_real_inf_2, start = start, frequency = 12)
    
    ts_real_inf_3 <- ts_real_inf[-1]
    ts_real_inf_3 <- ts(ts_real_inf_3, start = c(start[1], (start[2] + 1)), frequency = 12)
    
    
    
    # Plot ACF
    acf(ts_real_inf, main = paste("ACF - Inflation, (2004 -", end[1],")"))
    
    # Plot PACF
    pacf(ts_real_inf, main = paste("PACF - Inflation, (2004 -", end[1],")"))
    
    cifry <- 9
    
    ######################################################################################################
    for (i in 1:ncol(gt_dss)) {
      print(colnames(gt_dss)[i])
      for (d in 0:1) {
        for (ma in 1:3) {
          for (ar in 1:3) {
          
            
            debug_print(i)
            
            regresor <- ts(data = gt_dss[,i], start = c(2004, 1), end = end, frequency = 12)
            regresor <- regresor / regresor[1] * ts_real_inf[1]
            #print(regresor)
            
            for (promenna in c("regresor","delay", "posun_vpred")) {
              
              rozeznani_do_tabulky <- promenna
              
              if (promenna == "regresor") {
                
                arima_model <- try(forecast::Arima(ts_real_inf, order = c(ar,d,ma), xreg = regresor))
                
                
              } else if(promenna == "delay"){
                
                ext_regressor <- regresor
                if(bum == 1){
                #future_values <- opposite_lag(ext_regressor, 1)
                posun <- 1
                future_values <- stats::lag(ext_regressor, posun)
                ext_regressors <- ts(future_values[-(1:posun)], start = c(start[1], (start[2])), frequency = 12)
                #ext_regressors <- ts(regresor, start = c(start[1], (start[2]+1)))
                arima_model <- try(forecast::Arima(ts_real_inf_3, order = c(ar,d,ma), xreg = ext_regressors))
                } else {
                  ext_regressor <- as.vector(ext_regressor)
                  ext_regressor <- lag(ext_regressor, posun)
                  ext_regressor <- ts(ext_regressor, start = c(start[1], (start[2])), frequency = 12)
                  

                  arima_model <- try(forecast::Arima(ts_real_inf, order = c(ar,d,ma), xreg = ext_regressor))
                }
                
                
                
                #print(ext_regressors)
                #print(ts_real_inf_3)
                #print("Konec")
                
              } else if(promenna == "posun_vpred") {
                
                ext_regressor <- regresor
                if(bum == 1){
                  ext_regressor <- regresor
                  posun <- 1
                  future_values <- opposite_lag(ext_regressor, posun)
                  
                  
                  # Remove the last row, as it contains NA for future_values
                  ext_regressors <- ts(future_values[-length(future_values)], start = start, frequency = 12) #ext_regressors[-nrow(ext_regressors), ]
                  
                  arima_model <- try(forecast::Arima(ts_real_inf_2, order = c(ar,d,ma), xreg = ext_regressors))
                  
                } else {
                  ts_real_inf_4 <- ts_real_inf
                  ext_regressor <- opposite_lag(ext_regressor, posun)
                  ext_regressor <- ts(ext_regressor, start = c(start[1], (start[2])), frequency = 12)
                  
                  arima_model <- try(forecast::Arima(ts_real_inf_4, order = c(ar,d,ma), xreg = ext_regressor))
                }
                

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
                cat("Error encountered for ARIMA(", colnames(gt_dss)[i]) # . Skipping this model.\n")
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
                
                debug_print("p_value")
                debug_print(p_value)
                
                
                pocet_ss <- pocet_ss + 1
                debug_print(i)
                b <- colnames(gt_dss)[i]
                cat(red(b))
                
                
                
                moje_aic <- round(arima_model$aic, digits = cifry)
                moje_aicc <- round(arima_model$aicc, digits = cifry)
                moje_bic <- round(arima_model$bic, digits = cifry)
                
                
                # mae, mse, rmse
                
                fitted_values <- fitted(arima_model)   
                
                
                if(promenna == "posun_vpred"){
                  
                  residuals <- ts_real_inf_4 - fitted_values
                  
                } else {
                  
                  residuals <- ts_real_inf - fitted_values
                  
                }
                
                residuals <- na.omit(residuals)
                
                # Mean Absolute Error (MAE)
                mae <- mean(abs(residuals))
                mae <- round(mae, digits = cifry)
                
                # Mean Squared Error (MSE)
                mse <- mean(residuals^2)
                mse <- round(mse, digits = cifry)
                
                # Root Mean Squared Error (RMSE)
                rmse <- sqrt(mse)
                rmse <- round(rmse, digits = cifry)
                
                
                if (rozeznani_do_tabulky == "regresor") {
                  
                  srovnavaci_model <- try(forecast::Arima(ts_real_inf, order = c(ar,d,ma)))
                  srovnavaci_vektor <- c(as.character(ar), as.character(d), as.character(ma), srovnavaci_model$aic, srovnavaci_model$aicc, srovnavaci_model$bic)
                  originalni_modely <- rbind(originalni_modely,srovnavaci_vektor)
                  
                } else if (rozeznani_do_tabulky == "delay") {
                  
                  srovnavaci_model <- try(forecast::Arima(ts_real_inf, order = c(ar,d,ma)))
                  srovnavaci_vektor <- c(as.character(ar), as.character(d), as.character(ma), srovnavaci_model$aic, srovnavaci_model$aicc, srovnavaci_model$bic)
                  originalni_modely <- rbind(originalni_modely, srovnavaci_vektor)
                  
                  
                } else if (rozeznani_do_tabulky == "posun_vpred") {
                  
                  srovnavaci_model <- try(forecast::Arima(ts_real_inf_4, order = c(ar,d,ma)))
                  srovnavaci_vektor <- c(as.character(ar), as.character(d), as.character(ma), srovnavaci_model$aic, srovnavaci_model$aicc, srovnavaci_model$bic)
                  originalni_modely <- rbind(originalni_modely, srovnavaci_vektor)
                  
                }
                
                
                # benchmark mae, mse, rmse
                
                fitted_values <- fitted(srovnavaci_model)
                
                if(promenna == "posun_vpred"){
                  
                  residuals <- ts_real_inf_4 - fitted_values
                  
                } else {
                  
                  residuals <- ts_real_inf - fitted_values
                  
                }
                
                # Mean Absolute Error (MAE)
                b_mae <- mean(abs(residuals))
                b_mae <- round(b_mae, digits = cifry)
                
                # Mean Squared Error (MSE)
                b_mse <- mean(residuals^2)
                b_mse <- round(b_mse, digits = cifry)
                
                # Root Mean Squared Error (RMSE)
                b_rmse <- sqrt(b_mse)
                b_rmse <- round(b_rmse, digits = cifry)
                
                informace <- c(as.character(colnames(gt_dss)[i]), p_value, moje_aic, moje_aicc, moje_bic, ar, d, ma, co,rozeznani_do_tabulky, 
                               srovnavaci_model$aic, srovnavaci_model$aicc, srovnavaci_model$bic, mae, mse, rmse, b_mae, b_mse, b_rmse)
                
                
                ssm <- rbind(ssm, informace)
                
                rownames(ssm)[nrow(ssm)] <- paste(as.character(end[1]),"/",as.character(end[2]),"/",as.character(i))
                
                
                
                debil <- c(ar, d, ma)
                
                #grafika
                
                # Generate the fitted values
                fitted_values <- arima_model$fitted
                
                
                if (DEBUG == TRUE) {
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
                
                }
              } else {
                debug_print(colnames(gt_dss)[i])
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
  
} else{
  #write.csv(tabulka_arima_modelu, "tabulka_arima_modelu.csv", row.names = FALSE)
  # Loading the dataframe with rownames
  tabulka_arima_modelu <- read.csv("tabulka_arima_modelu.csv", row.names = 1)
  
  write.csv(gt_dss_a, "gt_dss.csv", row.names = TRUE)
  gt_dss <- read.csv("gt_dss.csv", row.names = 1)
  
  ts_real_inf <- ts(data = s_inf_cpm_s, start = c(2004, 1), frequency = 12)
  ts_real_inf_3 <- ts_real_inf[-1]
  ts_real_inf_3 <- ts(ts_real_inf_3, start = c(start[1], (start[2] + 1)), frequency = 12)
  
}


granger <- na.omit(granger)
granger_sub <- granger %>% filter(granger_p_value < 0.05)
granger_sub %>% count(column_name)




tabulka_arima_modelu
nrow(tabulka_arima_modelu)




tabulka_arima_modelu_zaloha <- tabulka_arima_modelu

tabulka_arima_modelu <- tabulka_arima_modelu_zaloha

print(tabulka_arima_modelu)

for (i in 1:nrow(tabulka_arima_modelu)) {
  print(i)
  if (tabulka_arima_modelu[i, "lag"] == "lag") {
    tabulka_arima_modelu[i,] <- NA
  }
}

tabulka_arima_modelu <- tabulka_arima_modelu[complete.cases(tabulka_arima_modelu), ]

tabulka_arima_modelu <- tabulka_arima_modelu %>%
  mutate(across(c("AIC", "AICc","BIC", "sAIC", "sAICc","sBIC"), as.numeric))

tabulka_arima_modelu$model <-  tabulka_arima_modelu$AIC + tabulka_arima_modelu$AICc + tabulka_arima_modelu$BIC
tabulka_arima_modelu$benchmark <- tabulka_arima_modelu$sAIC + tabulka_arima_modelu$sAICc + tabulka_arima_modelu$sBIC
tabulka_arima_modelu$vyslednice <- tabulka_arima_modelu$model - tabulka_arima_modelu$benchmark

tabulka_arima_modelu$model_bez_bic <-  tabulka_arima_modelu$AIC + tabulka_arima_modelu$AICc
tabulka_arima_modelu$benchmark_bez_bic <- tabulka_arima_modelu$sAIC + tabulka_arima_modelu$sAICc
tabulka_arima_modelu$vyslednice_bez_bic <- tabulka_arima_modelu$model_bez_bic - tabulka_arima_modelu$benchmark_bez_bic


#View(tabulka_arima_modelu)

tabulka_arima_modelu <- tibble::rownames_to_column(tabulka_arima_modelu, var = "row_names")

tabulka_arima_modelu$row_names <- gsub("X", "", tabulka_arima_modelu$row_names)

# Replace "..." with "/" in the row_names column
tabulka_arima_modelu$row_names <- gsub("\\.\\.\\.", "/", tabulka_arima_modelu$row_names)

tabulka_arima_modelu$row_names <- gsub("(/[^/]*)/.*", "\\1", tabulka_arima_modelu$row_names)

# Print the updated dataframe
print(tabulka_arima_modelu)



min_rows <- tabulka_arima_modelu %>%
  group_by(row_names) %>%
  filter(model == min(model) | benchmark == min(benchmark))

# Print the resulting dataframe
print(min_rows)

min_rows_bez_bic <- tabulka_arima_modelu %>%
  group_by(row_names) %>%
  filter(model_bez_bic == min(model_bez_bic) | benchmark_bez_bic == min(benchmark_bez_bic))

# Print the resulting dataframe
print(min_rows_bez_bic)



min_rows_delay <- tabulka_arima_modelu %>% filter(lag == "delay")
min_rows_delay




result <- tabulka_arima_modelu %>%
  group_by(row_names, promenna) %>%
  filter(benchmark == min(benchmark) | model == min(model)) %>%
  ungroup()


View(tabulka_arima_modelu)
View(min_rows)
View(min_rows_delay)
View(result)

nrow(result)
result <- result %>%
  mutate_at(vars(-row_names, -promenna, -lag), as.numeric)


sum(result[,"vyslednice"]> 0)
sum((result$rmse-result$b_rmse)>0)
sum((result$mae-result$b_mae)>0)

######################################################################################################
# three key benchmark models motherfucker!

cifra <- 9

# 2004 - 2020


uga <- ts(ts_real_inf[1:(length(ts_real_inf) - 36)], start = start, frequency = 12)

benchmark_2020 <- forecast::Arima(uga, order = c(1,1,1))
forecast::auto.arima(uga)


residuals_2020 <- uga - fitted(benchmark_2020)

mae_2020 <- mean(abs(residuals_2020))
mae_2020 <- round(mae_2020, digits = cifra)

mse_2020 <- mean(residuals_2020^2)
mse_2020 <- round(mse_2020, digits = cifra)

rmse_2020 <- sqrt(mse_2020)
rmse_2020 <- round(rmse_2020, digits = cifra)

# 2004 - 2022



buga <- ts(ts_real_inf[1:(length(ts_real_inf) - 12)], start = start, frequency = 12)
          
benchmark_2022 <- forecast::Arima(buga, order = c(1,1,1))
forecast::auto.arima(buga)

residuals_2022 <- buga - fitted(benchmark_2022)

mae_2022 <- mean(abs(residuals_2022))
mae_2022 <- round(mae_2022, digits = cifra)

mse_2022 <- mean(residuals_2022^2)
mse_2022 <- round(mse_2022, digits = cifra)

rmse_2022 <- sqrt(mse_2022)
rmse_2022 <- round(rmse_2022, digits = cifra)

# 2004 - 2023

benchmark_2023 <- forecast::Arima(ts_real_inf, order = c(1,1,1))
forecast::auto.arima(ts_real_inf)


residuals_2023 <- ts_real_inf - fitted(benchmark_2023)

mae_2023 <- mean(abs(residuals_2023))
mae_2023 <- round(mae_2023, digits = cifra)

mse_2023 <- mean(residuals_2023^2)
mse_2023 <- round(mse_2023, digits = cifra)

rmse_2023 <- sqrt(mse_2023)
rmse_2023 <- round(rmse_2023, digits = cifra)


b_2020 <- c(benchmark_2020$aic, benchmark_2020$aicc, benchmark_2020$bic, mae_2020, mse_2020, rmse_2020)
b_2022 <- c(benchmark_2022$aic, benchmark_2022$aicc, benchmark_2022$bic, mae_2022, mse_2022, rmse_2022)
b_2023 <- c(benchmark_2023$aic, benchmark_2023$aicc, benchmark_2023$bic, mae_2023, mse_2023, rmse_2023)


three_key_benchmarks <- matrix(c(b_2020, b_2022, b_2023), nrow = 3, ncol = length(b_2020), byrow = TRUE)





# sejvni env




######################################################################################################
# Ten jediny funkcni model

end   <- c(2023, 2)

regresor <- ts(data = gt_dss[,"inflace"], start = c(2004, 1), end = end, frequency = 12)
regresor <- regresor / mean(regresor) * mean(ts_real_inf)

ext_regressor <- regresor
#future_values <- opposite_lag(ext_regressor, 1)
posun <- 1
future_values <- stats::lag(ext_regressor, posun)
ext_regressors <- ts(future_values[-(1:posun)], start = c(start[1], (start[2] + 1)), frequency = 12)
arima_model <- try(forecast::Arima(ts_real_inf_3, order = c(1,1,2), xreg = ext_regressors))
#arima_model <- try(forecast::Arima(ts_real_inf_3, order = c(1,1,2)))

arima_model$fitted


# Combine the data into a single data frame
data <- data.frame(ts_real_inf_3, ext_regressors)

uga <-  c("iterace", "ext", "rw", "mae", "mse", "rmse")

tabulka_nej_model <- matrix( nrow = 1, ncol = length(uga))
colnames(tabulka_nej_model) <- as.character(uga)


# sejvni env - bubu verze
#######################################################################################################################################################
for (bubu in 72:200) {
  print(bubu)

# Set up the window sizes for the rolling and expanding windows
rolling_window_size <- bubu
expanding_window_size <- nrow(data) - rolling_window_size

# Set up the initial model using the first window of data
#model <- Arima(data[1:rolling_window_size, "ts_real_inf_3"], order = c(1,1,1), xreg = data[1:rolling_window_size, "ext_regressors"])

# Set up empty vectors to store the forecasts and actuals
rolling_forecasts <- numeric()
rolling_actuals <- numeric()
expanding_forecasts <- numeric()
expanding_actuals <- numeric()

window_size <- rolling_window_size  # Length of the rolling window
n_ahead <- 1



library(forecast)

# Assuming ts_real_inf_3 and ext_regressors are time series objects with the same frequency and date range


start_date <- start(ts_real_inf_3)
end_date <- end(ts_real_inf_3)
n_iterations <- length(ts_real_inf_3) - window_size - n_ahead + 1


forecasts_rw <- list()

for (i in 1:n_iterations) {
  debug_print(i/n_iterations)
  start_window <- start_date + (i - 1) * c(0, 1)
  end_window <- start_window + c(0, window_size - 1)
  ts_window <- window(ts_real_inf_3, start = start_window, end = end_window)
  ext_regressors_window <- window(ext_regressors, start = start_window, end = end_window)
  
  tryCatch({
    model <- Arima(ts_window, order = c(1, 1, 2), xreg = ext_regressors_window)
    
    start_forecast <- end_window + c(0, 1)
    end_forecast <- start_forecast + c(0, n_ahead - 1)
    ext_regressors_forecast <- window(ext_regressors, start = start_forecast, end = end_forecast)
    
    forecast_result <- forecast::forecast(model, h = n_ahead, xreg = ext_regressors_forecast)
    
    forecasts_rw[[i]] <- forecast_result
  }, error = function(e) {
    cat("Error at iteration", i, ":", e$message, "\n")
    forecasts_rw[[i]] <- "TVOJE"
  })
}


point_estimates_rw <- do.call(c, lapply(forecasts_rw, function(x) x$mean))

# bodiky
{



# Print the point estimates
debug_print(point_estimates_rw)


# Create a new time series object with the forecasted values and their respective time indexes
prec <- c(window_size )
start_forecast_all <- start_date + c(0 , window_size)
end_forecast_all <- end_date
ts_forecast <- ts(point_estimates_rw, start = start_forecast_all, end = end_forecast_all, frequency = frequency(ts_real_inf_3))

# Plot the actual and forecasted values together
#ts.plot(ts_real_inf_3, ts_forecast, col = c("black", "red"), lty = c(1, 1), main = "Actual vs. Forecasted Values", xlab = "Time", ylab = "Value")
#legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = c(1, 1), bty = "n")


# Extract the actual values for which we have forecasts
actual_values <- window(ts_real_inf_3, start = start_forecast_all, end = end_forecast_all)

# Calculate quality measures
mae <- mean(abs(actual_values - point_estimates_rw))
mse <- mean((actual_values - point_estimates_rw)^2)
rmse <- sqrt(mse)

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

}


tabulka_nej_model <- rbind(tabulka_nej_model, c(bubu,"ext", "rw", mae, mse, rmse))

start_date <- start(ts_real_inf_3)
end_date <- end(ts_real_inf_3)
n_iterations <- length(ts_real_inf_3) - window_size - n_ahead + 1


forecasts_rw_bez <- list()

for (i in 1:n_iterations) {
  debug_print(i/n_iterations)
  start_window <- start_date + (i - 1) * c(0, 1)
  end_window <- start_window + c(0, window_size - 1)
  ts_window <- window(ts_real_inf_3, start = start_window, end = end_window)
  ext_regressors_window <- window(ext_regressors, start = start_window, end = end_window)
  
  tryCatch({
    model <- Arima(ts_window, order = c(1, 1, 2))
    
    start_forecast <- end_window + c(0, 1)
    end_forecast <- start_forecast + c(0, n_ahead - 1)
    ext_regressors_forecast <- window(ext_regressors, start = start_forecast, end = end_forecast)
    
    forecast_result <- forecast::forecast(model, h = n_ahead, xreg = ext_regressors_forecast)
    
    forecasts_rw_bez[[i]] <- forecast_result
  }, error = function(e) {
    cat("Error at iteration", i, ":", e$message, "\n")
    forecasts_rw_bez[[i]] <- "TVOJE"
  })
}


point_estimates_rw_bez <- do.call(c, lapply(forecasts_rw_bez, function(x) x$mean))

# bodiky
{
  
  
  
  # Print the point estimates
  debug_print(point_estimates_rw_bez)
  
  
  # Create a new time series object with the forecasted values and their respective time indexes
  prec <- c(window_size )
  start_forecast_all <- start_date + c(0 , window_size)
  end_forecast_all <- end_date
  ts_forecast <- ts(point_estimates_rw_bez, start = start_forecast_all, end = end_forecast_all, frequency = frequency(ts_real_inf_3))
  
  # Plot the actual and forecasted values together
  #ts.plot(ts_real_inf_3, ts_forecast, col = c("black", "red"), lty = c(1, 1), main = "Actual vs. Forecasted Values", xlab = "Time", ylab = "Value")
  #legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = c(1, 1), bty = "n")
  
  
  # Extract the actual values for which we have forecasts
  actual_values <- window(ts_real_inf_3, start = start_forecast_all, end = end_forecast_all)
  
  # Calculate quality measures
  mae <- mean(abs(actual_values - point_estimates_rw_bez))
  mse <- mean((actual_values - point_estimates_rw_bez)^2)
  rmse <- sqrt(mse)
  
  cat("Mean Absolute Error (MAE):", mae, "\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
}

tabulka_nej_model <- rbind(tabulka_nej_model, c(bubu,"bez", "rw", mae, mse, rmse))


# Expanding window forecast
n <- length(ts_real_inf_3)
start_forecast <- rolling_window_size
forecasts <- ts(numeric(n - start_forecast + 1), start = start(ts_real_inf_3)[1] + (start_forecast - 1) / frequency(ts_real_inf_3), frequency = frequency(ts_real_inf_3))

for (t in start_forecast:n) {
  # Fit the ARIMA model with the external regressor on the expanding window
  model <- Arima(ts_real_inf_3[1:(t - 1)], order = c(1, 1, 2), xreg = ext_regressors[1:(t - 1)])
  
  # One-step ahead forecast
  forecast <- predict(model, n.ahead = 1, newxreg = ext_regressors[t])
  
  # Save the forecast
  forecasts[t - start_forecast + 1] <- forecast$pred
}

# bodiky
{
# Print the forecasts
debug_print(forecasts)

lines(forecasts, col = "blue")


# Calculate the errors
errors <- forecasts - ts_real_inf_3[start_forecast:n]

# Calculate MAE
mae <- mean(abs(errors))

# Calculate MSE
mse <- mean(errors^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Print the results
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


}



tabulka_nej_model <- rbind(tabulka_nej_model, c(bubu,"ext", "ew", mae, mse, rmse))




for (t in start_forecast:n) {
  # Fit the ARIMA model with the external regressor on the expanding window
  model <- Arima(ts_real_inf_3[1:(t - 1)], order = c(1, 1, 2))
  
  # One-step ahead forecast
  forecast <- predict(model, n.ahead = 1)
  
  # Save the forecast
  forecasts[t - start_forecast + 1] <- forecast$pred
}

# bodiky
{
  # Print the forecasts
  debug_print(forecasts)
  
  lines(forecasts, col = "blue")
  
  
  # Calculate the errors
  errors <- forecasts - ts_real_inf_3[start_forecast:n]
  
  # Calculate MAE
  mae <- mean(abs(errors))
  
  # Calculate MSE
  mse <- mean(errors^2)
  
  # Calculate RMSE
  rmse <- sqrt(mse)
  
  # Print the results
  cat("Mean Absolute Error (MAE):", mae, "\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
  
}
#################################################### Rolling and expanding window forecasts bez regresoru


tabulka_nej_model <- rbind(tabulka_nej_model, c(bubu, "bez", "ew", mae, mse, rmse))



}
#sejvuju env
tabulka_nej_model <- as.data.frame(tabulka_nej_model)
tabulka_nej_model_z <- tabulka_nej_model
#tabulka_nej_model <- tabulka_nej_model_z



tabulka_nej_model <- tabulka_nej_model[-1,]

colnames(tabulka_nej_model) <- as.character(uga)
tabulka_nej_model

tabulka_nej_model$iterace <- as.numeric(tabulka_nej_model$iterace)
tabulka_nej_model$mae <- as.numeric(tabulka_nej_model$mae)
tabulka_nej_model$mse <- as.numeric(tabulka_nej_model$mse)
tabulka_nej_model$rmse <- as.numeric(tabulka_nej_model$rmse)


tabulka_nej_model


meritka <- c("mae","mse", "rmse")
par(mfrow = c(2, 2))
for (model in unique(tabulka_nej_model$ext)) {
  for (predpoved in unique(tabulka_nej_model$rw)) {

      data_graf <- tabulka_nej_model %>% filter(ext == model)
      data_graf <- data_graf %>% filter(rw == predpoved)

      
      if (model == "ext") {
        nazev_grafu_ext <- "External regressor"
      } else {
        nazev_grafu_ext <- "Benchmark model"
      }
      
      if (predpoved == "rw") {
        nazev_grafu_rw <- "Rolling window forecast"
      } else {
        nazev_grafu_rw <- "Expanding window forecast"
      }
      
      
      matplot(data_graf$iterace, data_graf[, meritka], type = "l", ylim = c(0.3,0.9), 
              xlab = "(Starting) Window length", ylab = "Value", main = c(paste(nazev_grafu_rw, " - ", nazev_grafu_ext)))
      

      legend("topleft", legend = colnames(data_graf[, meritka]), col = 1:3, lty = 1, cex = 0.8)
  }
}



line_types <- c(1, 1, 1, 2, 2, 2)
colores <- c("#3399FF", "#33CC66", "#FF0000", "#4DA6FF", "#66CC99", "#FF6666")#3399FF #33CC66
par(mfrow = c(1, 1))
for (predpoved in unique(tabulka_nej_model$rw)) {
  
  if (predpoved == "rw") {
    nazev_grafu_rw <- "Rolling window forecast"
  } else {
    nazev_grafu_rw <- "Expanding window forecast"
  }
  
  
  data_graf <- tabulka_nej_model %>% filter(rw == predpoved)
  jmena_sloupcu <- colnames(data_graf)
  print(predpoved)
  data_graf <- cbind(data_graf %>% filter(ext == "ext"), data_graf %>% filter(ext == "bez"))
  
  print(data_graf)
  colnames(data_graf)[7:12] <- c("v1", "v2","v3", "mae - benchmark", "mse - benchmark", "rmse - benchmark")
  
  
  meritka <- c("mae","mse", "rmse", "mae - benchmark", "mse - benchmark", "rmse - benchmark")
  
  if(predpoved == "rw"){
    osa_x <- "Window length"
  } else {
    osa_x <- "Starting window length"
  }
  
  matplot(data_graf$iterace, data_graf[, meritka], type = "l", lty = line_types, ylim = c(0.3,0.9), 
          xlab = osa_x, ylab = "Value", col = colores, main = nazev_grafu_rw)
  
  # Add a legend
  legend("top", legend = colnames(data_graf[, meritka]), col = colores, lty = line_types, cex = 1)
}




# testy


for (predpoved in unique(tabulka_nej_model$rw)) {
  
  if (predpoved == "rw") {
    nazev_grafu_rw <- "Rolling window forecast"
  } else {
    nazev_grafu_rw <- "Expanding window forecast"
  }
  

  data_graf <- tabulka_nej_model %>% filter(rw == predpoved)
  jmena_sloupcu <- colnames(data_graf)

  data_graf <- cbind(data_graf %>% filter(ext == "ext"), data_graf %>% filter(ext == "bez"))
  testy <- data_graf[,c(1,4,5,6,10,11,12)]
  
  soucet_m <- testy$mae + testy$mse + testy$rmse
  
  soucet_b <- testy$mae.1 + testy$mse.1 + testy$rmse.1
  
  my_vector <- soucet_m
  
  smallest_values <- sort(my_vector)[1:3]
  
  # Get the indices of the smallest values
  indices <- which(my_vector %in% smallest_values)
  
  print(predpoved)
  print("3 nejlepsi settingy okna")
  print(testy[indices,"iterace"])

  print("jak casto nizsi?")
  print(sum(soucet_m < soucet_b))
  
  print("parove modely")
  
  alternativa <- c("less")
  
  t_mae <- t.test(testy$mae ,testy$mae.1 , alternative = alternativa, paired = TRUE)
  print(t_mae)
  
  t_mse <- t.test(testy$mse ,testy$mse.1 , alternative = alternativa, paired = TRUE)
  print(t_mse)
  
  t_rmse <- t.test(testy$rmse ,testy$rmse.1 , alternative = alternativa, paired = TRUE)
  print(t_rmse)
  
  
  print("NEparove modely")
  t_mae <- t.test(testy$mae ,testy$mae.1 , alternative = alternativa)
  print(t_mae)
  
  t_mse <- t.test(testy$mse ,testy$mse.1 , alternative = alternativa)
  print(t_mse)
  
  t_rmse <- t.test(testy$rmse ,testy$rmse.1 , alternative = alternativa)
  print(t_rmse)
}





tabulka_nej_model_testy <- cbind(tabulka_nej_model %>% filter(rw == "rw"),tabulka_nej_model %>% filter(rw == "ew"))
tabulka_nej_model_testy <- tabulka_nej_model_testy[, c(4,5,6,10,11,12)]
t.test(tabulka_nej_model_testy$mae ,tabulka_nej_model_testy$mae.1 , alternative = alternativa)

#tisknuti tabulek do texu
{
print(xtable(min_rows, caption = "Best models",
             digits = 2, type = "latex"), file = "min_rows.tex")


View(tabulka_arima_modelu)
View(gt_dss)
View(min_rows)


tabulka_arima_modelu %>% group_by(row_names) %>% count(promenna)


View(print(tabulka_arima_modelu %>% group_by(promenna) %>%filter(row_names == "2022/2")))

print(pca_2022)

pc_1 <- pca_2022[[2]]

pc_1 <- pc_1[,"PC1"]

pc_1 <- as.data.frame(pc_1 %>% round(2))

pc_1

print(xtable(pc_1, caption = "Degree of correlation between First principal component and other search queries",
             digits = 2, type = "latex"), file = "tabulka_pc1.tex")



pc_1_modely <- tabulka_arima_modelu %>% filter(promenna == "PC1")

sorted_df <- pc_1_modely[order(pc_1_modely$model), ]

pc_1_modely <- sorted_df[1:3,]

pc_1_modely$coef <- as.numeric(pc_1_modely$coef)

pc_1_modely_rounded <- data.frame(sapply(pc_1_modely, function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}))

pc_1_modely

pc_1_modely <- pc_1_modely[,c(7,8,9,10,3,11,4,5,6,12,13,14)]
print(xtable(pc_1_modely, caption = "Modely",
             digits = 2, type = "latex"), file = "tabulka_pc1_modely.tex")







pc_1_modely <- tabulka_arima_modelu %>% filter(promenna == "cena.nafty")

sorted_df <- pc_1_modely[order(pc_1_modely$benchmark), ]

pc_1_modely <- sorted_df[1:3,]

pc_1_modely$coef <- as.numeric(pc_1_modely$coef)

pc_1_modely_rounded <- data.frame(sapply(pc_1_modely, function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}))

pc_1_modely



pc_1_modely <- tabulka_arima_modelu %>% filter(row_names == "2022/2") %>% filter(promenna == "cena.nemovitosti")

sorted_df <- pc_1_modely[order(pc_1_modely$benchmark), ]

pc_1_modely <- sorted_df[1:3,]

pc_1_modely$coef <- as.numeric(pc_1_modely$coef)

pc_1_modely_rounded <- data.frame(sapply(pc_1_modely, function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}))

pc_1_modely





pc_1_modely <- tabulka_arima_modelu %>% filter(row_names == "2022/2") %>% filter(promenna == "cena.ropy")

sorted_df <- pc_1_modely[order(pc_1_modely$benchmark), ]

pc_1_modely <- sorted_df[1:3,]

pc_1_modely$coef <- as.numeric(pc_1_modely$coef)

pc_1_modely_rounded <- data.frame(sapply(pc_1_modely, function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}))

pc_1_modely





pc_1_modely <- tabulka_arima_modelu %>% filter(row_names == "2022/2") %>% filter(promenna == "inflace")

sorted_df <- pc_1_modely[order(pc_1_modely$benchmark), ]

pc_1_modely <- sorted_df[1:3,]

pc_1_modely$coef <- as.numeric(pc_1_modely$coef)

pc_1_modely_rounded <- data.frame(sapply(pc_1_modely, function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}))

pc_1_modely



pc_1_modely <- tabulka_arima_modelu %>% filter(row_names == "2023/2") %>% filter(promenna == "inflace")

sorted_df <- pc_1_modely[order(pc_1_modely$benchmark), ]

pc_1_modely <- sorted_df[1:3,]

pc_1_modely$coef <- as.numeric(pc_1_modely$coef)

pc_1_modely_rounded <- data.frame(sapply(pc_1_modely, function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}))

pc_1_modely



5 <8

}




######################################################################################################
# VAR model


for (i in 1:ncol(gt_dss)) {
  debug_print(i)
  b <- colnames(gt_dss)[i]
  print(b)
  
  var_regresor <- ts(gt_dss[,i], start = start, frequency = 12)
  var_regresor <- window(var_regresor, start = start, end = end, frequency = 12)
  for (j in 1:2) {
    data_matrix <- cbind(ts_real_inf, var_regresor)
    complete_matrix <- data_matrix[complete.cases(data_matrix), ]
    if(nrow(data_matrix) != nrow(complete_matrix)){
      print("Tohle bude problemek")
    }
    colnames(complete_matrix)[2] <- b
    var_model <- vars::VAR(complete_matrix, p = j, type = "const")
    summary(var_model) %>% print
  }
}


summary(var_model)







library(forecast)
library(vars)
library(lmtest)  # Load the lmtest package for coeftest

# Assuming ts_real_inf and ext_regressors are time series objects with the same frequency and date range

start_date <- start(ts_real_inf)
end_date <- end(ts_real_inf)
n_iterations <- length(ts_real_inf) - window_size - n_ahead + 1
max_lag <- 12  # Set the maximum lag order for model selection

pocet_parametru <- 5
# Create an empty matrix to store the parameters of the best model for each column in gt_dss
parameter_matrix <- matrix(NA, ncol(gt_dss), pocet_parametru)  # 5 columns for Variable, AIC, AICc, BIC, and lag_order
# Create an empty list to store parameters of every model for each column in gt_dss
all_var_models <- matrix(c("originalni model", "AIC", "AICc", "BIC", "Lags"), nrow = 1, ncol = pocet_parametru)

# Column names of gt_dss
gt_dss_names <- colnames(gt_dss)

for (i in 1:ncol(gt_dss)) {
  debug_print(i)
  b <- colnames(gt_dss)[i]
  print(b)
  
  var_regressor <- ts(gt_dss[, i], start = start, frequency = 12)
  var_regressor <- window(var_regressor, start = start, end = end, frequency = 12)
  
  selected_lag <- NULL
  
  for (j in 1:max_lag) {
    data_matrix <- cbind(ts_real_inf, var_regressor)
    complete_matrix <- data_matrix[complete.cases(data_matrix), ]
    if (nrow(data_matrix) != nrow(complete_matrix)) {
      print("Tohle bude problemek")
    }
    colnames(complete_matrix)[2] <- b
    
    # Fit the VAR model with the current lag order
    var_model <- VAR(complete_matrix, p = j, type = "both")
    
    # Get the AIC value for the current model
    aic_value <- AIC(var_model)
    
    # Get the number of parameters (k) and observations (n)
    k <- length(coef(var_model))
    n <- nrow(complete_matrix)
    
    # Calculate AICc and BIC
    aicc_value <- aic_value + (2 * k * (k + 1)) / (n - k - 1)
    bic_value <- aic_value + k * log(n)
    
    vektorek <- c(gt_dss_names[i], aic_value, aicc_value, bic_value, j)
    all_var_models <- rbind(all_var_models, vektorek)
    
    # If this is the first iteration or the current AICc value is smaller than the previous one,
    # update the selected lag order and model
    if (is.null(selected_lag) || aicc_value < selected_lag$aicc) {
      selected_lag <- list(aic = aic_value, aicc = aicc_value, bic = bic_value, lag_order = j, model = var_model)
    }
  }
  
  # Save the parameters of the best model to the parameter matrix
  parameter_matrix[i, 1] <- gt_dss_names[i]
  parameter_matrix[i, 2] <- selected_lag$aic
  parameter_matrix[i, 3] <- selected_lag$aicc
  parameter_matrix[i, 4] <- selected_lag$bic
  parameter_matrix[i, 5] <- selected_lag$lag_order
  
  
  # Get the best fitted VAR model based on AICc
  best_var_model <- selected_lag$model
  
  # Print the summary of the best fitted model
  cat("Selected Lag Order:", selected_lag$lag_order, "\n")
  cat("AIC:", selected_lag$aic, "\n")
  cat("AICc:", selected_lag$aicc, "\n")
  cat("BIC:", selected_lag$bic, "\n")
  cat("Best Model Summary:\n")
  summary(best_var_model) %>% print
  
  # Get p-values for each variable in the selected model
  p_values <- coeftest(best_var_model)
  
  # Print the p-values
  cat("P-Values:\n")
  print(p_values)
  
  # Add the forecast result to a list or data frame for further analysis or visualization
  # (not included in the code snippet)
}

all_var_models %>% dim
rownames(all_var_models) <- c(1:nrow(all_var_models))
all_var_models











































































































































# Assuming ts_real_inf_3 and ext_regressors are time series objects with the same frequency and date range

start_date <- start(ts_real_inf_3)
start_date_offset <- start_date + c(floor((window_size)/12), ((window_size)%%12)+1)
end_date <- end(ts_real_inf_3)
n_iterations <- length(ts_real_inf_3) - window_size - n_ahead

forecasts_ex <- list()

last <- FALSE

for (i in 1:n_iterations) {
  print(i/n_iterations)
  start_window <- start_date
  end_date_current <- start_date_offset + c(floor((i)/12), ((i)%%12)+1)
  end_window <- c(end_date_current[1]+floor(end_date_current[2]/12), (end_date_current[2]%%12)+1)
  
  print(c(start_window, end_window))
  
  ts_window <- window(ts_real_inf_3, start = start_window, end = end_window)
  ext_regressors_window <- window(ext_regressors, start = start_window, end = end_window)
  
  #model <- auto.arima(ts_window, xreg = ext_regressors_window)
  model <- try(forecast::Arima(ts_window, order = c(1,1,2), xreg = ext_regressors_window))
  
  if (inherits(arima_model, "try-error"))  {
    # Print a message and continue to the next iteration if an error occurred
    cat("Error encountered for ARIMA(", i) # . Skipping this model.\n")
    forecasts_ex[[i]] <- 0
    last <- TRUE
    next  # Continue to the next iteration
  }
  
  if (last){
    last <- FALSE
    
  }

  

  start_forecast <- end_window + c(0, 1)
  end_forecast <- start_forecast + c(0, n_ahead - 1)
  ext_regressors_forecast <- window(ext_regressors, start = start_forecast, end = end_forecast)
  
  # Use forecast::forecast instead of the base forecast function
  
  
  forecast_result <- forecast::forecast(model, h = n_ahead, xreg = ext_regressors_forecast)
  
  forecasts_ex[[i]] <- forecast_result
}

point_estimates_ew <- do.call(c, lapply(forecasts_ex, function(x) x$mean))

print(point_estimates_ew)


{
  
  point_estimates_ew <- do.call(c, lapply(forecasts_ex, function(x) x$mean))
  
  # Print the point estimates
  print(point_estimates_ew)
  
  # Create a new time series object with the forecasted values and their respective time indexes
  start_forecast_all <- start_date + c(0, window_size)
  end_forecast_all <- end_date
  ts_forecast <- ts(point_estimates_ew, start = start_forecast_all, end = end_forecast_all, frequency = frequency(ts_real_inf_3))
  
  # Plot the actual and forecasted values together
  ts.plot(ts_real_inf_3, ts_forecast, col = c("black", "red"), lty = c(1, 1), main = "Actual vs. Forecasted Values", xlab = "Time", ylab = "Value")
  legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = c(1, 1), bty = "n")
  
  
  # Extract the actual values for which we have forecasts
  actual_values <- window(ts_real_inf_3, start = start_forecast_all, end = end_forecast_all)
  
  # Calculate quality measures
  mae <- mean(abs(actual_values - point_estimates_ew))
  mse <- mean((actual_values - point_estimates_ew)^2)
  rmse <- sqrt(mse)
  
  cat("Mean Absolute Error (MAE):", mae, "\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
}





























# Set the window size and number of periods ahead to forecast
window_size <- 30
n_ahead <- 1

# Set the start and end dates for the time series
start_date <- start(ts_real_inf_3)
end_date <- end(ts_real_inf_3)

# Set up empty vectors to store the forecasts and actuals
expanding_forecasts <- numeric()
expanding_actuals <- numeric()

# Loop over the data and perform the expanding window forecasts
for (i in  1:length(ts_real_inf_3 - window_size)) {
  
  # Set the end date for the current window
  end_window <- time(ts_real_inf_3)[i + window_size]
  
  # Create the training data
  ts_train <- window(ts_real_inf_3, end = end_window - 1)
  xreg_train <- window(ext_regressors, end = end_window - 1)
  
  # Fit the model using the training data
  model <- Arima(ts_train, order = c(1,1,1), xreg = xreg_train)
  
  # Make a forecast for the next n periods
  forecast_result <- forecast(model, h = n_ahead, xreg = ext_regressors[i:(i+n_ahead-1),])
  
  # Store the forecasted values and actual values for the expanding window
  expanding_forecasts <- c(expanding_forecasts, as.numeric(forecast_result$mean))
  expanding_actuals <- c(expanding_actuals, ts_real_inf_3[i:(i+n_ahead-1)])
  
}

# Calculate the expanding window forecast errors
expanding_errors <- expanding_actuals - expanding_forecasts

# Print the forecast errors
print(expanding_errors)
















class(tabulka_arima_modelu$row_names)

x <- c(1:8)

opposite_lag(x, 1)
















colnames(tabulka_arima_modelu)
colnames(originalni_modely)


tabulka_arima_modelu
originalni_modely

tabulka_arima_modelu %>% dim
originalni_modely %>% dim









merged_data <- merge(tabulka_arima_modelu, originalni_modely,
                     by.x = c("AR", "I", "MA"),
                     by.y = c("dAR", "dI", "dMA"),
                     all.x = FALSE, all.y = FALSE)

tabulka_arima_modelu$AR %>% class
originalni_modely[,"dAR"] %>% class


merged_data <- merge.data.table(x = tabulka_arima_modelu, y = originalni_modely,
                                      by.x = c("AR", "I", "MA"),
                                      by.y = c("dAR", "dI", "dMA"), all = FALSE, all.x = FALSE,
                                      all.y = FALSE, allow.cartesian=FALSE)



merged_data
merged_data %>% dim()










# Plot the impulse response function for the Google Trends variable
irf_plot <- plot(vars::irf(var_model, impulse = "ts_gt_inf", response = "ts_real_inf", n.ahead = 12))










data1 <- data.frame(A = 1:3, B = 4:6)
data2 <- data.frame(A = 7:9, B = 10:12)

# Use rbind() to combine the data frames by rows
combined_data <- rbind(data1, data2)

# Print the combined data frame
debug_print(combined_data)











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









