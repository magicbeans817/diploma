library(readxl)
library(dplyr)




setwd("C:/Users/jsuch/Desktop/diploma") #set your working directory

# Data o inflaci, pristup:
# https://www.czso.cz/csu/czso/inflation_rate


data <- list()

for (i in 1:4) {
  data[[i]] <- read_excel("inflace.xlsx", sheet = i)
}
data[[1]] %>% head

tabulka_1 <- data[[1]]
x <- c(t(tabulka_1))
x %>% is.vector
x











