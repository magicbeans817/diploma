library(readxl)
library(dplyr)




setwd("C:/Users/jsuch/Desktop/diploma") #set your working directory

# Data o inflaci, pristup:
# https://www.czso.cz/csu/czso/inflation_rate


data <- list()

pocet_sheetu <- 4

for (i in 1:pocet_sheetu) {
  data[[i]] <- read_excel("inflace.xlsx", sheet = i)
}

data[[2]] %>% head

for (i in 1:pocet_sheetu) {
  print(i)
  x <- data[[i]]
  y <- data.frame(x[,-1], row.names = x$Year)
  data[[i]] <- y
  
}



x <- data.frame(x, row.names = x[,"Year"])
row.names(x) <- x[,"Year"]
x %>% dim
x$Year %>% length

y <- data.frame(x[, -1], row.names = x$Year)


tabulka_1 <- data[[1]]

x <- ts(tabulka_1)
x

x <- c(t(tabulka_1))
x %>% is.vector
x











