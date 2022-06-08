library(readxl)
library(dplyr)




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
inf_cmpy    <- ts(inf_cmpy, frequency = 12, start = c(1997, 1))
inf_cpm     <- ts(inf_cpm, frequency = 12, start = c(1997, 1))
inf_classic <- ts(inf_classic, frequency = 12, start = c(1998, 1))


######################################################################################################
# 2) Google trends data

gt <- read.csv("search_trends.csv", row.names = 1)
gt %>% head
gt%>% dim


inf_fd_s <- inf_fd[49:(length(inf_fd) - 8)]
inf_fd_s %>% length


model <- lm(inf_fd_s ~ gt$inflace)
summary(model)
plot(inf_fd_s)
plot(model$fitted)
lines(inf_fd_s)

modylek <- forecast::Arima(inf_fd, order = c(1, 0, 0))
summary(modylek)


pomocna <- diff(inf_fd)
modylek2 <- lm(inf_fd[-1] ~ pomocna)
summary(modylek2)

plot(inf_fd)
lines(modylek$fitted, col = "red")
























x <- data.frame(x, row.names = x[,"Year"])
row.names(x) <- x[,"Year"]
x %>% dim
x$Year %>% length

y <- data.frame(x[, -1], row.names = x$Year)


tabulka_1 <- data[[1]]


x <- c(t(tabulka_1))
x %>% is.vector
x











