
data <- read.csv("data.csv")
View(data)
library(urca)


jotest <- urca::ca.jo(data, type = "trace", K = 2, ecdet = "none", spec = "transitory")
summary(jotest)
restrikce <- c(-1, 1)                                                                    # vektor, se kterym pracujeme
summary(urca::bh5lrtest(jotest, H = restrikce, r = 2))


data2 <- read.csv("data2.csv")
View(data2)

jotest <- urca::ca.jo(data2, type = "trace", K = 2, ecdet = "none", spec = "transitory")
summary(jotest)
restrikce <- c(-1, 1)                                                                    # vektor, se kterym pracujeme
summary(urca::bh5lrtest(jotest, H = restrikce, r = 2))
