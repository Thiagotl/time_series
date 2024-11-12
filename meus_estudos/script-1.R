library(ggplot2)
library(ggfortify)


set.seed(1087)

rb<-rnorm(200)
rb<-ts(rb)

autoplot(rb)+labs(x = "Time", y = "Ruído Branco")+theme_minimal()

#ARMA(1,2)
arma.sim<-arima.sim(model=list(ar=c(0.9),ma=c(-.7,.1)), n=200)
arma.sim<-ts(arma.sim)
