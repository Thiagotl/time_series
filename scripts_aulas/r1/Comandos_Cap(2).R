#-------------Packages----------------------------------------------------------
library(ggplot2)
library(ggfortify)
library(forecast)
#-------------------------------------------------------------------------------
set.seed(1087)
#Ruído Branco
rb<-rnorm(200)
rb<-ts(rb)
autoplot(rb)+labs(x = "Time", y = "Ruído Branco")+theme_minimal()
#ARMA(1,2)
arma.sim<-arima.sim(model=list(ar=c(0.9),ma=c(-.7,.1)),n=200)
arma.sim<-ts(arma.sim)
autoplot(arma.sim)+labs(x = "Time", y = "ARMA(1,2)")+theme_minimal()
#-------------------------------------------------------------------------------
set.seed(1087)
#Ruído Branco
rb<-rnorm(200)
rb<-ts(rb)
autoplot(rb)+labs(x = "Time", y = "Ruído Branco Simulado")+theme_minimal()
ggAcf(rb, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral",title="")+theme_minimal()
ggAcf(rb, lag.max=100,type = c("partial"))+labs(y = "FACP Amostral",title="")+theme_minimal()
#-------------------------------------------------------------------------------
# Random walk
random_walk<-function(n,prob)
{
  x<-rbinom(n,1,prob)
  y<-ifelse(x == 0, -1, 1)
  return(y)
}
set.seed(1087)
n<-200
## initialize {x_t} and {w_t}
rw <- ww <- rnorm(n = n, mean = 0, sd = 1)
rw <- ww <- random_walk(n,prob=0.5)
## compute values 2 thru TT
for (t in 2:n) {
  rw[t] <- rw[t - 1] + ww[t]
}
rw<-ts(rw)
autoplot(rw)+labs(x = "Time", y = "Passeio Aleatório Simulado")+theme_minimal()
ggAcf(rw, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral",title="Passeio Aleatório")+theme_minimal()
ggAcf(rw, lag.max=100,type = c("partial"))+labs(y = "FACP Amostral",title="Passeio Aleatório")+theme_minimal()
#-------------------------------------------------------------------------------
set.seed(1087)
#Ruído Branco
rb<-rnorm(200)
ggAcf(rb, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral",title="")+theme_minimal()
acf(rb,lag.max = 100,type="correlation")
#-------------------------------------------------------------------------------







