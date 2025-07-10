source("funcoes.R")
library(tidyverse)
library(forecast)
library(TSstudio)
library(lmtest)
library(MASS)


dados<-read.table("TREE_RIO_CISNE.txt", header = T)

serie<-ts(dados$trsgi, frequency = 12)


glambda<-BoxCox.lambda(serie,method = c("guerrero"))
llambda<-BoxCox.lambda(serie, method = c("loglik"))
bc21.dados<-BoxCox(serie, glambda)
bc22.dados<-BoxCox(serie, llambda)

shapiro.test(bc21.dados)
shapiro.test(bc22.dados)


#jarque.bera.test(bc21.dados)
#ggqqplot(bc22.dados) #deixar esse graf?? -CAROL

#acf(serie, lag.max = 350)

#plot(decompose(serie))

### Trend and Unit Root----
raiz_unit(bc22.dados)
tend_determ(bc22.dados)
sazonalidade(bc22.dados)

serie_part <- ts_split(bc22.dados, sample.out = 12)

serie_train <- serie_part$train
serie_test <- serie_part$test


serie_part <- ts_split(serie, sample.out = 12)

serie_train <- serie_part$train
serie_test <- serie_part$test


### MODEL 1----

mod<-forecast::auto.arima(serie_train, seasonal = FALSE)
forecast::checkresiduals(mod$residuals)

summary(mod)
coeftest(mod)

mod_fc <- forecast(mod, h=12)

accuracy(mod_fc, serie_test)

test_forecast(actual = serie,
              forecast.obj = mod_fc,
              test = serie_test)


### Modelo 1.1 ----

mod1.1<-forecast::Arima(serie_train, order = c(3,0,0))
forecast::checkresiduals(mod1.1$residuals)
coeftest(mod1.1)


mod1.1_fc <- forecast::forecast(mod1.1, h = 12)

test_forecast(actual = serie,
              forecast.obj = mod1.1_fc,
              test = serie_test)

acuracia_mod1.1 <- forecast::accuracy(mod1.1_fc, serie_test)
acuracia_mod1.1 <- round(acuracia_mod1.1[, c(1:3,5)],4)




mod1.2 <- forecast::Arima(serie_train, order = c(0,0,10))
coeftest(mod1.2)

forecast::checkresiduals(mod1.2$residuals)

mod1.2_fc <-forecast::forecast(mod1.2, h = 12)

acuracia_mod1.2 <- forecast::accuracy(mod1.2_fc, serie_test)

acuracia_mod1.2 <- round(acuracia_mod1.2[, c(1:3,5)], 4)

### MODEL 2 - BOX COX----

mod2<-forecast::Arima(serie_train, order = c(0,0,10))
coeftest(mod2)

forecast::checkresiduals(mod2$residuals)

mod2_fc <- forecast::forecast(mod2, h = 12)


accuracy(mod2_fc, serie_test)

test_forecast(actual = bc22.dados,
              forecast.obj = mod2_fc,
              test = serie_test)


### MODEL 3 - BOX COX ----

mod3<-forecast::Arima(serie_train, order = c(3,0,0))
coeftest(mod3)

forecast::checkresiduals(mod3$residuals)

mod3_fc <- forecast::forecast(mod3, h = 12)

accuracy(mod3_fc, serie_test)


test_forecast(actual = bc22.dados,
              forecast.obj = mod3_fc,
              test = serie_test)


#####

fit_nn<-forecast::nnetar(serie, lambda = "auto")

forecast::forecast(fit_nn, h=12) |> forecast::autoplot()


fit_nn_fc <- forecast::forecast(fit_nn, h=12)


accuracy(fit_nn_fc, serie_test)


forecast::checkresiduals(fit_nn$residuals)

