#http://leg.ufpr.br/~lucambio/CE017/20192S/tsar.pdf
#https://www.kaggle.com/code/farzadnekouei/gold-price-prediction-lstm-96-accuracy


suppressMessages(library(forecast))
suppressMessages(library(tseries))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(readr))



dados<- read_csv("trab_final/dados/Gold Price (2013-2023).csv")

View(dados)


dados <- dados |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

media_mensal <- dados |> 
  mutate(Month = floor_date(Date, "month")) |> 
  group_by(Month) |> 
  summarise(Average_Price = mean(Price, na.rm = TRUE))
#View(media_mensal)


data_serie<-ts(media_mensal$Average_Price, frequency=12, start=c(2013,1))
plot(data_serie)




## Verificar a Estacionaridade ------------------------------------------------ 

adf.test(data_serie, alternative = "stationary")

decomposicao <- decompose(ts(data_serie, frequency = 12))

plot(decomposicao)


data_serie_diff<-diff(data_serie,differences = 1)
plot(data_serie_diff)
acf(data_serie_diff)
adf.test(data_serie_diff, alternative = "stationary")

# library(urca)
# 
# teste_kpss <- ur.kpss(data_serie)
# 
# summary(teste_kpss)


modelo_ets <- ets(data_serie)

summary(modelo_ets)
# RESULTADO - ERRO MULTIPĹICATIVO, SEM TENDÊNCIA E SEM SAZONALIDADE 



modelo_ets_diff <- ets(data_serie_diff)
summary(modelo_ets_diff)



tsdisplay(modelo_ets_diff$residuals)
Box.test(modelo_ets_diff$residuals,lag=10)

modelo_ets_diff$residuals

checkresiduals(modelo_ets_diff)

ggqqplot(modelo_ets_diff$residuals)+ggtitle("Res?duos Modelo SES")



# USANDO AS FUNÇÕES ---------------------------

source("trab_final/functions.R")


tend_determ(data_serie)

raiz_unit(data_serie)

sazonalidade(data_serie)







# varificando ARIMA

modelo_arima <- auto.arima(data_serie_diff)
summary(modelo_arima)



checkresiduals(modelo_arima)


previsao <- forecast(modelo_arima, h = 12)
plot(previsao)


