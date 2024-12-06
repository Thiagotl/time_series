#http://leg.ufpr.br/~lucambio/CE017/20192S/tsar.pdf
#https://www.kaggle.com/code/farzadnekouei/gold-price-prediction-lstm-96-accuracy



library(readr)
library(tidyverse)
library(lubridate)
library(tseries)




dados<- read_csv("trab_final/dados/Gold Price (2013-2023).csv")

View(dados)


dados <- dados |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

media_mensal <- dados |> 
  mutate(Month = floor_date(Date, "month")) |> 
  group_by(Month) |> 
  summarise(Average_Price = mean(Price, na.rm = TRUE))
View(media_mensal)


data_series<-ts(media_mensal$Average_Price, frequency=12, start=c(2013,1))
plot(data_series)




## Verificar a Estacionaridade ------------------------------------------------ 

adf.test(data_series, alternative = "stationary")



library(urca)

teste_kpss <- ur.kpss(data_series)

summary(teste_kpss)






