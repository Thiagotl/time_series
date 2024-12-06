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

modelo_ets <- ets(data_serie)

summary(modelo_ets)

tsdisplay(modelo_ets$residuals)
Box.test(modelo_ets$residuals,lag=10)

modelo_ets$residuals

checkresiduals(modelo_ets)

ggqqplot(modelo_ets$residuals)+ggtitle("Res?duos Modelo SES")


data_serie_diff<-diff(data_serie,differences = 1)
plot(data_serie_diff)
acf(data_serie_diff)
adf.test(data_serie_diff, alternative = "stationary")

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



tend_determ(data_serie_diff)

raiz_unit(data_serie_diff)

sazonalidade(data_serie_diff)

plot(data_serie_diff)

modelo <- lm(data_serie_diff ~ seq_along(data_serie_diff))
summary(modelo)
# varificando ARIMA

modelo_arima <- auto.arima(data_serie)
summary(modelo_arima)



checkresiduals(modelo_arima)


previsao <- forecast(modelo_arima, h = 12)
plot(previsao)


tsdisplay(modelo_arima$residuals)
Box.test(modelo_arima$residuals,lag=10)

# validaçaõ modelo ----
# train_size <- floor(0.8 * length(data_serie_diff))  # 80% para treino
# train <- data_serie_diff[1:train_size]  # Conjunto de treinamento
# test <- data_serie_diff[(train_size + 1):length(data_serie_diff)]  # Conjunto de teste
# 
# 
# modelo_ets <- ets(train)
# forecast_ets <- forecast(modelo_ets, h = length(test))  # Previsões para o tamanho do teste
# 
# 
# 
# library(Metrics)
# 
# # Previsões e valores reais
# pred <- as.numeric(forecast_ets$mean)
# actual <- as.numeric(test)
# 
# # Métricas de avaliação
# mae <- mae(actual, pred)  # Erro Absoluto Médio
# rmse <- rmse(actual, pred)  # Raiz do Erro Quadrático Médio
# mape <- mape(actual, pred)  # Erro Percentual Absoluto Médio
# 
# # Exiba os resultados
# cat("MAE:", mae, "\nRMSE:", rmse, "\nMAPE:", mape, "\n")
# 
# plot(test, type = "l", col = "red", main = "Previsões vs Valores Reais", ylab = "Valores")
# lines(pred, col = "blue")
# legend("topleft", legend = c("Real", "Previsão"), col = c("red", "blue"), lty = 1)
# 
# checkresiduals(modelo_ets)


