library(TSstudio)
library(forecast)
source("meus_estudos/functions.R")

View(Coffee_Prices)

class(Coffee_Prices)


cafe <- Coffee_Prices[, "Robusta"]

sazonalidade(cafe)
raiz_unit(cafe)
tend_determ(cafe)

acf(cafe)
pacf(cafe)

ts_lags(cafe)


# DADOS PARA TRABALHAR


cafe_particao <- ts_split(cafe, sample.out = 12)


cafe_treino <- cafe_particao$train

cafe_teste <- cafe_particao$test  



# MODELO

mod<-auto.arima(cafe_treino)
mod
checkresiduals(mod)

check_res(mod)


Box.test(mod$residuals, lag = 1)


Box.test(mod$residuals, lag = 1, type = "Ljung-Box")


Acf(residuals(mod))



arima_diag(ts.obj = cafe)


fc <- forecast(mod, h=12)
fc

plot(fc)
accuracy(fc, cafe_teste)


test_forecast(actual = cafe,
              forecast.obj = fc,
              test = cafe_teste)



# # USAR O MODELO naive COMO REFERENCIA "Benchmark"
# 
# naive_model <- naive(cafe_treino, h = 12)
# 
# 
# #Analisando a acurácia do modelo naive
# accuracy(naive_model, cafe_teste)
# 
# #Obtendo o modelo final
# md_final <- forecast::auto.arima(cafe)
# fc_final <- forecast::forecast(md_final, h = 12)
# 
# 
# TSstudio::plot_forecast(fc_final,
#                         title = "X",
#                         Xtitle = "X",
#                         Ytitle = "Y")



#### TESTANDO OUTRAS OPÇÕES DE MODELOS



mod_ets <- ets(cafe_treino)

mod_ets
checkresiduals(mod_ets)  # Avalia a qualidade dos resíduos
autoplot(forecast(mod_ets, h = 12))  # Faz previsões

fc_ets<-forecast(mod_ets, h=12)


test_forecast(actual = cafe,
              forecast.obj = fc_ets,
              test = cafe_teste)


mod_tbats <- tbats(cafe_treino)
checkresiduals(mod_tbats)  # Avalia resíduos
autoplot(forecast(mod_tbats, h = 12))  # Faz previsões


fc_tbats<-forecast(mod_tbats, h=12)


test_forecast(actual = cafe,
              forecast.obj = fc_tbats,
              test = cafe_teste)
# Resultados, podre


########



mod_nn <- nnetar(cafe_treino)
forecast_nn <- forecast(mod_nn, h = 12)
autoplot(forecast_nn)


autoplot(forecast_nn) + 
  autolayer(cafe_teste, series = "Real", color = "red") +
  ggtitle("Previsão do NNAR vs. Valores Reais") +
  theme_minimal()

checkresiduals(mod_nn)




fc_nn<-forecast(mod_nn, h=12)


test_forecast(actual = cafe,
              forecast.obj = fc_nn,
              test = cafe_teste)


accuracy(fc_nn,cafe_teste)


# Comparando métricas dos modelos
acc_arima <- accuracy(fc, cafe_teste)
acc_ets <- accuracy(fc_ets, cafe_teste)
acc_tbats <- accuracy(fc_tbats, cafe_teste)
acc_nn <- accuracy(fc_nn, cafe_teste)

# Criando uma tabela de métricas
library(dplyr)

resultados <- data.frame(
  Modelo = c("ARIMA", "ETS", "TBATS", "NNAR"),
  RMSE = c(acc_arima["Test set", "RMSE"], 
           acc_ets["Test set", "RMSE"],
           acc_tbats["Test set", "RMSE"], 
           acc_nn["Test set", "RMSE"]),
  MAE = c(acc_arima["Test set", "MAE"], 
          acc_ets["Test set", "MAE"], 
          acc_tbats["Test set", "MAE"], 
          acc_nn["Test set", "MAE"]),
  MAPE = c(acc_arima["Test set", "MAPE"], 
           acc_ets["Test set", "MAPE"], 
           acc_tbats["Test set", "MAPE"], 
           acc_nn["Test set", "MAPE"])
)

# Visualizando os resultados
print(resultados)


par(mfrow=c(2,2))  # Dividir a tela para 4 gráficos

checkresiduals(mod)      # ARIMA
checkresiduals(mod_ets)  # ETS
checkresiduals(mod_tbats) # TBATS
checkresiduals(mod_nn)   # NNAR





