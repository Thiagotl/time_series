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

Box.test(mod$residuals, lag = 12)

Box.test(mod$residuals, lag = 12, type = "Ljung-Box")

arima_diag(ts.obj = cafe)


fc <- forecast(mod, h=12)
fc


accuracy(fc, cafe_teste)


test_forecast(actual = cafe,
              forecast.obj = fc,
              test = cafe_teste)



# USAR O MODELO naive COMO REFERENCIA "Benchmark"

naive_model <- naive(cafe_treino, h = 12)


#Analisando a acurácia do modelo naive
accuracy(naive_model, cafe_teste)

#Obtendo o modelo final
md_final <- forecast::auto.arima(cafe)
fc_final <- forecast::forecast(md_final, h = 12)


TSstudio::plot_forecast(fc_final,
                        title = "X",
                        Xtitle = "X",
                        Ytitle = "Y")

# fc_final2 <- forecast::forecast(md_final,
#                                 h = 60,
#                                 level = c(80, 90))
# 
# # Visualizando o valor previsto e intervalo de confiança
# fc_final2 
# 
# 
# f.arima1 <- forecast::checkresiduals(md)
# 
# 







