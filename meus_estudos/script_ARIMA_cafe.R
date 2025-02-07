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


# TESTANDO OUTROS MODELOS


library(forecast)

# Modelo atual
mod1 <- Arima(cafe_treino, order = c(1,1,0), 
               seasonal = list(order=c(1,0,2), period = 12))

# Testando outro modelo com mais um termo AR
mod2 <- Arima(cafe_treino, order = c(2,1,0), 
              seasonal = list(order = c(0,0,2), period = 12))

# Testando outro modelo com mais um termo MA
mod3 <- Arima(cafe_treino, order = c(1,1,1), 
              seasonal = list(order = c(0,0,2) , period = 12))

# Testando um modelo com sazonalidade diferente
mod4 <- Arima(cafe_treino,order = c(1,1,0), 
              seasonal = list(order = c(0,0,2), period = 12))

mod5 <- Arima(cafe_treino, order = c(2,1,1), 
              seasonal = list(order = c(0,0,2), period = 12))

mod6<- Arima(cafe_treino, order = c(1,1,0), 
             seasonal = list(order = c(1,0,2), period = 12))

mod7<- Arima(cafe_treino, order = c(1,1,0), 
             seasonal = list(order = c(1,1,2), period = 12))

mod8<-Arima(cafe_treino, order = c(2,1,1), 
              seasonal = list(order = c(1,0,2), period = 12))

mod9<-Arima(cafe_treino, order = c(2,1,1), 
            seasonal = list(order = c(1,1,2), period = 12))

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9)  # Comparação pelo AIC
BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8,mod9)  # Comparação pelo BIC


checkresiduals(mod1)


forecast_mod5 <- forecast(mod5, h = 12)
forecast_mod1 <- forecast(mod1, h = 12)

autoplot(forecast_mod5)
autoplot(forecast_mod1)

accuracy(forecast_mod5, cafe_teste)
accuracy(forecast_mod1, cafe_teste)


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

library(prophet)

# Criando um vetor de datas correto
start_year <- start(cafe_treino)[1]  # Ano inicial (1960)
start_month <- start(cafe_treino)[2]  # Mês inicial (jan = 1)
num_periods <- length(cafe_treino)  # Total de observações

# Criando uma sequência de datas mensais a partir do início da série
dates <- seq(as.Date(paste0(start_year, "-", start_month, "-01")), 
             by = "month", length.out = num_periods)

# Criando o dataframe no formato exigido pelo Prophet
df <- data.frame(ds = dates, y = as.numeric(cafe_treino))

# Ajustando o modelo Prophet
mod_prophet <- prophet(df)

# Criando um dataframe para previsões futuras (12 meses à frente)
future <- make_future_dataframe(mod_prophet, periods = 12, freq = "month")

# Fazendo previsões
forecast_prophet <- predict(mod_prophet, future)

# Plotando as previsões
plot(mod_prophet, forecast_prophet)










#################

# Criar sequência de datas para os dados de teste
start_test_year <- start(cafe_teste)[1]  # Ano inicial dos dados de teste (2017)
start_test_month <- start(cafe_teste)[2] # Mês inicial

num_test_periods <- length(cafe_teste)  # Quantidade de observações

# Criar vetor de datas correspondente
test_dates <- seq(as.Date(paste0(start_test_year, "-", start_test_month, "-01")),
                  by = "month", length.out = num_test_periods)

# Criar dataframe dos valores reais de teste
df_test <- data.frame(ds = test_dates, y = as.numeric(cafe_teste))



df_test$ds <- as.Date(df_test$ds)
forecast_prophet$ds <- as.Date(forecast_prophet$ds)

# Selecionar previsões apenas para as datas de teste
forecast_test <- forecast_prophet[forecast_prophet$ds %in% df_test$ds, c("ds", "yhat")]

# Renomear colunas para facilitar a comparação
colnames(forecast_test) <- c("ds", "y_pred")


library(Metrics)

# Mesclar previsões com os dados reais
comparison <- merge(df_test, forecast_test, by = "ds")

# Cálculo das métricas de erro
rmse_value <- rmse(comparison$y, comparison$y_pred)
mae_value <- mae(comparison$y, comparison$y_pred)
mape_value <- mape(comparison$y, comparison$y_pred) * 100  # MAPE em %

# Exibir resultados
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("MAPE:", mape_value, "%\n")



library(ggplot2)

ggplot(comparison, aes(x = ds)) +
  geom_line(aes(y = y, color = "Real"), size = 1) +
  geom_line(aes(y = y_pred, color = "Previsto"), size = 1, linetype = "dashed") +
  labs(title = "Comparação das Previsões do Prophet vs. Valores Reais",
       x = "Ano",
       y = "Valor do Café",
       color = "Legenda") +
  theme_minimal()


