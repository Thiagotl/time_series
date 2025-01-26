suppressMessages(library(forecast))
suppressMessages(library(tseries))
suppressMessages(library(tidyverse))
suppressMessages(library(tidyquant))
library(readr)

source("meus_estudos/functions.R")



dados <- read_csv("meus_estudos/Gold Price (2013-2023).csv")

View(dados)

attach(dados)
dados <- dados |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))


dados <- dados |> 
  select(Date, Price)

media_mensal <- dados |>
  mutate(Month = floor_date(Date, "month")) |>
  group_by(Month) |>
  summarise(Average_Price = mean(Price, na.rm = TRUE))


#View(media_mensal)

serie_ouro<-ts(media_mensal$Average_Price, frequency=12, start=c(2013,1))

raiz_unit(serie_ouro)

tend_determ(serie_ouro)

sazonalidade(serie_ouro)

# # Identificar os dias da semana presentes
# dias_semana_presentes <- unique(weekdays(dados$Date))
# 
# # Verificar se todos os dias estão presentes
# dias_semana_completos <- c("segunda-feira", "terça-feira", "quarta-feira", 
#                            "quinta-feira", "sexta-feira", "sábado", "domingo")
# 
# # Comparar os dias presentes com os esperados
# dias_faltantes <- setdiff(dias_semana_completos, dias_semana_presentes)
# 
# if (length(dias_faltantes) == 0) {
#   print("O dataset contém todos os dias da semana.")
# } else {
#   print("O dataset não contém os seguintes dias da semana:")
#   print(dias_faltantes)
# }
# 
# # REMOVER OS DIAS COM SÁBADO 
# 
# dados_sem_sabado<-dados[weekdays(dados$Date)!="sábado",]
# 
# unique(weekdays(dados_sem_sabado$Date))


acf(serie_ouro)

pacf(serie_ouro)

# PARTIÇÃO DO DATASET EM TREINO E TESTE

ouro_partitions <- TSstudio::ts_split(serie_ouro, sample.out = 12)

ouro_treino <- ouro_partitions$train
ouro_teste <- ouro_partitions$test

# PRIMEIRO MODELO - FIT1

md <- auto.arima(ouro_treino)
md

# CHECAGEM DOS RESIDUOS DO md

checkresiduals(md)

# TESTE PARA INDEPENDENCIA DOS RESIDUOS 
Box.test(md$residuals,lag=10)


# --- OBTENDO O VALOR PREVISTO PARA 12 PERIODOS USANDO O MODELO md

fc <- forecast(md,12)
fc

# --- TESTANDO A ACURÁCIA DO MODELO md COMPARANDO A SÉRIE DE TESTE
accuracy(fc, ouro_teste)


TSstudio::test_forecast(actual = serie_ouro,
                        forecast.obj = fc,
                        test = ouro_teste) # o resultado foi uma porcaria


serie_ouro_diff<-diff(serie_ouro)



# USAR O MODELO naive COMO REFERENCIA "Benchmark"

naive_model <- forecast::naive(treino, h = 12)


#Analisando a acurácia do modelo naive
forecast::accuracy(naive_model, teste)

#Obtendo o modelo final
md_final <- forecast::auto.arima(USgas)
fc_final <- forecast::forecast(md_final, h = 12)


TSstudio::plot_forecast(fc_final,
                        title = "Previsão para o consumo de Gás Natural USA",
                        Xtitle = "Ano",
                        Ytitle = "Bilhões de pés cúbicos")

fc_final2 <- forecast::forecast(md_final,
                                h = 60,
                                level = c(80, 90))

# Visualizando o valor previsto e intervalo de confiança
fc_final2 


# COMO RESULTADO FINAL, OS MODELOS AQUI FICARAM UM LIXO. 



gold_data <- tq_get("GLD", from = "2024-01-01", to = "2025-01-10")

gold_data<-gold_data |> 
  select(date, open)


gld_ts <- ts(gold_data$open, start = c(2024, 1), frequency = 252)
plot(gld_ts)

acf(gld_ts)
pacf(gld_ts)

raiz_unit(gld_ts)
tend_determ(gld_ts)


train <- window(gld_ts, start = c(2024, 1), end = c(2024, 252))
test <- window(gld_ts, start = c(2025, 1))

# Plotar as duas partes
plot(train, main = "Conjunto de Treinamento", col = "green", ylab = "Preço de Abertura")
plot(test, main = "Conjunto de Teste", col = "red", ylab = "Preço de Abertura")



md <- forecast::auto.arima(train)
md
arima <- forecast::checkresiduals(md)

fc <- forecast::forecast(md, h=5)

forecast::accuracy(fc, test)

length(gld_ts)      
length(test)        
length(fc$mean) 


TSstudio::test_forecast(actual = gld_ts,
                        forecast.obj = fc,
                        test = test)



