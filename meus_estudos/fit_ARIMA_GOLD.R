suppressMessages(library(forecast))
suppressMessages(library(tseries))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(zoo))
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

raiz_unit(serie_ouro_diff)
tend_determ(serie_ouro_diff)

plot(serie_ouro_diff)

ouro_partitions_diff <- TSstudio::ts_split(serie_ouro_diff, sample.out = 12)

ouro_treino_diff <- ouro_partitions_diff$train
ouro_teste_diff <- ouro_partitions_diff$test




# -- MODELO PARA SERIE DIFERENCIADA 
md_diff<-auto.arima(ouro_treino_diff)
md_diff

# CHECAGEM DOS RESIDUOS DO md

checkresiduals(md_diff)

# TESTE PARA INDEPENDENCIA DOS RESIDUOS 
Box.test(md_diff$residuals,lag=10)

# --- OBTENDO O VALOR PREVISTO PARA 12 PERIODOS USANDO O MODELO md

fc_diff <- forecast(md_diff,12)
fc_diff

# --- TESTANDO A ACURÁCIA DO MODELO md COMPARANDO A SÉRIE DE TESTE
accuracy(fc_diff, ouro_teste_diff)


TSstudio::test_forecast(actual = serie_ouro_diff,
                        forecast.obj = fc_diff,
                        test = ouro_teste_diff)




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









