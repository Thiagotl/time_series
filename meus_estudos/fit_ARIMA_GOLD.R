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


View(media_mensal)

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


# PARTIÇÃO DO DATASET EM TREINO E TESTE

ouro_partitions <- TSstudio::ts_split(serie_ouro, sample.out = 365)

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

fc <- forecast(md,  h = length(ouro_teste))
fc

# --- TESTANDO A ACURÁCIA DO MODELO md COMPARANDO A SÉRIE DE TESTE



TSstudio::test_forecast(actual = USgas,
                        forecast.obj = fc,
                        test = teste)



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











