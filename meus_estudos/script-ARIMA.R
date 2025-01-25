USgas<-TSstudio::USgas

# PREPARANDO OS DADOS 

# BASE DE TREINO
treino <- window(USgas, start = time(Usgas)[1],
                 end = time(USgas)[length(USgas)-12])
# BASE DE TESTE
teste <- window(USgas, start = time(USgas)[length(USgas)-12 + 1],
                end = time(USgas)[length(USgas)])

TSstudio::ts_info(teste)



# USANDO ts_split 

USgas_partitions <- TSstudio::ts_split(USgas, sample.out = 12)

treino <- USgas_partitions$train

teste <- USgas_partitions$test

# DETERMIAR O MODELOS AUTOMATICO PELA FUNÇÃO AUTO ARIMA

md <- forecast::auto.arima(treino)

md


# --- EXPLICAÇÃO DO MODELO md
# ARIMA(2,1,1): Refere-se ao componente não sazonal do modelo
# 2 (AR): Há dois termos autoregressivos (lag dependente do passado)
# 1 (I): Indica uma diferenciação para tornar a série estacionária
# 1 (MA): Há um termo de média móvel

# --- (2,1,1)[12]: Refere-se ao componente sazonal
# 2 (SAR): Há dois termos autoregressivos sazonais (sazonalidade de 12 períodos).
# 1 (SI): Indica uma diferenciação sazonal
# 1 (SMA): Um termo de média móvel sazonal

# --- Coeficientes Estimados
# ar1 (0.4301): Primeiro termo autoregressivo não sazonal
# ar2 (-0.0372): Segundo termo autoregressivo não sazonal
# ma1 (-0.9098): Primeiro termo de média móvel não sazonal
# sar1 (0.0117): Primeiro termo autoregressivo sazonal
# sar2 (-0.2673): Segundo termo autoregressivo sazonal
# sma1 (-0.7431): Primeiro termo de média móvel sazonal
# s.e.: Desvio padrão associado a cada coeficiente (mede a incerteza da estimativa)

# Os coeficientes são significativos quando o valor absoluto do coeficiente é muito maior que o desvio padrão. 
# Por exemplo, ma1 (-0.9098) tem um desvio padrão relativamente pequeno (0.0452), sugerindo significância


# -- VARIÂNCIA DOS RESÍDUOS 
# sigma² = 10446: Variância do erro (resíduo) do modelo. Um valor menor sugere um ajuste melhor

# -- likelihood 
# -1292.83: O logaritmo da verossimilhança indica a adequação do modelo aos dados. 
# Valores mais altos indicam melhor ajuste.


# --- CRITÉRIOS DE INFORMAÇÃO 
# AIC (2599.67): Critério de Informação de Akaike. É usado para comparar modelos: menor valor indica melhor ajuste, penalizando a complexidade.

# AICc (2600.22): Versão corrigida do AIC para amostras menores

# BIC (2623.2): Critério de Informação Bayesiano. Penaliza mais severamente modelos complexos (maior número de parâmetros).


# --- VERIFICAR OS RESÍDUOS DO MODELO md  

f.arima1 <- forecast::checkresiduals(md)

# --- OBTENDO O VALOR PREVISTO PARA 12 PERIODOS USANDO O MODELO md

fc <- forecast::forecast(md, h=12)

# --- TESTANDO A ACURÁCIA DO MODELO md COMPARANDO A SÉRIE DE TESTE

forecast::accuracy(fc, teste)


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














