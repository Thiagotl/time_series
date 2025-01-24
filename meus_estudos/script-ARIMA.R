USgas<-TSstudio::USgas

# PREPARANDO OS DADOS 

# BASE DE TREINO
treino <- window(USgas, start = time(Usgas)[1],
                 end = time(USgas)[length(USgas)-12])
# BASE DE TESTE
teste <- window(USgas, start = time(USgas)[length(USgas)-12 + 1],
                end = time(USgas)[length(USgas)])

TSstudio::ts_info(teste)
