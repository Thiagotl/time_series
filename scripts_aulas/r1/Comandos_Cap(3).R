#-------------------------------------------------------------------------------
#Pacotes Capítulo 3
#-------------------------------------------------------------------------------
suppressMessages(library(forecast))
suppressMessages(library(ggplot2))
suppressMessages(library(tseries))
suppressMessages(library(plotly))
suppressMessages(library(magrittr))
suppressMessages(library(ggseas))
suppressMessages(library(lmtest))
suppressMessages(library(ggpubr))
suppressMessages(library(fma))
#Rtools::install_github("FinYang/tsdl") #pacote time series data library
suppressMessages(library(tsdl)) #devtools::install_github("FinYang/tsdl")
suppressMessages(library(TTR))
suppressMessages(library(readxl))
suppressMessages(library(tidyquant))
suppressMessages(library(dplyr))
suppressMessages(library(astsa))
suppressMessages(library(L1pack))
suppressMessages(library(webr))
suppressMessages(library(randtests))
suppressMessages(library(Kendall))
suppressMessages(library(FitAR)) #remotes::install_github("cran/FitAR")
suppressMessages(library(trend))
suppressMessages(library(moments))
#-------------------------------------------------------------------------------
#Análise Covid SM - Médias Móveis
#-------------------------------------------------------------------------------
covidsm<-read_excel("scripts_aulas/r1/Covid_SM(2).xlsx",col_types = c("date", "numeric"))
covidsm<-read_excel("scripts_aulas/r1/covid_sm.xlsx",col_types = c("date", "numeric"))
t<-1:length(covidsm$date) # tempo em dias
n<-length(covidsm$date) # vetor de datas 
smac<-SMA(x=covidsm$confirmed,n=7) #Calcula a Média Móvel Simples (MMS) dos casos confirmados, com uma janela de 7 dias.
n1<-length(smac) # Remove os primeiros 6 valores NA, deixando apenas as médias móveis válidas.
n2<-n1-6
smac<-smac[7:n1]
framed<-as.data.frame(cbind(rep("real",n2),covidsm[7:n1,]))
colnames(framed)<-c("Legenda","data","dados")
frame7<-as.data.frame(cbind(rep("MMS(7)",n2),covidsm[7:n1,1],smac))
colnames(frame7)<-c("Legenda","data","dados")
df<-rbind(framed,frame7)
ggplot(df, aes(x=data, y=dados,group=Legenda,colour=Legenda)) +
  geom_line()+
  scale_color_manual(values = c("real" = "black", "MMS(7)" = "blue3"))+
      labs(y = "Casos Confirmados", x = "Tempo (dias)")+theme_minimal()

#-------------------------------------------------------------------------------
boxplot(covidsm$confirmed)
dframe<-as.data.frame(cbind(rep("detrend",n2),covidsm[7:n1,1],covidsm[7:n1,2]-smac))
colnames(dframe)<-c("Legenda","data","detrend")
ggplot(dframe, aes(x=data, y=detrend,group=Legenda,colour=Legenda)) +
  geom_line()+scale_color_manual(values = c("detrend" = "black"))+
  labs(y = "Casos Confirmados Sem Tendência", x = "Tempo (dias)")+theme_minimal()


# autocorrelação da serie com diferentes defasagens 
# Em vários lags (como em 3, 6, 10 e 15), a autocorrelação é significativamente 
# diferente de zero (barras ultrapassam as linhas azuis) - Isso sugere que há dependência temporal nesses lags.
# A alternância entre correlações positivas e negativas indica a presença de algum padrão periódico ou sazonal na série


ggAcf(dframe$detrend, lag.max=20)+labs(y = "FAC Série sem Tendência",title="")+theme_minimal()

#-------------------------------------------------------------------------------
n<-length(covidsm[,1])
framed<-cbind(rep("d",n),(covidsm))
colnames(framed)<-c("cond","data","dados")
frame7<-as.data.frame(cbind(rep("k7",n),covidsm[,1],SMA(x=covidsm[,2],n=7)))
colnames(frame7)<-c("cond","data","dados")
frame14<-as.data.frame(cbind(rep("k14",n),covidsm[,1],SMA(x=covidsm[,2],n=14)))
colnames(frame14)<-c("cond","data","dados")
frame21<-as.data.frame(cbind(rep("k21",n),covidsm[,1],SMA(x=covidsm[,2],n=21)))
colnames(frame21)<-c("cond","data","dados")
df<-rbind(framed,frame7,frame14,frame21)
ggplot(df, aes(x=data, y=dados,group=cond,colour=cond)) +
  geom_line()+  
  scale_color_manual(values = c("d" = "black", "k7" = "blue3","k14" = "red", "k21" = "green"))+
  labs(y = "Casos Confirmados", x = "Tempo (dias)")+theme_minimal()

#-------------------------------------------------------------------------------
#Média Móvel Simples
#-------------------------------------------------------------------------------
# x = Vetor com os dados observados (série temporal original).
# k: Tamanho da janela para a média móvel (número de observações usadas no cálculo de cada média)
# h: Horizonte de previsão (número de passos à frente para prever)
# conf: Nível de confiança para os intervalos (ex.: 0.95 para 95% de confiança)




sma.for<-function(x,k,h,conf,plot=TRUE)
{
  z<-c()
  z$Dados<-x
  n<-length(x)
  y<-ysma<-SMA(x,k) #calcula a média móvel dos dados observados.
  z$fitted.values<-ysma
  xa<-c(x,rep(NA,h))
  xa[n+1]<-y[n]
  for(t in (n+1):(n+h)){
    y[t]<-mean(xa[(t-1):(t-k)])
    xa[t+1]<-y[t]
  }
  z$residuals<-c(x-ysma)[k:n]
  ve<-var(z$residuals)
  q<-qnorm((1-conf)/2,lower.tail = FALSE)
  ep<-q*sqrt(ve/k)
  f<-cbind(y[(n+1):(n+h)]-ep,y[(n+1):(n+h)],y[(n+1):(n+h)]+ep)
  if(plot){
    fore<-rbind(matrix(ncol=3,nrow=n),f)
    ysmaf<-c(ysma,rep(NA,h))
    dados<-c(x,rep(NA,h))
    df<-as.data.frame(cbind(t=1:(n+h),dados=dados,ysma=ysmaf,fore=fore))
    colnames(df)<-c("t","Dados","SMA","LI","Prev","LS")
    fig <- plot_ly(df,x = ~t,y = df$Dados, name = 'Data', type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~df$SMA, name = 'SMA', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~df$LS, name = 'LS', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~df$LI, name = 'LI', mode = 'lines')
    fig <- fig %>% add_trace(y = ~df$Prev, name = 'Forecast', mode = 'lines')
    fig <- fig %>% layout(title =paste0("Médias Móveis Simples - k=",k),
                          xaxis = list(title = 'Tempo'),
                          yaxis = list (title = 'Dados'))
    return(fig)
  }
  colnames(f)<-c(paste0("LI:",100*conf,"%"),"Previsão",paste0("LS:",100*conf,"%"))
  z$Forecast<-f
  return(z)
}
#-------------------------------------------------------------------------------
#Exemplo
#-------------------------------------------------------------------------------
x <-read_excel("scripts_aulas/r1/temp.xlsx")
x<-x$Temperatura
sma.for(x=x,k=15,h=10,conf=0.95,plot=TRUE)
sma.for(x=x,k=15,h=10,conf=0.95,plot=TRUE)



#-------------------------------------------------------------------------------
#Análise ICV - Tendência Polinomial - Reta
#-------------------------------------------------------------------------------
#Índice de Custo de Vida no Município de São Paulo. Fonte: Fundação Instituto de 
#Pesquisas Economicas (FIPE) - Função geom_smooth
#-------------------------------------------------------------------------------
icv<-c(238,251,256,263,270,275,280,290,298,305,310,318,
       329,343,359,375,383,393,400,407,415,424,436,449)
t<-1:length(icv)
df.icv<-as.data.frame(cbind(t,icv))
plot.ts(df.icv$icv)
ggplot(df.icv, aes(x=t, y=icv)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = TRUE)+
  labs(y = "?ndice de Custo de Vida", x = "Tempo")+theme_minimal()
#-------------------------------------------------------------------------------
#Análise ICV - Tendência Polinomial - Reta - MQO
#-------------------------------------------------------------------------------


icv <- read_excel("scripts_aulas/r1/icv.xlsx", col_types = c("date","numeric"))
n<-length(icv$icv)
t<-1:n
df.icv<-as.data.frame(cbind(t,icv))
reg<-lm(icv~t,data=df.icv)
summary(reg) 
confint(reg,level = 0.95)
y<-reg$coefficients[1]+reg$coefficients[2]*t
y<-round(y,1)
df1<-as.data.frame(cbind(rep("icv",n),icv))
df2<-as.data.frame(cbind(rep("ajustado",n),icv[-2], y))
colnames(df1)<-c("cond","t","dados")
colnames(df2)<-c("cond","t","dados")
df.icv0<-rbind(df1,df2)
ggplot(df.icv0, aes(x=t,y=dados,group=cond,colour=cond)) +
  geom_line(size = 1)+theme_minimal()
d.st<-icv[,2]-y #Série sem a Tendência
d.st<-ts(d.st,start=c(1976,1),frequency=12)
autoplot(d.st)+labs(x = "Meses", y = "Série sem Tendência")+theme_minimal()
ggAcf(d.st, lag.max=20)+labs(y = "FAC Série sem Tendência",title="")+theme_minimal()
ggtsdisplay(d.st,plot.type="scatter", theme=theme_bw())
shapiro.test(d.st)
Box.test(d.st, lag = 10, type = c("Box-Pierce"))

#-------------------------------------------------------------------------------
#Análise ICV - Tendância Polinomial - Reta - LAD
#-------------------------------------------------------------------------------
library(L1pack)#lad: Least absolute deviations regression
icv <- read_excel("icv.xlsx", col_types = c("date","numeric"))
plot.ts(icv$icv)
icv$icv[10]<-icv$icv[10]+500#Outlier
icv$icv[15]<-icv$icv[15]+500#Outlier
icv$icv[20]<-icv$icv[20]+500#Outlier
plot(icv$icv)
n<-length(icv$icv)
t<-1:n
df.icv<-as.data.frame(cbind(t,icv))
mod<-lm(icv~t,data=df.icv)
modr<-lad(icv~t,data=df.icv)
summary(mod) 
confint(mod,level = 0.95)
summary(modr)
confint(modr,level = 0.95)
y<-mod$coefficients[1]+mod$coefficients[2]*t
yr<-modr$coefficients[1]+modr$coefficients[2]*t
y<-round(y,1)
yr<-round(yr,1)
df1<-as.data.frame(cbind(rep("icv",n),icv))
df2<-as.data.frame(cbind(rep("ajustado",n),icv[-2], y))
df2r<-as.data.frame(cbind(rep("ajustador",n),icv[-2], yr))
colnames(df1)<-c("cond","t","dados")
colnames(df2)<-c("cond","t","dados")
colnames(df2r)<-c("cond","t","dados")
df.icvr<-rbind(df1,df2,df2r)
ggplot(df.icvr, aes(x=t,y=dados,group=cond,colour=cond)) +
  geom_point(size = 2)
d.str<-icv[,2]-yr #Série sem a Tendência
d.str<-ts(d.str,start=c(1976,1),frequency=12)
autoplot(d.str)+labs(x = "Meses", y = "Série sem Tendência")+geom_point()+theme_minimal()
plot(t,d.str)
#-------------------------------------------------------------------------------
#Lucro por Ação - Johnson e Johnson. Fonte: Pacote astsa R Core Team (2022).
#jj (Johnson & Johnson) - Tendência Polinomial - Quadrática
#-------------------------------------------------------------------------------
library(astsa)#Pacote Livro: Times Series With R.
t<-1:length(jj)
df.jj<-as.data.frame(cbind(t1=t,jj=jj))
autoplot(jj)+
  labs(y = "Lucro por A??o - Johnson e Johnson", x = "Tempo")+theme_minimal()
ggplot(df.jj, aes(x=t, y=jj)) +
  geom_line() +
  geom_smooth(method = lm,formula=y~x+I(x^2),se=TRUE)+
  labs(y = "Lucro por A??o - Johnson e Johnson", x = "Tempo")+theme_minimal()
#-------------------------------------------------------------------------------
n<-length(jj)
t<-1:n
df.jj<-as.data.frame(cbind(t1=t,t2=t^2,t3=t^3,jj=jj))
reg<-lm(jj~t1+t2,data=df.jj)
summary(reg) 
confint(reg,level = 0.95)
y<-reg$coefficients[1]+reg$coefficients[2]*t+reg$coefficients[3]*t^2
y<-round(y,1)
y<-ts(y,start=c(1960,1),frequency = 4)
plot.ts(cbind(jj,y),plot.type = c("single"),col=c("black","blue"),
        main = "Johnson e Johnson",xlab="Tempo",ylab="")
d.jj<-jj-y
autoplot(d.jj)+
  labs(y = "Lucro por ação - Johnson & Johnson - Sem Tendência", x = "Tempo")+theme_minimal()
ggAcf(d.jj, lag.max=20)+labs(y = "FAC Série sem Tendência",title="")+theme_minimal()

#-------------------------------------------------------------------------------
#Analise de Residuos
#--------------------------------------------------------------------------------
ggqqplot(d.jj)
shapiro.test(d.jj)
#-------------------------------------------------------------------------------
ggtsdisplay(d.jj,plot.type="scatter", theme=theme_bw())
ggtsdisplay(d.jj,plot.type="histogram", theme=theme_bw())
#-------------------------------------------------------------------------------
#Dados Nao Correlacionados
#-------------------------------------------------------------------------------
Box.test(d.jj, lag = 10, type = c("Box-Pierce"))
Box.test(d.jj, lag = 10, type = c("Ljung-Box"))
res_test(d.jj,lag=20,type=c("Box-Pierce"))
#-------------------------------------------------------------------------------
#Série Temporal Simulada - Ajuste Função Não Linear utilizando a função stat_smooth
#-------------------------------------------------------------------------------
#Simulação da Série Temporal 
#-------------------------------------------------------------------------------
a<-2
b<-0.3
n<-100
t<-1:n
e<-rnorm(n,sd=sqrt(2))
acf(e)
y<-a*(1+t)^b+e
plot.ts(y)
#-------------------------------------------------------------------------------
y<-ts(y)
t<-1:length(y)
df.jj<-as.data.frame(cbind(t1=t,y=y))
autoplot(y)+
  labs(y = "Série Temporal Simulada", x = "Tempo")+theme_minimal()
ggplot(df.jj, aes(x=t, y=y)) +
  geom_line() +
  stat_smooth(method='nls', formula='y~a*(1+x)^b', 
              method.args=list(start =list(a=1, b=1)), se=FALSE)+
  labs(y = "Série Temporal Simulada - Ajuste Função Não Linear", x = "Tempo")+theme_minimal()

#-------------------------------------------------------------------------------
#Série Temporal - Ajuste Função Não Linear
#-------------------------------------------------------------------------------
t<-1:length(y)
mod<-nls(y~a*(1+t)^b,start=list(a=1,b=1))
summary(mod)
confint(mod,level = 0.95)
z<-coefficients(mod)[[1]]*(1+t)^coefficients(mod)[[2]]
d.y<-y-z
autoplot(d.y)+
  labs(y = "Série Temporal Simulada - Sem Tendência", x = "Tempo")+theme_minimal()
ggAcf(d.y, lag.max=20)+labs(y = "FAC Série sem Tendência",title="")+theme_minimal()

plot.ts(cbind(y,z),plot.type = c("single"),col=c("black","blue"),
       main = "Y",xlab="Tempo",ylab="")
#-------------------------------------------------------------------------------
#Análise de Resíduos
#-------------------------------------------------------------------------------
ggqqplot(d.y)
shapiro.test(d.y)
#-------------------------------------------------------------------------------
ggtsdisplay(d.y,plot.type="scatter", theme=theme_bw())
ggtsdisplay(d.y,plot.type="histogram", theme=theme_bw())
#-------------------------------------------------------------------------------
#Dados Não Correlacionados
#-------------------------------------------------------------------------------
Box.test(d.y, lag = 10, type = c("Box-Pierce"))
Box.test(d.y, lag = 10, type = c("Ljung-Box"))

hist(d.y,probability=TRUE)
lines(density(d.y),col="red")

#-------------------------------------------------------------------------------
#Série Temporal - Ajuste Função Não Linear
#-------------------------------------------------------------------------------
dataset <- data.frame(Exp = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                              5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
                      t = c(0, 0.33, 0.67, 1, 1.33, 1.67, 2, 4, 
                            6, 8, 10, 0, 33, 0.67, 1, 1.33, 1.67,
                            2, 4, 6, 8, 10, 0, 0.33, 0.67, 1, 1.33,
                            1.67, 2, 4, 6, 8, 10), fold = 
                        c(1, 0.957066345654286, 1.24139015724819,
                          1.62889151698633, 1.72008539595879, 
                          1.82725412314402, 1.93164365299958, 
                          1.9722929538061, 2.15842019312484, 
                          1.9200507796933, 1.95804730344453, 1,
                          0.836176542548747, 1.07077717914707, 
                          1.45471712491441, 1.61069357875771, 
                          1.75576377806756, 1.89280913889538, 
                          2.00219054189937, 1.87795513639311, 
                          1.85242493827193, 1.7409346372629, 1, 
                          0.840498729335292, 0.904130905000499, 
                          1.23116185602517, 1.41897551928886, 
                          1.60167656534099, 1.72389226836308, 
                          1.80635095956481, 1.76640786872057, 
                          1.74327897001172, 1.63581509884482))
#-------------------------------------------------------------------------------
df<-subset(dataset,Exp==4)#Subconjunto
ggplot(df,aes(x=t, y=fold))+ 
  #to make it obvious I use argument names instead of positional matching
  geom_line()+geom_point()+theme_minimal()
ggplot(df,aes(x=t, y=fold))+ 
  geom_point()+
  geom_smooth(method="nls", 
              formula=y~1+theta*(1-exp(-x/tau)), 
              method.args=list(start=c(tau=0.2,theta=2)),
              se=FALSE) +theme_minimal()
#-------------------------------------------------------------------------------
mod<-nls(fold~1+theta*(1-exp(-t/tau)),
         start=list(theta=2,tau=0.2),data=df)
summary(mod)
confint(mod,level = 0.95)
z<-1+coefficients(mod)[[1]]*(1-exp(-df$t/coefficients(mod)[[2]]))
d.y<-data.frame(t=df$t,data=df$fold-z)
ggplot(d.y,aes(x=t, y=data))+ 
  geom_line()+
  labs(y = "Série Temporal - Sem Tendência", x = "Tempo")+theme_minimal()
plot.ts(cbind(df$fold,z),plot.type = c("single"),col=c("red","blue"))
#-------------------------------------------------------------------------------
#Análise de Resíduos
#-------------------------------------------------------------------------------
d.y<-d.y$data
ggqqplot(d.y)
shapiro.test(d.y)
plot(density(d.y),col="red")
#-------------------------------------------------------------------------------

ggtsdisplay(d.y,plot.type="scatter", theme=theme_bw())

ggtsdisplay(d.y,plot.type="histogram", theme=theme_bw())
#-------------------------------------------------------------------------------
#Dados Não Correlacionados
#-------------------------------------------------------------------------------
Box.test(d.y, lag = 10, type = c("Box-Pierce"))
Box.test(d.y, lag = 10, type = c("Ljung-Box"))

#-------------------------------------------------------------------------------
#Testes de Tendência Deteminística
#-------------------------------------------------------------------------------
#Teste de Cox-Stuart, Wald-Wolfowitz e Mann-Kendall
#-------------------------------------------------------------------------------
# Sweet potato yield per acre, harvested in the United States, between 1868 and 1937.
# Data available in this package.
data(sweetpotato)
sp<-ts(sweetpotato$yield,start=c(1868),frequency = 1)
autoplot(sp)+labs(y = "Produção de Batata Doce por Acre", x = "Tempo")+theme_minimal()
#-------------------------------------------------------------------------------
cox.stuart.test(sp,c("two.sided"))
runs.test(sp)
#-------------------------------------------------------------------------------
mk.test(sp,continuity = TRUE)
mk.test(sp,continuity = FALSE)
-------------------------------------------------------------------------------
MannKendall(sp)
#-------------------------------------------------------------------------------
#Teste de Cox-Stuart, Wald-Wolfowitz e Mann-Kendall
#-------------------------------------------------------------------------------
IBOVESPA<-c(56176,55706,55582,56258,57504,57898,58153,58460,
            58448,59082,60088,60570,60432,60575,61467,61157,
            60020,60207,61089,61152,61523,60772)
IBOVESPA<-ts(IBOVESPA)
autoplot(IBOVESPA)+labs(x = "Dias Úteis", y = " Índices do IBOVESPA ")+theme_minimal()
#----------------------
cox.stuart.test(IBOVESPA)
cs.test(IBOVESPA)
#----------------------
runs.test(IBOVESPA)
ww.test(IBOVESPA)
#----------------------
mk.test(IBOVESPA)
MannKendall(IBOVESPA)
#-------------------------------------------------------------------------------
#Teste de Cox-Stuart, Wald-Wolfowitz e Mann-Kendall
#-------------------------------------------------------------------------------
petrobras<-read_excel("petrobras.xlsx",col_types=c("date","numeric"))
ggplot(petrobras,aes(x=Data, y=Dados))+ 
  geom_line()+
  labs(y = "Valores diários de fechamento das ações da Petrobras", x = "Tempo")+theme_minimal()
#----------------------
cox.stuart.test(petrobras$Dados)
cs.test(petrobras$Dados)
#----------------------
runs.test(petrobras$Dados)
ww.test(petrobras$Dados)
#----------------------
mk.test(petrobras$Dados)
MannKendall(petrobras$Dados)

#-------------------------------------------------------------------------------
#Testes de Tendência Estocástica - Raiz Unitária
#-------------------------------------------------------------------------------
#Teste de ADF, PP e KPSS 
#-------------------------------------------------------------------------------
#?ndice Mensal de Atividade Economica do Banco Central - IBC-Br
#-------------------------------------------------------------------------------
ibc_br <- read_excel("ibc-br.xlsx",col_names = TRUE)
fig = plot_ly(ibc_br, x = ~data, y = ~valor, type = 'scatter', mode = 'lines')
fig <- fig %>% layout(title = "Índice de Atividade Econômica do Banco Central - IBC-Br")
fig
ggAcf(ibc_br$valor,lag.max=100,type=c("correlation"))+labs(y="FAC Amostral",title="IBC-Br")+theme_minimal()
ibc_br <- ts(ibc_br$valor, start=c(2003,1), frequency = 12)
#-------------------------------------------------------------------------------
cs.test(ibc_br)
mk.test(ibc_br)
ww.test(ibc_br)
#----------------------
adf.test(ibc_br,alternative = c("stationary"))
pp.test(ibc_br,alternative = c("stationary"))
kpss.test(ibc_br, null = c("Level"))
kpss.test(ibc_br, null = c("Trend"))
#----------------------
diff.ibc_br<-diff(ibc_br, differences = 1)
autoplot(diff.ibc_br,ylab = "")+labs(title= "Diff. IBC-Br", x = "Tempo")+theme_minimal()
ggAcf(diff.ibc_br,lag.max=100,type=c("correlation"))+labs(y="FAC Amostral",title="Diff. IBC-Br")+theme_minimal()
#----------------------
cs.test(diff.ibc_br)
mk.test(diff.ibc_br)
ww.test(diff.ibc_br)
#----------------------
adf.test(diff.ibc_br,alternative = c("stationary"))
pp.test(diff.ibc_br,alternative = c("stationary"))
kpss.test(diff.ibc_br, null = c("Level"))
kpss.test(diff.ibc_br, null = c("Trend"))

#----------------------------------------------------------
# Example
#Data from example in Brownlee (1965), p. 223.
## Results of 23 determinations, ordered in time, of the density of the earth.
earthden<-c(5.36, 5.29, 5.58, 5.65, 5.57, 5.53, 5.62, 5.29, 5.44, 5.34, 5.79, 
            5.10, 5.27, 5.39, 5.42, 5.47, 5.63, 5.34, 5.46, 5.30, 5.75, 5.68, 5.85)
earthden<-ts(earthden)
autoplot(earthden)+labs(y = "Valores densidade da Terra", x = "Tempo")+theme_minimal()
ggAcf(earthden)
#----------------------
cox.stuart.test(earthden)
cs.test(earthden)
#----------------------
runs.test(earthden)
ww.test(earthden)
#----------------------
mk.test(earthden)
MannKendall(earthden)
#-------------------------------------------------------------------------------
adf.test(earthden,alternative = c("stationary"))
pp.test(earthden,alternative = c("stationary"))#<-Não ? bom resultado
diff.earthden<-diff(earthden, differences = 1)
autoplot(diff.earthden,ylab = "")+labs(title= "Diff. Earthden", x = "Tempo")+theme_minimal()
ggAcf(diff.earthden)
adf.test(diff.earthden,alternative = c("stationary"))
pp.test(diff.earthden,alternative = c("stationary"))
kpss.test(earthden,null = c("Level"))
#-------------------------------------------------------------------------------
#Regressção Local (LOESS)
#-------------------------------------------------------------------------------
#Dados de economia de combustível de 1999 a 2008 para 38 
#modelos populares de carros
#-------------------------------------------------------------------------------
attach(mpg)
ggplot(mpg, aes(displ, hwy)) +
  geom_point()+
  labs(y = "Rendimento na rodovia (milhas/galão)", x = "Cilindrada do motor (litros)")+theme_minimal()
#----------------------
#LOESS+geom_smooth
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method="loess",span = 0.2,formula=y~x)+#span=alpha
  labs(y = "Rendimento na rodovia (milhas/galão)", x = "Cilindrada do motor (litros)")+theme_minimal()
#----------------------
#LOESS+geom_smooth+facet_wrap
#displ+hwy separadas por tipo de tração
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method="loess",span = 0.95,formula=y~x) +
  facet_wrap(~drv)
#-------------------------------------------------------------------------------
#https://bit.ly/37Jqtee
#-------------------------------------------------------------------------------
mydata <-read.csv("Example-ND.csv", sep=",")#Colocar arquivo no Moodle
mydata<-data.frame(x=mydata$Year, y=mydata$EMD)
colnames(mydata)<-c("Year","EMD")
ggplot(mydata, aes(x=Year, y=EMD)) +
  geom_point()+
  labs(y = "EMD", x = "Ano")+theme_minimal()
#----------------------
#LOESS
y.loess<-loess(EMD~Year,span=0.50,data=mydata,
               control = loess.control(surface = "direct"))
#----------------------
#Cálculo Predição
tmp<-predict(y.loess,mydata$Year,se=T)
#----------------------
#Cálculo Intervalo de Confiança
li<-tmp$fit - qt(0.975,tmp$df)*tmp$se.fit
ls<-tmp$fit + qt(0.975,tmp$df)*tmp$se.fit
#-------------------------------------------------------------------------------
#Gráfico (1)
y.predict<-data.frame(Year=mydata$Year,EMD=mydata$EMD,fit=tmp$fit,li=li,ls=ls)
ggplot(y.predict, aes(Year, EMD)) +
  geom_point()+
  geom_line(aes(Year, fit),colour = 'red')+
  geom_line(aes(Year, li),colour = 'blue')+
  geom_line(aes(Year, ls),colour = 'blue')+
     labs(y = "EMD", x = "Ano") +theme_minimal()
#-------------------------------------------------------------------------------
#Gráfico (2)
df1<-data.frame(Legenda=rep("Dados",length(mydata$Year)),Data=mydata$Year,Dados=mydata$EMD)
df2<-data.frame(Legenda=rep("Fitted",length(mydata$Year)),Data=mydata$Year,Dados=tmp$fit)
df3<-data.frame(Legenda=rep("LI",length(mydata$Year)),Data=mydata$Year,Dados=li)
df4<-data.frame(Legenda=rep("LS",length(mydata$Year)),Data=mydata$Year,Dados=ls)
y.predict<-data.frame(rbind(df1,df2,df3,df4))
ggplot(y.predict, aes(Data, Dados,col=Legenda)) +
  geom_point(size = 0.5)+
  scale_color_manual(values = c("Dados" = "black", "Fitted" = "red","LI"="blue3","LS"="blue3"))+
  labs(y = "EMD", x = "Ano",title="Predição-Loess")+theme_minimal()
#----------------------
#Cálculo Previsão
a<-rep(1/12,12)
ano<-c(rep(2005,9)+cumsum(a)[4:12],rep(2006,12)+cumsum(a))
tmp2<-predict(y.loess,data.frame(Year=c(mydata$Year,ano)),se=T) 
#----------------------
#Cálculo Intervalo de Confiança
li<-tmp2$fit - qt(0.975,tmp2$df)*tmp2$se.fit
ls<-tmp2$fit + qt(0.975,tmp2$df)*tmp2$se.fit
#----------------------
#Gráfico (3)
Year<-c(mydata$Year,ano)
df1<-data.frame(Legenda=rep("Dados",length(mydata$Year)),Data=mydata$Year,Dados=mydata$EMD)
df2<-data.frame(Legenda=rep("Fitted",length(c(Year))),Data=c(Year),Dados=tmp2$fit)
df3<-data.frame(Legenda=rep("LI",length(c(Year))),Data=c(Year),Dados=li)
df4<-data.frame(Legenda=rep("LS",length(c(Year))),Data=c(Year),Dados=ls)
y.predict<-data.frame(rbind(df1,df2,df3,df4))
ggplot(y.predict, aes(Data, Dados,col=Legenda)) +
  geom_point(size = 1)+
  scale_color_manual(values = c("Dados" = "black", "Fitted" = "red","LI"="blue3","LS"="blue3"))+
  labs(y = "EMD", x = "Ano",title="Predição - Previ??o - Loess")+theme_minimal()
#-------------------------------------------------------------------------------






