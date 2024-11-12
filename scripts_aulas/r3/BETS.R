> # Busca em português pela série de produção de bens intermediários
  > results = BETS.search(description = "'bens intermediarios'", lang = "pt", view = F)
  > results
  
  ##    code
  ## 1  1334
  ## 2 11068
  ## 3 21864
  ## 4 25302
  ## 5 25328
  ##                                                                       description
  ## 1 Indicadores da produção (1991=100) - Por categoria de uso - Bens intermediários
  ## 2 Indicadores da produção (2002=100) - Por categoria de uso - Bens intermediários
  ## 3                        Indicadores da produção (2012=100) - Bens intermediários
  ## 4                                        Importações - Bens intermediários (CGCE)
  ## 5                                   Importações (kg) - Bens intermediários (CGCE)
  ##     unit periodicity      start     source
  ## 1 Índice           M 31/01/1975       IBGE
  ## 2 Índice           M 31/01/1991       IBGE
  ## 3 Índice           M 01/01/2002       IBGE
  ## 4    US$           M 01/01/1997 MDIC/Secex
  ## 5     kg           M 01/01/1997 MDIC/Secex
  
  #-----------------------------------------------------------------------------
  results = BETS.search(description = "consumption ~ 'seasonally adjusted' ~ private", view = F)
  ##   code                                           description
  ## 1 1393          Petroleum derivatives consumption - Gasoline
  ## 2 1394               Petroleum derivatives consumption - GLP
  ## 3 1395          Petroleum derivatives consumption - Fuel oil
  ## 4 1396        Petroleum derivatives consumption - Diesel oil
  ## 5 1397 Petroleum derivatives consumption - Other derivatives
  ## 6 1398 Petroleum derivatives consumption - Total derivatives
  ##                     unit periodicity      start last_value source
  ## 1 Barrels/day (thousand)           M 31/01/1979   apr/2017    ANP
  ## 2 Barrels/day (thousand)           M 31/01/1979   apr/2017    ANP
  ## 3 Barrels/day (thousand)           M 31/01/1979   apr/2017    ANP
  ## 4 Barrels/day (thousand)           M 31/01/1979   apr/2017    ANP
  ## 5 Barrels/day (thousand)           M 31/01/1979   apr/2017    ANP
  ## 6 Barrels/day (thousand)           M 31/01/1979   apr/2017    ANP
  
  
  #-----------------------------------------------------------------------------
  BETS.search(description = "gdp accumulated", unit = "US", view = F)
  ##   code                                            description
  ## 1 4192 GDP accumulated in the last 12 months - in US$ million
  ## 2 4386           GDP accumulated in the year - in US$ million
  ##            unit periodicity      start last_value    source
  ## 1 US$ (million)           M 31/01/1990   may/2017 BCB-Depec
  ## 2 US$ (million)           M 31/01/1990   may/2017 BCB-Depec
  
  
  
  #------------------------------------------------------------------
  #Análise da Serie temporal de Producao de Bens Intermediarios.
  #------------------------------------------------------------------
  #https://rpubs.com/modelthinkingbr/bets
  data <- BETS::BETSget(21864)

  result<-BETS::BETSsearch(description="ipca")
  ipca <- BETS::BETSget(13522)
  plot.ts(window(data,start=c(2000,1)))
  
  Gasoline <- BETS::BETSget(1393)
  ggplot2::ggplot(Gasoline,aes(x=date,y=value))+geom_line()+theme_minimal()
  
  Diesel <- BETS::BETSget(1396)
  ggplot2::ggplot(Diesel,aes(x=date,y=value))+geom_line()+theme_minimal()
  
  result<-BETS::BETSsearch(description = "gdp accumulated", unit = "US", view = F)
  result
  gdp_accum = BETS::BETSget(4192)
  plot.ts(gdp_accum)
  window(gdp_accum, start = c(2014,1))
  
  result<-BETSsearch(description="bens",lang="pt")
  result<-BETSsearch(description="*",lang="en")
  
  
  results = BETSsearch(description = "consumption ~ 'seasonally adjusted' ~ private", view = F)
  