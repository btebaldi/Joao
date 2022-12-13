# limpeza
rm(list = ls())

# Bibliotecas utilizadas
library(readxl)
library(ggplot2)
library(dplyr)
library(seasonal)
library(urca)
library(vars)
library(TSA)

# 1) Buscar e preparar o banco de dados
Dados.full <- read_excel("Trabalho/Dados.xlsx", 
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))
Dados.full$Data <-  as.Date(Dados.full$Data)

Dados.full


# Data regularization -----------------------------------------------------


# Vamos trabalhar com as variaveis: T, G, PIB, Selic, IPCA


ipca_ref <- Dados.full$IPCA[Dados.full$Data == "2022-10-01"]

#  construindo as vaiaveis em termos reais
Dados <- Dados.full %>% 
  dplyr::select(Data, T, G, PIB, IPCA, Selic) %>% 
  mutate(fator = 1,
         t = log(T/fator),
         g = log(G/fator),
         pib = log(PIB/fator),
         inflacao = (log(IPCA) - lag(log(IPCA), n = 12))*10,
         selic = Selic*100)

# Selecioando a amostra
Dados <- Dados %>% 
  dplyr::filter(Data >= "1998-12-01") %>% 
  dplyr::select(data=Data, t, g, pib, inflacao, selic)

# estatisticas descritivas
summary(Dados)


# graficos das series
for(col in colnames(Dados)){
  
  if(col == "data"){
    next()
  }
  
  g1 <- Dados %>% dplyr::select(x=data, y=!!col) %>%
    ggplot() +
    geom_line(aes(x=x, y=y)) +
    labs(title = col)
  
  print(g1)
}

# analise grafica
# 1: grafico t - quebra estrutural em 2010
# 2: grafico g - quebra estrutural em 2015
# 3 : Inflacao - quebra em 2002-11 ate 2003-01
# 4 : Selic - quebra em 1999-01 ate 1999-03


# Script do Teo para testar raiz unitaria
# sa rotina so funciona com critical = "5pct" 
source("Trabalho/Testa_RaizUnitaria.R")
Dados %>% Testa.RaizUnitaria(N.lags = 12, InfoCriteria = "BIC", critical = "5pct") 

for(col in colnames(Dados)){
  
  if(col == "Data"){ next() }
  
  cat("---------------------------------------", "\n")  
  cat("Analise de raiz unitaria em:", col, "\n")
  df <- urca::ur.df(y = Dados[[col]], type = "trend", lags = 12, selectlags = "BIC")
  print(df)
  
  df <- urca::ur.df(y = Dados[[col]], type = "drift", lags = 12, selectlags = "BIC")
  print(df)
  
  df <- urca::ur.df(y = Dados[[col]], type = "none", lags = 12, selectlags = "BIC")
  print(df)
}


# Vamos estacionarizar as 
# Dados <- Dados %>%
#   mutate(d.t = t - lag(t),
#          d.g = g - lag(g), 
#          d.pib = pib - lag(pib)) %>% 
#   dplyr::select(data, d.t, d.g, d.pib, inflacao, selic) %>% 
#   na.omit()

# Verificando estacionariedade
Dados %>% Testa.RaizUnitaria(N.lags = 12, InfoCriteria = "BIC", critical = "5pct") 

Dados <- Dados %>% dplyr::filter(data>="1999-01-01")

# Contrucao das Dummies

# 1: grafico t - quebra estrutural em 2010
# 2: grafico g - quebra estrutural em 2015
# 3 : Inflacao - quebra em 2002-11 ate 2003-01
# 4 : Selic - quebra em 1999-01 ate 1999-03
Dummies <- Dados %>%
  dplyr::transmute(
    Dummy_1 = as.numeric(data >= as.Date("2010-01-01") & data <= as.Date("2010-10-01")),
    Dummy_2 = as.numeric(data >= as.Date("2015-01-01") & data <= as.Date("2015-10-01")),
    Dummy_3 = as.numeric(data >= as.Date("2002-11-01") & data <= as.Date("2003-01-01")),
    Dummy_4 = as.numeric(data >= as.Date("1999-01-01") & data <= as.Date("1999-03-01")),
    Pandemia = as.numeric(data >= as.Date("2020-03-01") & data <= as.Date("2021-03-01")) ) %>% 
  data.matrix() %>% 
  ts(start = c(1999, 01), frequency = 12)

# Transforma os dados em TS
Dados.ts <- Dados %>% 
  dplyr::select(-data) %>% 
  data.matrix() %>% 
  ts(start = c(1999, 01), frequency = 12)



# 2) Analise de sazonalidade (se necessario, caso contrario pulamos)
seasMdl <- seasonal::seas(x = Dados.ts)


#  busca as series filtradas
Dados.filtro <- seasonal::final(seasMdl)

plot(Dados.ts)
plot(Dados.filtro)

# 5) Modelo VAR + Analise de cointegracao
# 6) Analise de causalidade de granger
# 7) Analise de choque fiscal e choque monetario



# O comando VARselect nos informa o critério de informação com $p_{max}$
# definido por lag.max.
vars::VARselect(Dados.filtro,
                type = c("const"),
                exogen = Dummies,
                lag.max=24)


# Vamos estimar o VAR(4), usando o comando VAR conforme abaixo. O summary() do
# modelo mostra os coeficientes em formato de regressão
my_var = VAR(Dados.filtro,
             type = c("const"),
             # exogen = Dummies,
             p=6)

# testamos com varios lags no VAR e o primeiro que comeca a retirar o efeito de
# autocorrelacao 'e um lag 20. Como isso implica em uma perda consideravel de
# graus de liberdade, seguimos com a recomendacao de VAR(4) do AIC

summary(my_var)

# plot(my_var)

for(i in c(3,6,9,12)){
  # Fazendo teste para autocorrelação dos resíduos
  # serial.test(my_var, lags.pt=6, type="PT.asymptotic")  # H0 = não há autocorrelação PT assintótico
  # serial.test(my_var, lags.pt=6, type="PT.adjusted")    # teste Portmanteau ajustado
  # serial.test(my_var, lags.bg=6, type="BG")
  print(serial.test(my_var, lags.bg=i, type="ES"))
}

# acf(my_var$varresult$t$residuals)
# pacf(my_var$varresult$t$residuals)
# 
#  acf(my_var$varresult$dg$residuals)
# pacf(my_var$varresult$dg$residuals)
# 
# acf(my_var$varresult$dpib$residuals)
# pacf(my_var$varresult$dpib$residuals)
# 
# acf(my_var$varresult$inflacao$residuals)
# pacf(my_var$varresult$inflacao$residuals)
# 
# acf(my_var$varresult$selic$residuals)
# pacf(my_var$varresult$selic$residuals)



# O teste rejeita HO, confirmando que há autocorrelação dos resíduos


# Função Impulso Resposta IRF
# dt, dg, dpib, inflacao, selic
my_irf <- vars::irf(x = my_var, 
              exogen = matrix(data = 0, ncol = 5, nrow = 60),
              impulse = c("t", "g", "selic",  "inflacao"),
              cumulative = TRUE,
              n.ahead = 60)



plot(my_irf)


source(file = "uhlig_reject.R")

# t         g      pib inflacao     selic
restricoes <- c(+2, +3)

# vetor com o nomde das respostas
v1 = c("Response of t" ,
       "Response of g",
       "Response of pib",
       "Response of pi",
       "Response of bf")

svar_sig_1 <- uhlig.reject(Y = Dados.filtro,
                           nlags = 4,
                           draws = 500,
                           subdraws = 200,
                           nkeep = 1000, 
                           KMIN = 1,
                           KMAX = 6,
                           constrained = restricoes, 
                           constant = TRUE, 
                           steps = 20)


irfplot(irfdraws = svar_sig_1$IRFS, 
        type = "median",
        labels = v1,
        save = FALSE,
        bands = c(0.16, 0.84),
        grid = TRUE,
        bw = FALSE)










