
# Setup -------------------------------------------------------------------

# limpeza de variaveis
rm(list = ls())

# Pacoates
library(readxl) # Carregar base de dados
library(dplyr) # Manipulacao de base de dados
library(vars) # Estimacao de VAR

if(!any(.packages(all.available = TRUE) == "mvnfast")){
  install.packages("mvnfast") # para sorteios de normal multivariada
}

library(ggplot2) # graficos
library(cowplot) # graficos



# User Defined Functions --------------------------------------------------

source("my_plot_varest.R")


# Data load ---------------------------------------------------------------

# faz a leitura do banco de dados
dados_monpol <- read_excel("dados_monpol.xlsx", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"))

#  ajusta coluna de data
dados_monpol$dates <- as.Date(dados_monpol$dates)


# Com exceção da taxa de juros, compute o log das variáveis e mostre a evolução
# das variáveis coletadas ao longo do tempo.


# Passa o log nas variaveis
dados_monpol2 <- dados_monpol %>% 
  mutate(rgdp = 100*log(rgdp), 
         pgdp = 100*log(pgdp), 
         pcomm = 100*log(pcomm), 
         # ff = log(ff), 
         totres = 100*log(totres), 
         nbres = 100*log(nbres), 
         rcons = 100*log(rcons), 
         rinv = 100*log(rinv) )

# transforma os dados em objeto de serie temporal
y.df <- dados_monpol2 %>% dplyr::select(rgdp, pgdp, pcomm, totres, nbres, rcons, rinv, ff)
y.ts <- ts(y.df, start = c(1965, 1), frequency = 4)

#  faz um grafico do nivel das variaveis
par(mfrow = c(3,3))
for(col in colnames(y.ts)){
  plot(y.ts[,col], main = col)
  abline(h=0)
}

par(mfrow = c(1,1))

#  ------------------------------- METODO 2 
#  faz um grafico do nivel das variaveis utilizando o ggplot
for(col in colnames(y.df)){
  g <- dados_monpol2 %>%
    dplyr::select(x = dates, y = !!col) %>% 
    ggplot() + 
    geom_line(aes(x=x, y=y)) +
    labs(title = col, y=NULL, x= NULL)
  
  print(g)
}



# Selecting lag order -----------------------------------------------------

# seleciona os dados em lag
VARselect(y = y.ts, lag.max = 8, type = "const")

# Estima os modelos utilizando os lags de AIC e BIC
var_aic <- VAR(y.ts, p = 2, type = "const")
var_bic <- VAR(y.ts, p = 1, type = "const")

# plota graficos de AIC e BIC
# plot(var_bic)
# plot(var_aic)


#  ------------------------------- METODO 2 

# seleciona os dados em lag
VARselect(y = y.df, lag.max = 8, type = "const")

# Estima os modelos utilizando os lags de AIC e BIC
var_aic <- VAR(y.df, p = 2, type = "const")
var_bic <- VAR(y.df, p = 1, type = "const")


# plota graficos de AIC e BIC
par(ask=F)
my_plot_varest(var_bic, ask=FALSE)
 # plot(var_aic)


# Variaveis exogenas ------------------------------------------------------

#  determina as dummies a serem utilizadas
# dummy1 <- as.numeric(dados_monpol2$dates >= "2001-03-01" & dados_monpol2$dates <= "2001-12-01")
# dummy2 <- as.numeric(dados_monpol2$dates >= "1985-03-01" & dados_monpol2$dates <= "1985-12-01")
# 
# #  junta as dummies em uma matriz
# data.exo <- cbind(dummy1, dummy2)
# 
# #  avalia o lag de selecao do 
# VARselect(y = y.ts,
#           lag.max = 15,
#           type = "const",
#           exogen = NULL)
# 
# 
# # Estima o var com dummies
# var_bic <- VAR(y = y.df,
#                lag.max = 15,
#                type = "const",
#                ic = c("SC"),
#                exogen = data.exo)
# 
# # Exibe o resultado da estimacao
# summary(var_bic)

# realiza os testes de estimacao do VAR 
for(j in c(2, 4, 8, 12)){
  cat("Testes para defasagem", j, "\n")
  print(serial.test(var_bic, lags.pt = j, type =  "PT.asymptotic"))
  print(serial.test(var_bic, lags.pt = j, type =  "PT.adjusted"))
  print(serial.test(var_bic, lags.bg = j, type =  "BG"))
  print(serial.test(var_bic, lags.bg = j, type =  "ES"))
}


# Script com as funcoes para teste de uhlig
source(file = "uhlig_reject.R")

restricoes <- c(+8, -2, -3, -5)

# vetor com o nomde das respostas
v1 = c("Response of rgdp" ,
       "Response of pgdp",
       "Response of pcomm",
       "Response of totres",
       "Response of nbres",
       "Response of rcons",
       "Response of rinv",
       "Response of ff" )

svar_sig_1 <- uhlig.reject(Y = y.ts,
                           nlags = 2,
                           draws = 500,
                           subdraws = 500,
                           nkeep = 1000, 
                           KMIN = 1,
                           KMAX = 2,
                           constrained = restricoes, 
                           constant = TRUE, 
                           steps = 20)

# plota o resultado
irfplot(irfdraws = svar_sig_1$IRFS, 
        type = "median",
        labels = v1,
        save = FALSE,
        bands = c(0.16, 0.84),
        grid = TRUE,
        bw = FALSE)

# impoe as novas restricoes
restricoes <- c(+8, -2, -3, -5, -6, -7)

svar_sig_2 <- uhlig.reject(Y = y.ts,
                           nlags = 2,
                           draws = 500,
                           subdraws = 500,
                           nkeep = 1000, 
                           KMIN = 1,
                           KMAX = 2,
                           constrained = restricoes, 
                           constant = TRUE, 
                           steps = 20)


irfplot(irfdraws = svar_sig_2$IRFS, 
        type = "median",
        labels = v1,
        save = TRUE,
        bands = c(0.16, 0.84),
        grid = TRUE,
        bw = FALSE)


