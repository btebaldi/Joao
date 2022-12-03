# Macroeconometria Aplicada
# Marcel Ribeiro
# Vitor Caio ( Monitor)

# PS2 PF 26.11,22

rm(list=ls())

# library(sidrar) # baixa dados brasileiros diretamente da base do IBGE 
library(seasonal) # package para retirar sazonalidade
# library(forecast) # package para previsão e ACF/PACF
# library(dynlm) # package para OLS com defasagens
# library(aTSA) # Algumas funções úteis de séries de tempo
# library(lmtest) # Alguns testes de hipóteses úteis
library(readxl)
library(dplyr)
library(vars)

library(ggplot2)

# Determina o diretorio de trabalho 
setwd("C:/Users/João/Desktop/Doutorado Profissional/Macroeconometria")

######################################################################################################
####  EXERCÍCIO 1 - a) 
#Calcule as séries de gastos gy e de receitas ty como percentagem do PIB e retire a sazonalidade da série. 
#Procure pelo outliers mais claros e crie dummies para tentar controlar por essas observações. 
######################################################################################################


# Preparando os dados 
tbl1 <- read_excel(path = "./Problem set 2/dados_fiscal.xlsx")

# Ajustar coluna de data
tbl1$Data <- as.Date(tbl1$Data)

# Renomear as colunas
colnames(tbl1) <- c("Data", "Receita", "Despesa", "PIB",  "ty",  "gy")

tbl1

#  Mostra as estatisticas descritas das series
summary(tbl1)

# Transformando em TS
ty.ts = ts(data = tbl1$ty, start=c(1997,1), frequency = 12)
gy.ts = ts(data = tbl1$gy, start=c(1997,1), frequency = 12)

# Plota as séries 

plot(ty.ts)
plot(gy.ts)

#  ACF e PACF
acf(ty.ts, lag.max = 36)
pacf(ty.ts, lag.max = 36)

acf(gy.ts)
pacf(gy.ts)


# Filtro sazonal
ty.seas <- seasonal::seas(ty.ts)
gy.seas <- seasonal::seas(gy.ts)


# Mostra o ajuste sazonal em gy e ty
summary(ty.seas)
plot(ty.seas)

summary(gy.seas)
plot(gy.seas)


ty.final <- final(ty.seas) 
plot(ty.final)

gy.final <- final(gy.seas)
plot(gy.final)

boxplot(ty.final, gy.final)

# Criando Dummy de outliers

# AO1998.Aug         0.34405    0.06407   5.370 7.87e-08 ***
  #   AO2010.Sep         0.79584    0.06176  12.885  < 2e-16 ***
#   AO2013.Nov         0.35519    0.06177   5.750 8.91e-09 ***
#   AO2016.Oct         0.34207    0.06182   5.533 3.14e-08 ***
  #   AO2019.Dec         0.42555    0.06273   6.783 1.17e-11 ***
  #   AO2020.Apr        -0.29066    0.06292  -4.620 3.85e-06 ***
  #   AO2020.May        -0.44393    0.06295  -7.052 1.77e-12 ***
  #   AO2020.Jun        -0.34317    0.06426  -5.340 9.29e-08 ***
#   AO2022.Jun         0.34199    0.07145   4.786 1.70e-06 ***

  #   AO2010.Sep         0.564085   0.058444   9.652  < 2e-16 ***
#   AO2015.Dec         0.324461   0.058478   5.548 2.88e-08 ***
  #   AO2019.Dec         0.311003   0.058986   5.272 1.35e-07 ***
  #   AO2020.Apr         0.539597   0.060726   8.886  < 2e-16 ***
  #   AO2020.May         0.530314   0.061996   8.554  < 2e-16 ***
  #   AO2020.Jun         0.815124   0.065106  12.520  < 2e-16 ***
#   AO2020.Jul         0.293384   0.063924   4.590 4.44e-06 ***
#   AO2020.Aug         0.503834   0.062050   8.120 4.67e-16 ***
#   AO2020.Sep         0.346370   0.062142   5.574 2.49e-08 ***
#   AO2021.Jun         0.300429   0.063467   4.734 2.21e-06 ***
#   AO2022.Jul        -0.290507   0.070057  -4.147 3.37e-05 ***


D_01 <- as.numeric(tbl1$Data == as.Date('2010-09-01'))
D_02 <- as.numeric(tbl1$Data == as.Date('2019-12-01'))
D_03 <- as.numeric(tbl1$Data >= as.Date('2020-04-01') & tbl1$Data <= as.Date('2020-06-01'))


# Transformando em um data.frame

tbl.var <- data.frame(gy.final, ty.final, D_01, D_02, D_03)


# O comando VARselect nos informa o critério de informação com $p_{max}$ definido por lag.max.
vars::VARselect(tbl.var[ , c("gy.final", "ty.final")],
                 type = c("const"),
                 exogen = tbl.var[ , c("D_01", "D_02",  "D_03")], lag.max=13)



# A maioria dos critérios indica 12 defasagens para o VAR bivariado. 



# Vamos estimar o VAR(12) , usando o comando VAR conforme abaixo. O summary() do modelo mostra os coeficientes em formato de regressão
my_var = VAR(tbl.var[ , c(1,2)],
             type = c("const"),
             exogen = tbl.var[ , c(-1, -2)],
             p=12)

summary(my_var)var

plot(my_var)

# Fazendo teste para autocorrelação dos resíduos
serial.test (my_var, lags.pt=12, type="PT.asymptotic")  # H0 =  não há autocorrelação PT assintótico
serial.test (my_var, lags.pt=12, type="PT.adjusted")    # teste Portmanteau ajustado

# O teste rejeita HO, confirmando que há autocorrelação dos resíduos

# Função Impulso Resposta IRF

my_irf <- irf(x = my_var, 
    impulse = c("gy.final", "ty.final"),
    cumulative = TRUE,
    n.ahead = 60)



plot(my_irf)


