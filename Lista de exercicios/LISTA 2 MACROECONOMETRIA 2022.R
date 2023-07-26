#' ---
#' title: "LISTA-2 DE MACROECONOMIA APLICADA (Prof. Marcel Ribeiro)"
#' author: "João Luis Tenreiro Barroso"
#' date: "16/12/2022"
#' output: html_document
#' ---


#####################################################################################################
#   ABERTURA
######################################################################################################
rm(list = ls())



library(vars) # package para estimar modelos VAR
library(readxl)
library(ggplot2)
library(dplyr)



#########################################################

########################################################################################################

# Questão 1. VAR Projeção 
#Colete dados mensais da taxa anual de  #inflação anual medida pelo IPCA, taxa nominal de juros anualizada (SELIC) e
#IBC-BR para o Brasil. Construa a base de dados mais longa que conseguir.


#####################################################################################################
# Preparar os dados
######################################################################################################


Dados <- read_excel("./Dados.xlsx", 
                    col_types = c("numeric", "numeric", "date", 
                                  "numeric", "numeric", "numeric"))
Dados

Dados$Data <- as.Date(Dados$Data)

plot(Dados$IPCA, type="l")
plot(Dados$SELIC, type="l")
plot(Dados$IBCBR, type="l")

# Fazendo a 1a. diferenca e sobreescrevendo a coluna do IPCA e do IBCBR
Dados <- Dados %>% mutate(IPCA = (log(IPCA)-lag(log(IPCA)))*100,
                          IBCBR =( log(IBCBR)-lag(log(IBCBR)))*100)

Dados <- na.omit(Dados)

plot(Dados$IPCA, type="l")
plot(Dados$SELIC, type="l")
plot(Dados$IBCBR, type="l")

range(Dados$Data)

# Definindo as variáveis como séries de tempo mensais
IPCA  = ts(Dados$IPCA,  start=c(2003,2), frequency = 12)
SELIC = ts(Dados$SELIC, start=c(2003,2), frequency = 12)
IBCBR = ts(Dados$IBCBR, start=c(2003,2), frequency = 12)

# Vetor de dados 
x = cbind(IBCBR, IPCA, SELIC)
x



######################################################################################################
####  Q1 - (a) Mostre a evolução das variáveis coletadas ao longo do tempo em uma única figura
plot(x)


######################################################################################################


######################################################################################################
####  Q1 - (b)Escolha a defasagem do VAR com base nos critérios de informação e análise dos resíduos
######################################################################################################

acf(x)
pacf(x)


# O comando VARselect nos informa o critério de informação com $p_{max}$ definido por lag.max.
vars::VARselect(x, type = c("const"), exogen = NULL, lag.max=12)


# Vamos estimar o VAR(12) sugerido pelos critérios AIC, HQ e FPE. O summary() do modelo mostra os coeficientes em formato de regressão
var = VAR(x, type = c("const"), p=12)
summary(var)



## Diagnósticos dos resíduos

#O package é tal que a função plot() "plota" a série, os resíduos e o ACF e PACF dos resíduos 
#Este último é uma avaliação visual  de confirmação  para os critérios de seleção).
plot(var)

# Com base nos testes de autocorrelação dos resíduos. estes parecem bem comportados, com exceção do IBC-BR com algum resíduo no lag 12
  

for (j in c(3, 6, 9, 12)) {
  cat("Sequencia de testes para defasagem:",j,"\n")
  print(serial.test(var, lags.pt = j, type ="PT.asymptotic"))
  print(serial.test(var, lags.pt = j, type ="PT.adjusted"))
  print(serial.test(var, lags.bg = j, type="BG"))
  print(serial.test(var, lags.bg = j, type ="ES")) 
  }

# Ho: Não tem autocorrelação dos resíduos 
# Problema nas defasagens 6 e , principalmente, 12, pois rejeitam H0. Pode ser devido a sazonalidad das séries.

######################################################################################################
####  Q1 - (c) Com base na defasagem encontrada, faça os testes de causalidade de Granger e interprete.
########################################################################################


# Teste de causalidade de Granger VAR(2) com dummy sazonal 
# Versão com erro padrão tradicional

GrangerIBCBR <- causality(var, cause = "IBCBR")
GrangerIBCBR  
# A 5% e 1% não rejeito H0: IBC-BR não causa no sentido de  Granger o IPCA ou SELIC.

GrangerIPCA <- causality(var, cause = "IPCA")
GrangerIPCA  
# A 10% e 5% de confiança, rejeito H0. IPCA causa no sentido de Granger o IBCBR ou SELIC . 
# Inflação provoca subida da atividade econômica e reação do BACEN subindo juros.

GrangerSELIC <- causality(var, cause = "SELIC")
GrangerSELIC 
# A 10% e 5%, rejeito H0. SELIC causa no sentido de Granger o IBCBR ou IPCA a 10% e 5%. 
# Selic afeta atividade econômica e inflação.

#########################################################################################################
## (d) Gere projeções e o intervalo de confiança para as variáveis consideradas até o final de 2023
########################################################################################################


prediction <- predict(var, n.ahead=15)

plot(prediction)

prediction$fcst$IBCBR
prediction$fcst$IPCA
prediction$fcst$Selic






######################################################################################################
####  Questão 2 SVAR Identificação recursiva
# (a)  Compute as funções IRF com base em uma identificação recursiva de ordem IBC-BR, IPCA, SELIC. 
#Plote os três choques e discuta os resultados.
######################################################################################################


IBCBR_irf <- irf(var , impulse =  "IBCBR", response = c("IPCA", "SELIC", "IBCBR"), n.ahead=24)
plot(IBCBR_irf)

# Conclusão: Um choque de IBC-BR, que se dilui em 2 meses, não tem efeito no IPCA. tendo um efeito de subida ligeira da SELIC durante o primeiro ano

IPCA_irf <- irf(var , impulse =  "IPCA", response = c("IPCA", "SELIC", "IBCBR"), n.ahead=24)
plot(IPCA_irf)

# Conclusão: Um choque de IPCA que se dilui em 6 meses, tem efeito de provocar uma subida da SELIC pelo BACEN no período de dois anos, mas sem impacto no nível de atividade

SELIC_irf <- irf(var , impulse =  "SELIC", response = c("IPCA", "SELIC", "IBCBR"), n.ahead=24)
plot(SELIC_irf)

# Conclusão: Um choque de SELIC, que vai crescendo ao longo de um ano , e se dilui em dois, não tem efeito significante no IPCA e no IBC-Br
########################################################################################################
## Q2 (b) Compute a decomposição da variância do erro de previsão para os
## horizontes h = 1; 12; 24; 60 para cada variável. Interprete os resultados
#######################################################################################################

plot(fevd(var, n.ahead=1))

plot(fevd(var, n.ahead=12))

plot(fevd(var, n.ahead=24))

plot(fevd(var, n.ahead=60))

# Conclusão: Para o choque na atividade econômica (IBC-BR) e para o IPCA a quase totalidade da variância do
# erro vem dos próprios indicadores.
# No entanto, para a taxa de juros SELIC, a variância do erro de previsão vem do próprio IPCA e , em menor grau, da SELIC, com contribuição quase nula do IBC-BR

######################################################################################################
####  Q2  (c) Troque a ordem da especificação colocando o IPCA em primeiro e IBCBR em
## segundo. Os resultados do choque monetário se alteram? E dos demais choques?
## Explique
######################################################################################################


# Trocando ordem das variaveis
x2 = cbind(IPCA, IBCBR, SELIC)

# Restimando o VAR
var2 = VAR(x2, type = c("const"), p=12)

IBCBR_irf <- irf(var2, impulse =  "IBCBR", response = c("IPCA", "SELIC", "IBCBR"), n.ahead=24)
plot(IBCBR_irf)

IPCA_irf <- irf(var2, impulse =  "IPCA", response = c("IPCA", "SELIC", "IBCBR"), n.ahead=24)
plot(IPCA_irf)

SELIC_irf <- irf(var2, impulse =  "SELIC", response = c("IPCA", "SELIC", "IBCBR"), n.ahead=24)
plot(SELIC_irf)



# Conclusão: Os resultados das funções IRF não se alteram em relação ao item anterior. Tal resultado se deve 
#ao fato de a SELIC ter permanecido em terceiro lugar na identificação e os choques serem ortogonais.


######################################################################################################
####  Questão 3 (a) SVAR: identificado via restrição de sinais
######################################################################################################


source("./PS3/uhlig_reject.R")

##################Q3  a) Choque monetário

# vetor com o nome das respostas
v1 = c("Response of IBCBR" ,
       "Response of IPCA",
       "Response of SELIC")

restricoes <- c(+3, -2)

svar_sig_1 <- uhlig.reject(Y = x,
                           nlags = 12,
                           draws = 500,
                           subdraws = 500,
                           nkeep = 1000, 
                           KMIN = 1,
                           KMAX = 3,
                           constrained = restricoes, 
                           constant = TRUE, 
                           steps = 20)

# plota o resultado
irfplot(irfdraws = svar_sig_1$IRFS, 
        type = "median",
        labels = v1,
        save = TRUE,
        bands = c(0.16, 0.84),
        grid = TRUE,
        bw = FALSE, 
        fileName = "Selic_up e IPCA_dw.eps")

# Choque monetário tem efeito sobre IBC-BR, IPCA e SELIC

###################### Q3 b)  Choque na demanda -------------------------------------------------------

restricoes <- c(+1, +2)

svar_sig_2 <- uhlig.reject(Y = x,
                           nlags = 12,
                           draws = 500,
                           subdraws = 500,
                           nkeep = 1000, 
                           KMIN = 1,
                           KMAX = 3,
                           constrained = restricoes, 
                           constant = TRUE, 
                           steps = 20)

# plota o resultado
irfplot(irfdraws = svar_sig_2$IRFS, 
        type = "median",
        labels = v1,
        save = TRUE,
        bands = c(0.16, 0.84),
        grid = TRUE,
        bw = FALSE, 
        fileName = "IBCBR_up e IPCA_up.eps")

# Choque de demanda tem efeito sobre IBC-BR, IPCA e SELIC

################### Q3 c) Choque de oferta negativo -------------------------------------------------------

restricoes <- c(-1, +2)

svar_sig_2 <- uhlig.reject(Y = x,
                           nlags = 12,
                           draws = 5000,
                           subdraws = 500,
                           nkeep = 1000, 
                           KMIN = 1,
                           KMAX = 3,
                           constrained = restricoes, 
                           constant = TRUE, 
                           steps = 20)

# plota o resultado
irfplot(irfdraws = svar_sig_2$IRFS, 
        type = "median",
        labels = v1,
        save = TRUE,
        bands = c(0.16, 0.84),
        grid = TRUE,
        bw = FALSE, 
        fileName = "IBCBR_dw e IPCA_up.eps")

# Choque de oferta negativo (" cost-push shock") tem efeito sobre IBC-BR, IPCA e SELIC
