# 25/7/2023 SP
# limpeza das variaveis de ambiente
rm(list = ls())

# lista de bibliotecas utilizadas
library(readxl) 
library(dplyr)
library(magrittr)
library(ggplot2)
library(plm)
library(DescTools)

#  leitura da base de dados
tbl <- read_excel("C:/BACKUP/BACKUP_JOAO_BARROSO/USER/Desktop/Doutorado Profissional/Minha Tese  DPE ESG/TESE DPE/TabelaFinal 1406 corrigido.xlsx", sheet = "Sheet 1")

# cria funcao que tira a diferenca do log
d.ln <- function(x) { return(log(x)-lag(log(x)))}

tbl

# shortcut <- (alt -)
# filtra os dados com o ano atual
tbl <- tbl %>% filter(Ano != "Atual") %>% mutate(Ano = as.numeric(Ano))

tbl
colnames(tbl)

# ajusta nome das colunas.
# retira caracteres estranhos dos nomes das colunas
colnames(tbl) <- stringr::str_replace_all(string = colnames(tbl), pattern = "[ '%]", replacement = "_")
colnames(tbl) <- stringr::str_replace_all(string = colnames(tbl), pattern = "ç", replacement = "c")
colnames(tbl) <- stringr::str_replace_all(string = colnames(tbl), pattern = "ã", replacement = "a")
# Determinantes do ESG

colnames(tbl)

# passando o log nas colunas "Total_Assets", "Number_Of_Trades"
tbl <- tbl %>% mutate_at(.vars = c("Total_Assets", "Number_Of_Trades"), .funs = log)


# eliminar ESG ausentes no Y  ( Como fica isto mas regressões em que muda o Y?)
tbl <- tbl %>% filter(!(is.na(ESG_Disclosure_Score))) 

# REMOCAO DE OUTLIERS ( Winsorização) -----------------------------------------------------

colnames(tbl)

summary(tbl)

tbl$Trailing_12M_EBITDA_Margin = Winsorize(tbl$Trailing_12M_EBITDA_Margin,
                                           probs = c(0.01, 0.99),
                                           na.rm = TRUE)


tbl$Cap_Expend_to_Tot_Assets = Winsorize(tbl$Cap_Expend_to_Tot_Assets,
                                         probs = c(0.01, 0.99),
                                         na.rm = TRUE)

tbl$Return_on_Invested_Capital = Winsorize(tbl$Return_on_Invested_Capital,
                                           probs = c(0.01, 0.99),
                                           na.rm = TRUE)


tbl$Tobin_s_Q_Ratio = Winsorize(tbl$Tobin_s_Q_Ratio,
                                probs = c(0.01, 0.99),
                                na.rm = TRUE)

tbl$Normalized_Net_Income_Growth = Winsorize(tbl$Normalized_Net_Income_Growth,
                                             probs = c(0.01, 0.99),
                                             na.rm = TRUE)

tbl$EBIT = Winsorize(tbl$EBIT,
                     probs = c(0.01, 0.99),
                     na.rm = TRUE)

# Revenue_Growth_Year_over_Year 
tbl$Revenue_Growth_Year_over_Year = Winsorize(tbl$Revenue_Growth_Year_over_Year,
                                              probs = c(0.01, 0.99),
                                              na.rm = TRUE)

# Risk_Premium    
tbl$Risk_Premium = Winsorize(tbl$Risk_Premium,
                             probs = c(0.01, 0.99),
                             na.rm = TRUE)


# Revenue_Adjusted 
tbl$Revenue_Adjusted = Winsorize(tbl$Revenue_Adjusted,
                                 probs = c(0.01, 0.99),
                                 na.rm = TRUE)


# Applied_Beta_for_EQRP   
tbl$Beta_winsorized = Winsorize(tbl$Applied_Beta_for_EQRP,
                                probs = c(0.01, 0.99),
                                na.rm = TRUE)

#Total_Debt_to_Total_Assets
tbl$Total_Debt_to_Total_Assets = Winsorize(tbl$Total_Debt_to_Total_Assets,
                                probs = c(0.01, 0.99),
                                na.rm = TRUE)
## Volatility_360_Day
tbl$ Volatility_360_Day = Winsorize(tbl$ Volatility_360_Day,
                                           probs = c(0.01, 0.99),
                                           na.rm = TRUE)

summary(tbl)

# # remove as acoes de prio de anos anteriores a 2014
# tbl <- tbl %>% filter(!(Acao == "PRIO3 BS Equity" & Ano <= 2014))
# 
# # Cap. Expenditures / Total Assets = 155.6129 ( ECOR3 2004)
# tbl <- tbl %>% filter(!(Acao == "ECOR3 BS Equity" & Ano == 2004))
# 
# # Return on invested capital = -169.242 ( Gol4 2021)
# tbl <- tbl %>% filter(!(Acao == "GOLL4 BS Equity" & Ano == 2021))
# 
# # Tobin Q = 1624786 ( WEGE3 1994/1995)
# tbl <- tbl %>% filter(!(Acao == "WEGE3 BS Equity" & Ano == 1994))
# tbl <- tbl %>% filter(!(Acao == "WEGE3 BS Equity" & Ano == 1995))
# 
# # Normalized net income growth = -29724 ( VAA3 2009)
# tbl <- tbl %>% filter(!(Acao == "VIIA3 BS Equity" & Ano == 2009))
# 
# # EBIT = - 25665 ( Vale3 2015)  
# tbl <- tbl %>% filter(!(Acao == "VALE3 BS Equity" & Ano == 2015))



# Calculo do d.ln(last_price) ---------------------------------------------

# arrumo a tabela para ficar na ordem de acao e ano
tbl <- tbl %>%  arrange(Acao, Ano)

# inicializa a coluna com a info d.ln
tbl$d.ln_price = NA

for(i in 1:nrow(tbl)){
  # na primeira linha nao temos passado. Colocar NA
  if(i == 1) {
    tbl$d.ln_price[i] = NA
  } else {
    if(tbl$Acao[i] == tbl$Acao[i-1]){
      tbl$d.ln_price[i] = log(tbl$Last_Price[i])-log(tbl$Last_Price[i-1])
    } else{
      tbl$d.ln_price[i] = NA
    }
  }
}


# Determinando o lag das variaveis ----------------------------------------

# arrumo a tabela para ficar na ordem de acao e ano
tbl <- tbl %>%  arrange(Acao, Ano)

colnames(tbl)

# inicializa a coluna com a info d.ln
tbl[ , "LAG_Trailing_12M_EBITDA_Margin"] = NA
tbl[ , "LAG_Cap_Expend_to_Tot_Assets"] = NA
tbl[ , "LAG_Return_on_Invested_Capital"] = NA
tbl[ , "LAG_Tobin_s_Q_Ratio"] = NA
tbl[ , "LAG_Total_Assets"] = NA
tbl[ , "LAG_Total_Debt_to_Total_Assets"] = NA
tbl[ , "LAG_Normalized_Net_Income_Growth"] = NA
tbl[ , "LAG_Long_Term_Assets_as___Total_Assets"] = NA
tbl[ , "LAG_EBIT"] = NA
tbl[ , "LAG_Revenue_Growth_Year_over_Year"] = NA
tbl[ , "LAG_Revenue_Adjusted"] = NA
tbl[ , "LAG_Number_Of_Trades"] = NA
tbl[ , "LAG_Volatility_360_Day"] = NA
tbl[ , "LAG_Risk_Premium"] = NA
tbl[ , "LAG_Applied_Beta_for_EQRP"] = NA
tbl[ , "LAG_Last_Price"] = NA
tbl[ , "LAG_VWAP_(Standar_Deviation)"] = NA
tbl[ , "LAG_Beta_winsorized"] = NA

tbl[ , "LAG_ESG_Disclosure_Score"] = NA
tbl[ , "LAG_BESG_ESG_Score"] = NA

# ( e o pilares ESG e ESG Disclosure?)

for(i in 1:nrow(tbl)){
  # na primeira linha nao temos passado. Colocar NA
  if(i == 1) {
    next
  } else {
    if(tbl$Acao[i] == tbl$Acao[i-1]){
      
      tbl[i, "LAG_Trailing_12M_EBITDA_Margin"] = tbl[i-1, "Trailing_12M_EBITDA_Margin"]
      tbl[i, "LAG_Cap_Expend_to_Tot_Assets"] = tbl[i-1, "Cap_Expend_to_Tot_Assets"]
      tbl[i, "LAG_Return_on_Invested_Capital"] = tbl[i-1, "Return_on_Invested_Capital"]
      tbl[i, "LAG_Tobin_s_Q_Ratio"] = tbl[i-1, "Tobin_s_Q_Ratio"]
      tbl[i, "LAG_Total_Assets"] = tbl[i-1, "Total_Assets"]
      tbl[i, "LAG_Total_Debt_to_Total_Assets"] = tbl[i-1, "Total_Debt_to_Total_Assets"]
      tbl[i, "LAG_Normalized_Net_Income_Growth"] = tbl[i-1, "Normalized_Net_Income_Growth"]
      tbl[i, "LAG_Long_Term_Assets_as___Total_Assets"] = tbl[i-1, "Long_Term_Assets_as___Total_Assets"]
      tbl[i, "LAG_EBIT"] = tbl[i-1, "EBIT"]
      tbl[i, "LAG_Revenue_Growth_Year_over_Year"] = tbl[i-1, "Revenue_Growth_Year_over_Year"]
      tbl[i, "LAG_Revenue_Adjusted"] = tbl[i-1, "Revenue_Adjusted"]
      tbl[i, "LAG_Number_Of_Trades"] = tbl[i-1, "Number_Of_Trades"]
      tbl[i, "LAG_Volatility_360_Day"] = tbl[i-1, "Volatility_360_Day"]
      tbl[i, "LAG_Risk_Premium"] = tbl[i-1, "Risk_Premium"]
      tbl[i, "LAG_Applied_Beta_for_EQRP"] = tbl[i-1, "Applied_Beta_for_EQRP"]
      tbl[i, "LAG_Last_Price"] = tbl[i-1, "Last_Price"]
      tbl[i, "LAG_VWAP_(Standar_D-1eviation)"] = tbl[i, "VWAP_(Standar_Deviation)"]
      tbl[i, "LAG_Beta_winsorized"] = tbl[i-1, "Beta_winsorized"]
      
      tbl[i, "LAG_ESG_Disclosure_Score"] = tbl[i-1, "ESG_Disclosure_Score"]
      tbl[i, "LAG_BESG_ESG_Score"] = tbl[i-1, "BESG_ESG_Score"]
    } else{
      next
    }
  }
}
# ( e o pilares ESG e ESG Disclosure em lag?)

# Analise de integridade --------------------------------------------------

sort(unique(tbl$Ano))
sort(unique(tbl$Acao))

tbl %>%
  count(Acao, Ano) %>% 
  arrange(-n) %>% print(n=10)

########################################################################################
# Análise de determinantes de ESG_Score -----------------------------------------------

regressores <- "Trailing_12M_EBITDA_Margin + 
              Cap_Expend_to_Tot_Assets + 
              Return_on_Invested_Capital +
              Tobin_s_Q_Ratio +
              Total_Assets +
              Total_Debt_to_Total_Assets +
              Normalized_Net_Income_Growth +
              Revenue_Growth_Year_over_Year +
              Number_Of_Trades +
              Long_Term_Assets_as___Total_Assets +
              Volatility_360_Day" 


f <- formula(paste("BESG_ESG_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Empresas grandes e alavancadas, liquidas e  com boa margem ( a 5%) investem mais em ESG. Estranho que empresas com muitos ativos LP investem menos  

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)


phtest(fixed, random)


# # Analise de Environmental_Pillar_Score ------------------------------------------------
# 
# f <- formula(paste("BESG_Environmental_Pillar_Score ~", regressores))
# 
# # Efeitos fixos
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# 
# #phtest(fixed, random)
# 

# # Analise de Social_Pillar_Score ------------------------------------------------
# 
# f <- formula(paste("BESG_Social_Pillar_Score ~", regressores))
# 
# # Efeitos fixos
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #             data = tbl,
# #             index = c("Acao", "Ano"),
# #             model="random")
# #summary(random)
# 
# 
# #phtest(fixed, random)
# 
# # Analise de Governance_Pillar_Score ------------------------------------------------
# 
# f <- formula(paste("BESG_Governance_Pillar_Score ~", regressores))
# 
# # Efeitos fixos
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# 
# #phtest(fixed, random)
# 

#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
# Analise dos determinantes em lag  de ESG Score  ------------------------------------------

regressoresLAG <- "LAG_Trailing_12M_EBITDA_Margin + 
                LAG_Cap_Expend_to_Tot_Assets + 
                LAG_Return_on_Invested_Capital +
                LAG_Tobin_s_Q_Ratio +
                LAG_Total_Assets +
                LAG_Total_Debt_to_Total_Assets +
                LAG_Normalized_Net_Income_Growth +
                LAG_Revenue_Growth_Year_over_Year +
                LAG_Number_Of_Trades +
                LAG_Long_Term_Assets_as___Total_Assets +
                LAG_Volatility_360_Day" 


f <- formula(paste("BESG_ESG_Score ~", regressoresLAG))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Empresas grandes, alavancadas, valiosas e rentáveis investem mais em ESG ( Cap exp/Total assets menos...)

# Efeitos aleatórios

#random <- plm(formula = f,
#             data = tbl,
#             index = c("Acao", "Ano"),
#              model="random")
#summary(random)


#phtest(fixed, random)


# # LAG - Analise de BESG_Environmental_Pillar_Score ------------------------------------------------
# 
# f <- formula(paste("BESG_Environmental_Pillar_Score ~", regressoresLAG))
# 
# # Efeitos fixos
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# 
# #phtest(fixed, random)
# 
# 
# # LAG - Analise de BESG_Social_Pillar_Score ------------------------------------------------
# 
# f <- formula(paste("BESG_Social_Pillar_Score ~", regressoresLAG))
# 
# # Efeitos fixos
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #             data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# 
# #phtest(fixed, random)
# 
# # LAG - Analise de BESG_Governance_Pillar_Score ------------------------------------------------
# 
# f <- formula(paste("BESG_Governance_Pillar_Score ~", regressoresLAG))
# 
# # Efeitos fixos
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #             model="random")
# #summary(random)
# 
# 
# #phtest(fixed, random)


#############################################################################################
#############################################################################################
# Analise de determinantes de ESG Disclosure Score ------------------------------------------------------

f <- formula(paste("ESG_Disclosure_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Empresas grandes, mais rentáveis  e mais líquidas  investem mais em Disclosure ESG

# Efeitos aleatórios

#random <- plm(formula = f,
#             data = tbl,
#             index = c("Acao", "Ano"),
#              model="random")
#summary(random)


#phtest(fixed, random)

# # Determinantes do score Disclosure Ambiental 
# 
# f <- formula(paste("Environmental_Disclosure_Score ~", regressores))
# 
# # Efeitos fixos
# 
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# 
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #             index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# #phtest(fixed, random)
# 
# # Determinantes do score Disclosure Social
# 
# f <- formula(paste("Social_Disclosure_Score ~", regressores))
# # Efeitos fixos
# 
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# # Determinantes do score de Disclosure Governança 
# 
# f <- formula(paste("Governance_Disclosure_Score ~", regressores))
# 
# # Efeitos fixos
# 
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
#############################################################################################
# LAG - Analise de determinantes em lag  de ESG Disclosure Score ------------------------------------------------------

f <- formula(paste("ESG_Disclosure_Score ~", regressoresLAG))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)
## Empresas grandes, mais valiosas e mais líquidas  investem mais em Disclosure ESG

# Efeitos aleatórios

#random <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="random")
#summary(random)


#phtest(fixed, random)

# # Determinantes do score Ambiental 
# 
# f <- formula(paste("Environmental_Disclosure_Score ~", regressoresLAG))
# 
# # Efeitos fixos
# 
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# 
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #             data = tbl,
# #             index = c("Acao", "Ano"),
# #             model="random")
# #summary(random)
# 
# #phtest(fixed, random)
# 
# # Determinantes do score Social
# 
# f <- formula(paste("Social_Disclosure_Score ~", regressoresLAG))
# 
# # Efeitos fixos
# 
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #              data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)
# 
# # Determinantes do score de Governança 
# 
# f <- formula(paste("Governance_Disclosure_Score ~", regressoresLAG))
# 
# # Efeitos fixos
# 
# fixed <- plm(formula = f,
#              data = tbl,
#              index = c("Acao", "Ano"),
#              model="within")
# summary(fixed)
# 
# # Efeitos aleatórios
# 
# #random <- plm(formula = f,
# #             data = tbl,
# #              index = c("Acao", "Ano"),
# #              model="random")
# #summary(random)

######################################################################################
######################################################################################

# Determinantes do  Retorno da ação :  d.ln_price ~ BESG_ESG_Score 


f <- formula("d.ln_price  ~ 
             Trailing_12M_EBITDA_Margin + 
             Cap_Expend_to_Tot_Assets + 
             Return_on_Invested_Capital +
             BESG_ESG_Score +
             Total_Assets +
             Total_Debt_to_Total_Assets +
             Normalized_Net_Income_Growth +
             Revenue_Growth_Year_over_Year +
             Number_Of_Trades +
             Long_Term_Assets_as___Total_Assets +
             Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)


## Empresas com melhor Score ESG têm menor retorno  da ação 

#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
# ------------------------------------------
f <- formula("d.ln_price  ~ 
             LAG_Trailing_12M_EBITDA_Margin + 
             LAG_Cap_Expend_to_Tot_Assets + 
             LAG_Return_on_Invested_Capital +
             LAG_BESG_ESG_Score +
             LAG_Total_Assets +
             LAG_Total_Debt_to_Total_Assets +
             LAG_Normalized_Net_Income_Growth +
             LAG_Revenue_Growth_Year_over_Year +
             LAG_Number_Of_Trades +
             LAG_Long_Term_Assets_as___Total_Assets +
             LAG_Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)


## Score ESG defasado não tem correlação com retorno futuro da ação


#####################################################################################
# Determinantes do retorno:  d.ln_price ~ Disclosure Score 


f <- formula("d.ln_price  ~ 
             Trailing_12M_EBITDA_Margin + 
             Cap_Expend_to_Tot_Assets + 
             Return_on_Invested_Capital +
             ESG_Disclosure_Score +
             Total_Assets +
             Total_Debt_to_Total_Assets +
             Normalized_Net_Income_Growth +
             Revenue_Growth_Year_over_Year +
             Number_Of_Trades +
             Long_Term_Assets_as___Total_Assets + Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Empresas com melhor Disclosure Score têm pior retorno futuro ( tal como acontece no Score)

#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
# ------------------------------------------
f <- formula("d.ln_price  ~ 
             LAG_Trailing_12M_EBITDA_Margin + 
             LAG_Cap_Expend_to_Tot_Assets + 
             LAG_Return_on_Invested_Capital +
             LAG_ESG_Disclosure_Score +
             LAG_Total_Assets +
             LAG_Total_Debt_to_Total_Assets +
             LAG_Normalized_Net_Income_Growth +
             LAG_Revenue_Growth_Year_over_Year +
             LAG_Number_Of_Trades +
             LAG_Long_Term_Assets_as___Total_Assets +
             LAG_Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Disclosure Score ESG defasado não tem correlação com retorno futuro ação ( tal como aconteceu com SCORE)

############################################################################################
#  Determinantes do Valor da empresa:  Tobin's Q ~ ESG Score


f <- formula("Tobin_s_Q_Ratio ~
              Trailing_12M_EBITDA_Margin + 
              Cap_Expend_to_Tot_Assets + 
              Return_on_Invested_Capital + BESG_ESG_Score +
              Total_Assets +
              Total_Debt_to_Total_Assets +
              Normalized_Net_Income_Growth +
              Revenue_Growth_Year_over_Year +
              Number_Of_Trades +
              Long_Term_Assets_as___Total_Assets + Volatility_360_Day")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Score ESG não tem relação com valuation da empresa 

#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

f <- formula("Tobin_s_Q_Ratio ~
              LAG_Trailing_12M_EBITDA_Margin + 
              LAG_Cap_Expend_to_Tot_Assets + 
              LAG_Return_on_Invested_Capital + 
              LAG_BESG_ESG_Score +
              LAG_Total_Assets +
              LAG_Total_Debt_to_Total_Assets +
              LAG_Normalized_Net_Income_Growth +
              LAG_Revenue_Growth_Year_over_Year +
              LAG_Number_Of_Trades +
              LAG_Long_Term_Assets_as___Total_Assets +
              LAG_Volatility_360_Day")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Score ESG defasado  não tem relação com valuation futuro da empresa ( nem retorno )
#########################################################################################
# Determinantes do Valor da empresa:   Tobin's Q ~ ESG Disclosure Score

f <- formula("Tobin_s_Q_Ratio ~
              Trailing_12M_EBITDA_Margin + 
              Cap_Expend_to_Tot_Assets + 
              Return_on_Invested_Capital + ESG_Disclosure_Score +
              Total_Assets +
              Total_Debt_to_Total_Assets +
              Normalized_Net_Income_Growth +
              Revenue_Growth_Year_over_Year +
              Number_Of_Trades +
              Long_Term_Assets_as___Total_Assets + Volatility_360_Day")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)
## Disclosure Score  não tem relação com valuation da empresa


#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
f <- formula("Tobin_s_Q_Ratio ~
              LAG_Trailing_12M_EBITDA_Margin + 
              LAG_Cap_Expend_to_Tot_Assets + 
              LAG_Return_on_Invested_Capital + 
              LAG_ESG_Disclosure_Score +
              LAG_Total_Assets +
              LAG_Total_Debt_to_Total_Assets +
              LAG_Normalized_Net_Income_Growth +
              LAG_Revenue_Growth_Year_over_Year +
              LAG_Number_Of_Trades +
              LAG_Long_Term_Assets_as___Total_Assets +
              LAG_Volatility_360_Day")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Disclosure Score defasado  não tem relação com valuation futuro da empresa

#############################################################################################
#############################################################################################
# Determinantes do risco : Volatility ~ ESG Score

f <- formula("Volatility_360_Day ~ 
             Trailing_12M_EBITDA_Margin + 
             Cap_Expend_to_Tot_Assets + 
             Return_on_Invested_Capital + BESG_ESG_Score +
             Total_Assets +
             Total_Debt_to_Total_Assets +
             Normalized_Net_Income_Growth +
             Revenue_Growth_Year_over_Year +
             Number_Of_Trades +
             Long_Term_Assets_as___Total_Assets + 
             Tobin_s_Q_Ratio")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## ESG Score  não tem relação com risco total  da empresa

# LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

f <- formula("Volatility_360_Day~
              LAG_Trailing_12M_EBITDA_Margin + 
              LAG_Cap_Expend_to_Tot_Assets + 
              LAG_Return_on_Invested_Capital + 
              LAG_BESG_ESG_Score +
              LAG_Total_Assets +
              LAG_Total_Debt_to_Total_Assets +
              LAG_Normalized_Net_Income_Growth +
              LAG_Revenue_Growth_Year_over_Year +
              LAG_Number_Of_Trades +
              LAG_Long_Term_Assets_as___Total_Assets +
              LAG_Tobin_s_Q_Ratio")

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## ESG Score  defasado não tem relação com risco total  da empresa

################################################################################################
# Caso Volatility ~ Disclosure Score

f <- formula("Volatility_360_Day ~ 
             Trailing_12M_EBITDA_Margin + 
             Cap_Expend_to_Tot_Assets + 
             Return_on_Invested_Capital + ESG_Disclosure_Score +
             Total_Assets +
             Total_Debt_to_Total_Assets +
             Normalized_Net_Income_Growth +
             Revenue_Growth_Year_over_Year +
             Number_Of_Trades +
             Long_Term_Assets_as___Total_Assets + 
             Tobin_s_Q_Ratio")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Disclosure Score  não tem relação com risco total  da empresa

#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

f <- formula("Volatility_360_Day ~ 
             LAG_Trailing_12M_EBITDA_Margin + 
             LAG_Cap_Expend_to_Tot_Assets + 
             LAG_Return_on_Invested_Capital + LAG_ESG_Disclosure_Score +
             LAG_Total_Assets +
             LAG_Total_Debt_to_Total_Assets +
             LAG_Normalized_Net_Income_Growth +
             LAG_Revenue_Growth_Year_over_Year +
             LAG_Number_Of_Trades +
            LAG_Long_Term_Assets_as___Total_Assets + 
             LAG_Tobin_s_Q_Ratio")
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

## Disclosure Score defasado  não tem relação com risco total  da empresa



####################################################################################
# Determinantes do risco :  Beta ~ ESG Score

f <- formula("Applied_Beta_for_EQRP  ~ 
             Trailing_12M_EBITDA_Margin + 
             Cap_Expend_to_Tot_Assets + 
             Return_on_Invested_Capital + BESG_ESG_Score +
             Total_Assets +
             Total_Debt_to_Total_Assets +
             Normalized_Net_Income_Growth +
             Revenue_Growth_Year_over_Year +
             Number_Of_Trades +
             Long_Term_Assets_as___Total_Assets + Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# ESG Score não tem relação com Beta 

# LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

f <- formula("Applied_Beta_for_EQRP  ~ 
             LAG_Trailing_12M_EBITDA_Margin + 
             LAG_Cap_Expend_to_Tot_Assets + 
             LAG_Return_on_Invested_Capital + LAG_BESG_ESG_Score +
             LAG_Total_Assets +
             LAG_Total_Debt_to_Total_Assets +
             LAG_Normalized_Net_Income_Growth +
             LAG_Revenue_Growth_Year_over_Year +
             LAG_Number_Of_Trades +
             LAG_Long_Term_Assets_as___Total_Assets + LAG_Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# ESG Score defasado não tem relação com Beta

#################################################################################################
# Determinantes do risco:  Beta ~ Disclosure Score 

f <- formula("Applied_Beta_for_EQRP  ~ 
             Trailing_12M_EBITDA_Margin + 
             Cap_Expend_to_Tot_Assets + 
             Return_on_Invested_Capital + ESG_Disclosure_Score +
             Total_Assets +
             Total_Debt_to_Total_Assets +
             Normalized_Net_Income_Growth +
             Revenue_Growth_Year_over_Year +
             Number_Of_Trades +
             Long_Term_Assets_as___Total_Assets + Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# ESG Disclosure Score não tem relação com Beta

#LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

f <- formula("Applied_Beta_for_EQRP  ~ 
             LAG_Trailing_12M_EBITDA_Margin + 
             LAG_Cap_Expend_to_Tot_Assets + 
            LAG_Return_on_Invested_Capital + LAG_ESG_Disclosure_Score +
             LAG_Total_Assets +
             LAG_Total_Debt_to_Total_Assets +
             LAG_Normalized_Net_Income_Growth +
             LAG_Revenue_Growth_Year_over_Year +
             LAG_Number_Of_Trades +
             LAG_Long_Term_Assets_as___Total_Assets + LAG_Tobin_s_Q_Ratio")

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# ESG Disclosure Score defasado  não tem relação com Beta

##############################################################################
#############################################################################

