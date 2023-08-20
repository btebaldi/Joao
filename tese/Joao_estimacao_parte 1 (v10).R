#' ---
#' title: "Capitulo 1"
#' author: "João Luis Tenreiro Barroso"
#' date: "2023-07-28"
#' output: html_document
#' ---
#' 
#' Script com regressoes do capitulo 1


# Setup -------------------------------------------------------------------


# limpeza das variaveis de ambiente
rm(list = ls())

# lista de bibliotecas utilizadas
# library(readxl) 
library(dplyr)
# library(magrittr)
# library(ggplot2)
library(plm)
# library(DescTools)

# Data load ---------------------------------------------------------------

#  leitura da base de dados
tbl <- readRDS(file = here::here("./tese/base.rds"))


# Determina os regressores ------------------------------------------------

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

regressores_LAG <- "LAG_Trailing_12M_EBITDA_Margin + 
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

# BESG_ESG_Score ----------------------------------------------------------


f <- formula(paste("BESG_ESG_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Efeitos aleatórios
random <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="random")
summary(random)

# teste de Hausman
phtest(fixed, random)


# BESG_ESG_SCORE em seus diversos fatores ---------------------------------

f <- formula(paste("BESG_Environmental_Pillar_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

f <- formula(paste("BESG_Social_Pillar_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

f <- formula(paste("BESG_Governance_Pillar_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# BESG_ESG_Score em lag ---------------------------------------------------

f <- formula(paste("BESG_ESG_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# BESG_ESG_Score em lag em seus diversos fatores --------------------------

f <- formula(paste("BESG_Environmental_Pillar_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


f <- formula(paste("BESG_Social_Pillar_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


f <- formula(paste("BESG_Governance_Pillar_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)



# ESG Disclosure Score ----------------------------------------------------

f <- formula(paste("ESG_Disclosure_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# ESG Disclosure Score em seus diversos fatores ---------------------------

# Determinantes do score Ambiental 
f <- formula(paste("Environmental_Disclosure_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Determinantes do score Social
f <- formula(paste("Social_Disclosure_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Determinantes do score de Governança 
f <- formula(paste("Governance_Disclosure_Score ~", regressores))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# ESG Disclosure Score em lag ---------------------------------------------

f <- formula(paste("ESG_Disclosure_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# ESG Disclosure Score em lag em seus diversos fatores --------------------

# Determinantes do score Ambiental 
f <- formula(paste("Environmental_Disclosure_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Determinantes do score Social
f <- formula(paste("Social_Disclosure_Score ~", regressores_LAG))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Determinantes do score de Governança 
f <- formula(paste("Governance_Disclosure_Score ~", regressores_LAG))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)
