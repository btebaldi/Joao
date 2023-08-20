#' ---
#' title: "Capitulo 3"
#' author: "João Luis Tenreiro Barroso"
#' date: "2023-07-28"
#' output: html_document
#' ---
#' 
#' Script com regressoes do capitulo 3


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


# Determina regressores de controle ---------------------------------------

regressores <- "Trailing_12M_EBITDA_Margin + 
                Cap_Expend_to_Tot_Assets + 
                Return_on_Invested_Capital + 
                Total_Assets +
                Total_Debt_to_Total_Assets +
                Normalized_Net_Income_Growth +
                Revenue_Growth_Year_over_Year +
                Number_Of_Trades +
                Long_Term_Assets_as___Total_Assets +
                Tobin_s_Q_Ratio"


# Analise de Volatilidade vs ESG Score ------------------------------------

f <- formula(paste("Volatility_360_Day ~", regressores,
                   "+ BESG_ESG_Score + LAG_BESG_ESG_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# Analise de Volatilidade vs ESG Disclosure -------------------------------

f <- formula(paste("Volatility_360_Day ~", regressores,
                   "+ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Analise de Beta vs ESG Score --------------------------------------------

f <- formula(paste("Beta_winsorized ~", regressores,
                   "+ BESG_ESG_Score + LAG_BESG_ESG_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# Analise de Beta vs ESG Disclosure ---------------------------------------

f <- formula(paste("Beta_winsorized ~", regressores,
                   "+ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# Analise de Risk_Premium vs Score ----------------------------------------

f <- formula(paste("Risk_Premium ~ BESG_ESG_Score + LAG_BESG_ESG_Score + SMB + WML + IML + HML + Rm_minus_Rf"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Analise de Risk_Premium vs Disclosure -----------------------------------

f <- formula(paste("Risk_Premium ~ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score + SMB + WML + IML + HML + Rm_minus_Rf"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# Analise de Sharpe vs Score ----------------------------------------------

f <- formula(paste("Sharpe ~ BESG_ESG_Score + LAG_BESG_ESG_Score + SMB + WML + IML + HML + Rm_minus_Rf"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Analise de Sharpe vs Disclorure -----------------------------------------

f <- formula(paste("Sharpe ~ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score + SMB + WML + IML + HML + Rm_minus_Rf"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

