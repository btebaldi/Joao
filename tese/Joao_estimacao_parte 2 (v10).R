#' ---
#' title: "Capitulo 2"
#' author: "João Luis Tenreiro Barroso"
#' date: "2023-11-24"
#' output: html_document
#' ---
#' 
#' Script com regressoes do capitulo 2


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
tbl <- readRDS(file = here::here("C:/BACKUP/BACKUP_JOAO_BARROSO/USER/Desktop/Doutorado Profissional/Minha Tese  DPE ESG/TESE DPE/base.rds"))


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
  Volatility_360_Day" 


# Analise do retorno vs ESG Score -----------------------------------------

# f <- formula(paste("d.ln_price ~", regressores, "+ Tobin_s_Q_Ratio",
#                   "+ BESG_ESG_Score + LAG_BESG_ESG_Score"))

f <- formula(paste("d.ln_price ~", regressores, "+ Tobin_s_Q_Ratio",
                   "+ BESG_ESG_Score"))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# CONCLUSÃO 1.1 ESG Score associado a menor retorno da ação 

# Analise do retorno vs ESG Disclosure ------------------------------------

# f <- formula(paste("d.ln_price ~", regressores, "+ Tobin_s_Q_Ratio",
#                    "+ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score"))

f <- formula(paste("d.ln_price ~", regressores, "+ Tobin_s_Q_Ratio",
                   "+ ESG_Disclosure_Score "))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# CONCLUSÃO 1.2 ESG Disclosure associado  a menor retorno da ação ( não significante)

#Analise do Q de tobin vs ESG Score ---------------------------------

# f <- formula(paste("Tobin_s_Q_Ratio ~", regressores,
#                    "+ BESG_ESG_Score + LAG_BESG_ESG_Score"))

f <- formula(paste("Tobin_s_Q_Ratio ~", regressores,
                   "+ BESG_ESG_Score "))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# CONCLUSÃO 2.1 : Q Tobin não relacionado com ESG Score ( nem vice versa, como vimos)

 #Analise do Q de tobin vs ESG Disclosure ---------------------------------

# f <- formula(paste("Tobin_s_Q_Ratio ~", regressores,
#                    "+ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score"))


f <- formula(paste("Tobin_s_Q_Ratio ~", regressores,
                "+ ESG_Disclosure_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# CONCLUSÃO 2.2 : Q Tobin não relacionado com ESG Disclosure ( nem vice versa, como vimos)

# Analise do Risk premium ( Retorno em excesso sobre Rf) vs ESG Score ------------------------------------

# f <- formula(paste("Risk_Premium ~", regressores,
#                    "+ BESG_ESG_Score + LAG_BESG_ESG_Score", 
#                   "+ Tobin_s_Q_Ratio"))

 f <- formula(paste("Risk_Premium ~", regressores,
                 "+ BESG_ESG_Score", 
               "+ Tobin_s_Q_Ratio"))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

#  CONCLUSÃO 3.1 Retorno em excesso sobre Rf relacionado a ESG Score

# Analise do Risk premium( Retorno em excesso) vs ESG Disclosure -------------------------------

#f <- formula(paste("Risk_Premium ~", regressores,
#                  "+ ESG_Disclosure_Score + LAG_ESG_Disclosure_Score", 
#                   "+ Tobin_s_Q_Ratio"))

f <- formula(paste("Risk_Premium ~", regressores,
                 "+ ESG_Disclosure_Score", 
                   "+ Tobin_s_Q_Ratio"))
# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# CONCLUSÃO 3.1 Retorno em excesso relacionado a Disclosure Score


## ESG Score e Diclosure relacionados com menor retorno das ações ,
# mas associados a maior retorno em excesso acima da R_f



