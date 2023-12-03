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

regressores <- "Idx_SMB +
                Idx_WML +
                Idx_IML +
                Idx_HML +
                Idx_Rm_minus_Rf"

# Analise de Volatilidade vs ESG Score ------------------------------------

f <- formula(paste("WACC_COST_DEBT  ~", regressores,
                   "+ BESG_ESG_Score + ESG_Disclosure_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)


# Analise de Volatilidade vs ESG Disclosure -------------------------------

f <- formula(paste("WACC ~ ", regressores,
                   "+ BESG_ESG_Score + ESG_Disclosure_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)

# Analise de Beta vs ESG Score --------------------------------------------

f <- formula(paste("WACC_COST_EQUITY ~ ", regressores,
                   "+ BESG_ESG_Score + ESG_Disclosure_Score"))

# Efeitos fixos
fixed <- plm(formula = f, data = tbl, index = c("Acao", "Ano"), model="within")
summary(fixed)
