
# limpeza das variaveis de ambiente
rm(list = ls())

# lista de bibliotecas utilizadas
library(readxl) 
library(dplyr)
library(magrittr)
library(ggplot2)
library(plm)

#  leitura da base de dados
tbl <- read_excel("TabelaFinal 1406 corrigido.xlsx", sheet = "Sheet 1")

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


# REMOCAO DE OUTLIERS -----------------------------------------------------
# eliminar ESG ausentes
tbl <- tbl %>% filter(!(is.na(ESG_Disclosure_Score))) 

# remove as acoes de prio de anos anteriores a 2014
tbl <- tbl %>% filter(!(Acao == "PRIO3 BS Equity" & Ano <= 2014))

# Cap. Expenditures / Total Assets = 155.6129 ( ECOR3 2004)
tbl <- tbl %>% filter(!(Acao == "ECOR3 BS Equity" & Ano == 2004))

# Return on invested capital = -169.242 ( Gol4 2021)
tbl <- tbl %>% filter(!(Acao == "GOLL4 BS Equity" & Ano == 2021))

# Tobin Q = 1624786 ( WEGE3 1994/1995)
tbl <- tbl %>% filter(!(Acao == "WEGE3 BS Equity" & Ano == 1994))
tbl <- tbl %>% filter(!(Acao == "WEGE3 BS Equity" & Ano == 1995))

# Normalized net income growth = -29724 ( VAA3 2009)
tbl <- tbl %>% filter(!(Acao == "VIIA3 BS Equity" & Ano == 2009))

# EBIT = - 25665 ( Vale3 2015)  
tbl <- tbl %>% filter(!(Acao == "VALE3 BS Equity" & Ano == 2015))


# Analise de integridade --------------------------------------------------

tbl %>%
  count(Acao, Ano) %>% 
  arrange(-n) %>% print(n=20)


# analise de BESG_ESG_Score ------------------------------------------------

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

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)


phtest(fixed, random)


# analise de BESG_Environmental_Pillar_Score ------------------------------------------------

f <- formula(paste("BESG_Environmental_Pillar_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)


phtest(fixed, random)


# analise de BESG_Governance_Pillar_Score ------------------------------------------------

f <- formula(paste("BESG_Governance_Pillar_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)


phtest(fixed, random)


# analise de BESG_Social_Pillar_Score ------------------------------------------------

f <- formula(paste("BESG_Social_Pillar_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)


phtest(fixed, random)


# regressoes antigas ------------------------------------------------------


f <- formula(paste("ESG_Disclosure_Score ~", regressores))

# Efeitos fixos
fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)


phtest(fixed, random)

# Determinantes do score Ambiental 

f <- formula(paste("Environmental_Disclosure_Score ~", regressores))

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")

summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)

phtest(fixed, random)

# Determinantes do score Social

f <- formula(paste("Social_Disclosure_Score ~", regressores))
# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)

# Determinantes do score de Governança 

f <- formula(paste("Governance_Disclosure_Score ~", regressores))

# Efeitos fixos

fixed <- plm(formula = f,
             data = tbl,
             index = c("Acao", "Ano"),
             model="within")
summary(fixed)

# Efeitos aleatórios

random <- plm(formula = f,
              data = tbl,
              index = c("Acao", "Ano"),
              model="random")
summary(random)

# Exploração :

# Caso Valor da empresa

f <- formula("Tobin_s_Q_Ratio ~ -1 +
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

# Caso Volatility

f <- formula("Volatility_360_Day ~ -1 +
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


