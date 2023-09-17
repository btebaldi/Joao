#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2023-07-27
#' 
#' Script que prepara a base para as regressoes dos capitulos 


# Setup -------------------------------------------------------------------

# limpeza das variaveis de ambiente
rm(list = ls())

# lista de bibliotecas utilizadas
library(readxl) 
library(dplyr)
library(magrittr)
library(ggplot2)
library(plm)
library(DescTools)
library(ggcorrplot)
library(tidyr)

# Data load ---------------------------------------------------------------

# Leitura da base de dados
tbl <- read_excel(here::here("./tese/TabelaFinal 2807.xlsx"), sheet = "Sheet 1")

# Analise de Disclosure Score
tbl %>% 
  filter( !(is.na(`ESG Disclosure Score`)) ) %>% 
  group_by(Ano) %>% 
  summarise(Cont = n()) %>% 
  arrange(Cont)

# Analise de ESG Score
tbl %>% 
  filter( !(is.na(`BESG ESG Score`)) ) %>% 
  group_by(Ano) %>% 
  summarise(Cont = n()) %>% 
  arrange(Cont)


# Carrega base de configuracao de setor das empresas
tbl_classificacao <- read_excel(here::here("./tese/economatica base setores.xlsx"))


# cria ticker bbg na base de classificacao setorial
tbl_classificacao <- tbl_classificacao %>% select(Codigo, Setor_Economico_Bovespa, Subsetor_Bovespa) %>%
  mutate(Ticker_bbg = sprintf("%s BS Equity", Codigo))

# Carrega base com informacao de Sharpe
base_Sharpe <- read_excel(here::here("tese/economatica_Sharpe.xlsx"), range = "A5:AD554", na = c("-"))

base_Sharpe <- base_Sharpe %>% 
  mutate(Ticker_bbg = sprintf("%s BS Equity", Ticker)) %>% 
  select(Ticker_bbg, starts_with("Sharpe_")) %>% 
  pivot_longer(cols = starts_with("Sharpe"),
               names_to = "Ano",
               names_prefix = "Sharpe_",
               names_transform = as.numeric,
               values_to = "Sharpe")

# Atualzia banco de dados com o sharpe
tbl <- tbl %>%
  left_join(base_Sharpe, by = c("Ação" = "Ticker_bbg", "Ano"="Ano"))


# Carrega base com informacao de DRE
base_DRE <- read_excel(here::here("tese/economatica base DRE.xlsx"), range = "A2:AD552", na = c("-"))

colnames(base_DRE)
base_DRE <- base_DRE %>% 
  mutate(Ticker_bbg = sprintf("%s BS Equity", Codigo)) %>% 
  select(Ticker_bbg, starts_with("PL")) %>% 
  pivot_longer(cols = starts_with("PL"),
               names_to = "Ano",
               names_prefix = "PL_",
               names_transform = as.numeric,
               values_to = "PL")

# Determina quais sao as empresas em situacao de is_Financial_Distress 
tbl <- tbl %>%
  left_join(base_DRE, by = c("Ação" = "Ticker_bbg", "Ano"="Ano")) %>%
  mutate(is_Financial_Distress = if_else(PL<0,1,0)) %>% 
  select(-PL)

# Carrega dados de fatores
NEFIN_Factores <- read_excel(here::here("tese/NEFIN_Factores.xlsx"))

NEFIN_Factores <- NEFIN_Factores %>%
  group_by(year) %>% 
  mutate(date = as.Date(date),
         min_date = min(date),
         max_date = max(date)) %>% 
  filter(date == max_date) %>% 
  select(year, Idx_SMB, Idx_WML, Idx_IML, Idx_HML, Idx_Rm_minus_Rf)

# Data regularization -----------------------------------------------------

# # exclusao de empresas do setor financeiro
tbl <- tbl %>%
  inner_join(tbl_classificacao, by = c("Ação" = "Ticker_bbg")) %>%
  filter(!(Subsetor_Bovespa %in% c("Intermediários financeiros", "Previdência e seguros", "Exploração de imóveis", "Serviços financeiros diversos")))

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


# Filtrando a base para anos posteriores a 2009 (inclusive)
tbl <- tbl %>% filter(Ano >= 2009)


# Filtrando a base para eliminar empresas em Financial Distress
tbl <- tbl %>% filter(is_Financial_Distress == 0)


# Adiciona informacao de fatores
tbl <- tbl %>%
  left_join(NEFIN_Factores, by = c("Ano" = "year"))


# Analise de distribuicao de empresas por ano -----------------------------

tbl %>% 
  group_by(Ano) %>% 
  summarise(Cont_empresas = n(),
            Cont_Score = sum(!is.na(BESG_ESG_Score)),
            Cont_Disclosure = sum(!is.na(ESG_Disclosure_Score))) %>% 
  arrange(Cont_empresas)


# Remocao de outliers (winsorização) -----------------------------------------------------

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

tbl$Total_Debt_to_Total_Assets = Winsorize(tbl$Total_Debt_to_Total_Assets,
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


colnames(tbl)

corr <- tbl %>% 
  select("Margin EBITDA"="Trailing_12M_EBITDA_Margin",
         "Capex"="Cap_Expend_to_Tot_Assets",
         "ROI" = "Return_on_Invested_Capital",
         "Tobin's Q" = "Tobin_s_Q_Ratio",
         "Total Assets" = "Total_Assets",
         "Debt" = "Total_Debt_to_Total_Assets",
         "Income Growth" = "Normalized_Net_Income_Growth",
         "Long Term Assets" = "Long_Term_Assets_as___Total_Assets",
         "ESG Disclosure" = "ESG_Disclosure_Score",
         "EBIT" = "EBIT",
         "Revenue Growth" = "Revenue_Growth_Year_over_Year",
         "Revenue_Adjusted",
         "Liquidity"="Number_Of_Trades",
         "Volatility"="Volatility_360_Day",
         "Risk Premium" ="Risk_Premium",
         # "Last_Price",
         "ESG_Score"="BESG_ESG_Score",
         "Sharpe"="Sharpe",
         "Beta" = "Beta_winsorized") %>% 
  cor(use = "pairwise.complete.obs")


# Grafico -----------------------------------------------------------------

ggcorrplot(corr,
           hc.order = TRUE, 
           type = "upper",
           lab = TRUE, 
           lab_size = 3, 
           show.diag = TRUE,
           # method="circle",
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)


ggsave(filename = "./tese/Correlograma.png",units = "in",
       width = 8, height = 6,dpi = 100, scale = 2)


# Calculo do d.ln(last_price) e SMB WML IML HML ---------------------------

# arrumo a tabela para ficar na ordem de acao e ano
tbl <- tbl %>%  arrange(Acao, Ano)

# Inicializa a coluna com a info d.ln
tbl$d.ln_price = NA

# Inicializa a coluna com a info de SMB WML IML HML
tbl$SMB = NA
tbl$WML = NA
tbl$IML = NA
tbl$HML = NA
tbl$HML = NA
tbl$Rm_minus_Rf = NA


for(i in 1:nrow(tbl)){
  # na primeira linha nao temos passado. Colocar NA
  if(i == 1) {
    tbl$d.ln_price[i] = NA
    
    tbl$SMB[i] = NA
    tbl$WML[i] = NA
    tbl$IML[i] = NA
    tbl$HML[i] = NA
    tbl$Rm_minus_Rf[i] = NA
    
  } else {
    if(tbl$Acao[i] == tbl$Acao[i-1]){
      tbl$d.ln_price[i] = log(tbl$Last_Price[i])-log(tbl$Last_Price[i-1])
      
      tbl$SMB[i] = tbl$Idx_SMB[i] - tbl$Idx_SMB[i-1]
      tbl$WML[i] = tbl$Idx_WML[i] - tbl$Idx_WML[i-1]
      tbl$IML[i] = tbl$Idx_IML[i] - tbl$Idx_IML[i-1]
      tbl$HML[i] = tbl$Idx_HML[i] - tbl$Idx_HML[i-1]
      tbl$Rm_minus_Rf[i] = tbl$Idx_Rm_minus_Rf[i] - tbl$Idx_Rm_minus_Rf[i-1]  
      
    } else{
      tbl$d.ln_price[i] = NA
      
      tbl$SMB[i] = NA
      tbl$WML[i] = NA
      tbl$IML[i] = NA
      tbl$HML[i] = NA
      tbl$Rm_minus_Rf[i] = NA
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
tbl[ , "LAG_Sharpe"] = NA

tbl[ , "LAG_ESG_Disclosure_Score"] = NA
tbl[ , "LAG_Environmental_Disclosure_Score"] = NA
tbl[ , "LAG_Governance_Disclosure_Score"] = NA
tbl[ , "LAG_Social_Disclosure_Score"] = NA

tbl[ , "LAG_BESG_ESG_Score"] = NA
tbl[ , "LAG_BESG_Environmental_Pillar_Score"] = NA
tbl[ , "LAG_BESG_Governance_Pillar_Score"] = NA
tbl[ , "LAG_BESG_Social_Pillar_Score"] = NA


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
      tbl[i, "LAG_Sharpe"] = tbl[i-1, "Sharpe"]
      
      tbl[i, "LAG_ESG_Disclosure_Score"] = tbl[i-1, "ESG_Disclosure_Score"]
      tbl[i, "LAG_Environmental_Disclosure_Score"] = tbl[i-1, "Environmental_Disclosure_Score"]
      tbl[i, "LAG_Governance_Disclosure_Score"] = tbl[i-1, "Governance_Disclosure_Score"]
      tbl[i, "LAG_Social_Disclosure_Score"] = tbl[i-1, "Social_Disclosure_Score"]
      
      tbl[i, "LAG_BESG_ESG_Score"] = tbl[i-1, "BESG_ESG_Score"]
      tbl[i, "LAG_BESG_Environmental_Pillar_Score"] = tbl[i-1, "BESG_Environmental_Pillar_Score"]
      tbl[i, "LAG_BESG_Governance_Pillar_Score"] = tbl[i-1, "BESG_Governance_Pillar_Score"]
      tbl[i, "LAG_BESG_Social_Pillar_Score"] = tbl[i-1, "BESG_Social_Pillar_Score"]
      
    } else{
      next
    }
  }
}



# Analise de integridade --------------------------------------------------

sort(unique(tbl$Ano))
sort(unique(tbl$Acao))

tbl %>%
  count(Acao, Ano) %>% 
  arrange(-n) %>% print(n=10)


# Salva a base de dados ---------------------------------------------------

saveRDS(object = tbl, file = "./tese/base.rds")
