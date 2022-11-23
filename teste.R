rm(list = ls())

library(readxl)
library(seasonal)
library(ggplot2)
library(lubridate)


dados_fiscal <- read_excel("dados_fiscal.xlsx")

dados_fiscal$Data <- as.Date(dados_fiscal$Data)

start_date <- c(lubridate::year(dados_fiscal$Data[1]), lubridate::month(dados_fiscal$Data[1]))

gy.ts <- ts(dados_fiscal$gy, start = start_date, frequency = 12)
ty.ts <- ts(dados_fiscal$tyy, start = start_date, frequency = 12)

ggplot(dados_fiscal) + 
  geom_line(aes(x=Data, y = ty)) +
  geom_line(aes(x=Data, y = gy))


m <- seas(x = gy.ts)

m$data

summary(m)

plot(gy.ts)

plot(m$data[,"seasonal"])
plot(m)


seasonal::final(m)

plot(gy.ts/final(m))
plot(original(m))
plot(irregular(m))
plot(gy.ts-trend(m))

seasonal::fivebestmdl(m)
boxplot(final(m))









