#2.14 Exercicio 10
dados <- read.csv("\\Introdução à Análise de Eventos Extremos\\Datapct.csv")
head(dados)
install.packages("evir")
library(evir)

valores_r <- 120 #em torno de 10% amostra +-1200 obs
valores_xi <- numeric(length(valores_r))
gev(dados$AAPL, block = r)$par.ests
