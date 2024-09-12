###Exercicio 9 (Meio zoado, trabalheira de pegar dados melhores usei esses mesmo)
#Vamos baixar os pacotes, pegar os dados em API e tirar os Nan. Vamos pegar a coluna de maximos 
#diarios, após isso iremos fazer uma séria de maximos com n=365(Aprox o max do ano).
#Após plotamos a distribuição dos máximos com uma Gumbell teorica
# Instalar e carregar o pacote se necessário

##(PROBLEMA!!!!, o negocio ta em painel, com varias observaçoes pro mesmo t, por isso tem 
##mais de 10k de obs pra 2024, como não achei onde discriminar pra pegar só uma observação
##deixei assim, mas tem esse problema. A ideia do código ta certa, mas esses dados tão zoados...)
#install.packages("GSODR")
library(GSODR)
library(dplyr)
library(evd)

gsod_data <- get_GSOD(years = 2024, country = "Brazil")
gsod_data_clean <- gsod_data[!is.na(gsod_data$MAX), ]
n <- 30  #Mudar aqui para ver outros periodos
# Aplicar sapply para calcular o máximo em blocos de 30 elementos(aprox 1 mes)
Max <- sapply(seq(1, length(gsod_data_clean$MAX), by = n), function(i) {
  max(gsod_data_clean$MAX[i:min(i + n - 1, length(gsod_data_clean$MAX))], na.rm = TRUE)
})

fit_gumbel <- fgev(Max)
fit_gumbel$estimate
dgumbel <- function(x, mu, beta) {
  z <- (x - mu) / beta
  (1 / beta) * exp(-(z + exp(-z)))
}

hist(Max)
plot(density(Max))
curve(dgumbel(x, mu = fit_gumbel$estimate["loc"], beta = fit_gumbel$estimate["scale"]),
      col = "red", lwd = 2, add = TRUE)

plot(Max)

gsod_data$STATE
