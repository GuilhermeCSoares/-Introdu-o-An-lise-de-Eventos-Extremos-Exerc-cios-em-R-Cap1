## Exercício 3 e 4- Capítulo 2.14
#Só trocar o valor de n, para n=3,5,10 para ter os plots do exercicio 4
library(evd)
# Gerando amostra aleatória da distribuição exp(1) com n = 1000
x <- rexp(1000, 1)

# Pegando o máximo de M_n com blocos de tamanho n
n <- 10 # Tamanho dos blocos
M_n <- sapply(seq(1, length(x), by = n), function(i) max(x[i:min(i + n - 1, length(x))]))
# Normalizando os máximos com c_n = 1 e d_n = ln(n)
Mn_n <- (M_n - log(n)) / 1  
# Estimando os parâmetros da distribuição Gumbel
fit_gumbel <- fgev(Mn_n)
fit_gumbel$estimate
dgumbel <- function(x, mu, beta) {
  z <- (x - mu) / beta
  (1 / beta) * exp(-(z + exp(-z)))
}

hist(Mn_n, probability = TRUE, main = "Histograma e Densidades Gumbel e Empírica",
     xlab = "Máximos Normalizados", col = "lightblue")
lines(density(Mn_n), col = "blue", lwd = 2)
curve(dgumbel(x, mu = fit_gumbel$estimate["loc"], beta = fit_gumbel$estimate["scale"]),
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Densidade Empírica", "Densidade Gumbel"), 
       col = c("blue", "red"), lwd = 2)

