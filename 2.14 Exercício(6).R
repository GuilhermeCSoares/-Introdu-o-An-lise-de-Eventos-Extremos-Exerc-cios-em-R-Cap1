#2.14 Exercicio 6
#Basicamente vamos refazer o grafico 2.3 da pag 42, com base no codigo dos exercicios 3e4
library(evd)
# Função para calcular o máximo, normalizar, ajustar a Gumbel e fazer os plots
gumbel_analysis_duplo <- function(n, x) {
  M_n <- sapply(seq(1, length(x), by = n), function(i) max(x[i:min(i + n - 1, length(x))]))
  Mn_n <- (M_n - log(n)) / 1
  fit_gumbel <- fgev(Mn_n)

    dgumbel <- function(x, mu, beta) {
    z <- (x - mu) / beta
    (1 / beta) * exp(-(z + exp(-z)))
  }
  
  dens_empirica <- density(Mn_n)
  dens_teorica <- dgumbel(dens_empirica$x, mu = fit_gumbel$estimate["loc"], beta = fit_gumbel$estimate["scale"])
  diff_densidades <- dens_empirica$y - dens_teorica
  
  par(mfrow = c(1, 2))  
  plot(dens_empirica, col = "blue", lwd = 2, main = paste("Densidades (n =", n, ")"),
       xlab = "Máximos Normalizados", ylab = "Densidade", ylim = c(0, max(dens_empirica$y, dens_teorica)))
  lines(dens_empirica$x, dens_teorica, col = "red", lwd = 2)
  legend("topright", legend = c("Densidade Empírica", "Densidade Gumbel"), 
         col = c("blue", "red"), lwd = 2)
  
  plot(dens_empirica$x, diff_densidades, type = "l", col = "purple", lwd = 2,
       main = "Diferença entre Densidades",
       xlab = "Máximos Normalizados", ylab = "Diferença (Empírica - Gumbel)")
  abline(h = 0, col = "black", lwd = 1, lty = 2)
}

# Gerando amostra aleatória da distribuição exp(1) com n = 1000
x <- rexp(1000, 1)

# Plot para n = 3
gumbel_analysis_duplo(3, x)

# Plot para n = 5
gumbel_analysis_duplo(5, x)

# Plot para n = 10
gumbel_analysis_duplo(10, x)
