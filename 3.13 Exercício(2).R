#Exercício 2
#Dificuldade de fazer funcionar essa distribuição podada, mas de resto da pra ver o efeito da contaminação nos dados
# Carregar o pacote evir
library(evir)
xi_values <- c(-0.4, -0.1, 0.1, 0.4)
# Definir os parâmetros
n <- 500  # Tamanho da amostra original
phi <- 1  # Valor fixo de phi
extreme_values <- 5  # Número de valores de contaminação
percentile <- 0.95  # Percentil para o limiar u

# Função para gerar amostras e adicionar valores extremos
generate_samples_with_extremes <- function(xi, n, phi, extreme_values) {
  sample <- rgpd(n, xi = xi, mu = 0, beta = phi)
  extremes <- rgpd(n = extreme_values, xi = xi, mu = 0, beta = phi) * 10  # Multiplicar para garantir extremos
  sample_with_extremes <- c(sample, extremes)
  
  return(sample_with_extremes)
}

samples_with_extremes <- lapply(xi_values, generate_samples_with_extremes, n = n, phi = phi, extreme_values = extreme_values)
u_values_extreme <- sapply(samples_with_extremes, function(sample) quantile(sample, probs = percentile))

calc_excesses <- function(sample, u) {
  excesses <- sample[sample > u] - u
  return(excesses)
}
excesses_extreme <- lapply(1:length(samples_with_extremes), function(i) calc_excesses(samples_with_extremes[[i]], u_values_extreme[i]))

mean_excesses_extreme <- sapply(excesses_extreme, mean)
median_excesses_extreme <- sapply(excesses_extreme, median)

names(mean_excesses_extreme) <- paste0("Mean_XI_", xi_values)
names(median_excesses_extreme) <- paste0("Median_XI_", xi_values)

mean_excesses_extreme
median_excesses_extreme

e_u <- function(u, xi, phi) {
  (phi + xi * u) / (1 - xi)
}

u_values <- seq(0, 10, by = 0.1)

plot(u_values, e_u(u_values, xi_values[1], phi), type = "l", col = "blue", ylim = c(-2, 10), xlim = c(0, 5),
     xlab = "Limiar u", ylab = "Média dos excessos e(u)", main = "Média dos Excessos para diferentes valores de XI")
lines(u_values, e_u(u_values, xi_values[2], phi), col = "red")
lines(u_values, e_u(u_values, xi_values[3], phi), col = "green")
lines(u_values, e_u(u_values, xi_values[4], phi), col = "purple")

legend("topright", legend = paste("xi =", xi_values), col = c("blue", "red", "green", "purple"), lty = 1)


#tentando pegar a distribuição podada... (voltar aqui depois)
e_F_u_p <- function(u, xi, phi, p) {
  if (xi == 0) {
    quantile_u <- Inf  # Caso xi = 0, integre até infinito
  } else {
    quantile_u <- qgpd(p, xi = xi, beta = phi)
  }
  integral <- integrate(function(x) x * gpd_density(x, xi, phi), lower = u, upper = quantile_u)$value
  return(integral / p)
}

gpd_density <- function(x, xi, phi) {
  if (xi == 0) {
    return(dexp(x / phi, rate = 1 / phi))
  } else {
    return(dgpd(x, xi = xi, beta = phi))
  }
}

mean_podada <- sapply(1:length(xi_values), function(i) {
  u <- u_values_extreme[i]
  p <- percentile
  e_F_u_p(u, xi_values[i], phi, p)
})
names(mean_podada) <- paste0("Mean_Podada_XI_", xi_values)

mean_excesses_extreme
median_excesses_extreme
mean_podada