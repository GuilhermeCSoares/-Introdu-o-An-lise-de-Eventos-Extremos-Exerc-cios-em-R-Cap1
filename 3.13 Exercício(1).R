##Exercício 1
install.packages("evir")
library(evir)

n <- 500  # Tamanho da amostra
phi <- 1  # Parâmetro de escala

# Parâmetros Xi
xi_values <- c(-0.4, -0.1, 0.1, 0.4)
# Gerar amostras para cada valor de XI
set.seed(123) 
samples <- lapply(xi_values, function(xi) rgpd(n, xi = xi, mu = 0, beta = phi))
names(samples) <- paste0("Sample_XI_", xi_values)

plot(samples$`Sample_XI_-0.4`)
plot(samples$Sample_XI_0.4)

#vamos definir u, limmiar de 95%
percentile <- 0.95

calc_threshold_u <- function(sample, percentile) {
  quantile(sample, probs = percentile)
}

u_values <- sapply(samples, calc_threshold_u, percentile = percentile)
u_values

#Agora vamos calcular a média e a mediana dos excessos de cada série

calc_excesses <- function(sample, u) {
  excesses <- sample[sample > u] - u  # Apenas os valores acima de u
  return(excesses)
}
excesses <- lapply(1:length(samples), function(i) calc_excesses(samples[[i]], u_values[i]))
mean_excesses <- sapply(excesses, mean)
median_excesses <- sapply(excesses, median)
names(mean_excesses) <- paste0("Mean_XI_", xi_values)
names(median_excesses) <- paste0("Median_XI_", xi_values)

mean_excesses
median_excesses

##vamos plotar a média pela função fornecida na pagina 85
#coloquei as duas figuras no mesmo grafico, as com xi>0 e xi<0
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
