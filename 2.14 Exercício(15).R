#Exercico 15

library(evir)

# Definir parâmetros da distribuição GEV
xi_sim <- 0.4   # Parâmetro de forma
mu_sim <- 0     # Parâmetro de localização
sigma_sim <- 1  # Parâmetro de escala

# Simular dados GEV
n <- 2000
set.seed(123)
sim_data <- rgev(n, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)

# Calcular a densidade teórica da GEV
x_vals <- seq(min(sim_data), max(sim_data), length.out = 1000)
dens_teorica <- dgev(x_vals, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)

# Calcular a densidade empírica dos dados simulados
dens_empirica <- density(sim_data)

# Plotar densidade teórica e empírica
plot(dens_empirica, main = "Densidade Teórica e Empírica da GEV",
     xlab = "Valores", ylab = "Densidade", col = "blue", lwd = 2, ylim = c(0, max(dens_empirica$y, dens_teorica)))
lines(x_vals, dens_teorica, col = "red", lwd = 2)
legend("topright", legend = c("Densidade Empírica", "Densidade Teórica"),
       col = c("blue", "red"), lty = 1, lwd = 2)

ks_test <- ks.test(sim_data, "pgev", xi = xi_sim, mu = mu_sim, beta = sigma_sim)
print(ks_test)
#podemos ver que pelo p-valor(maior que 0.05) ele segue. o D mostra a maior distancia absoluta entre as distribuições

#qq-plot exponencial
exp_theoretical <- qgev(x_vals, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)
qqplot(exp_theoretical, sim_data, main = "QQ-Plot: GEV(0.4, 0, 1) vs Exponencial(1)",
       xlab = "Quantis teóricos (Exponencial(1))", ylab = "Quantis empíricos (GEV)",
       pch = 16, col = "blue")
abline(0, 1, col = "red" )


