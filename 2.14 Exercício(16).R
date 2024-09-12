# Exercício 16
library(ismev)

# Definir os parâmetros da GEV simulada
xi_sim <- 0.4    
mu_sim <- 0      
sigma_sim <- 1   
n <- 2000        
set.seed(123)
sim_data <- rgev(n, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)

pgev(-1, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)

valores_r <- c(1,2,3,4,5,6,7,8,9,10,15,20,25,30)
prob_extremos <- numeric(length(valores_r))

# Calcular a probabilidade de observar ao menos um valor extremo >= r para cada r
for (i in seq_along(valores_r)) {
  r <- valores_r[i]
  fn <- pgev(r, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)
  prob_extremos[i] <- 1 - fn
}

resultados_df <- data.frame(Valor = valores_r, Probabilidade = prob_extremos)
print(resultados_df)
plot(resultados_df$Valor, resultados_df$Probabilidade, type = "b",
     xlab = "Valor (r)", ylab = "Probabilidade de valor extremo >= r",
     main = "Probabilidade de Observar Valor Extremo ≥ r",
     col = "blue", pch = 16)

tempo_medio <- numeric(length(valores_r))

# Calcular a esperança do tempo médio para cada valor de k
for (i in seq_along(valores_r)) {
  k <- valores_r[i]
  F_k <- pgev(k, xi = xi_sim, mu = mu_sim, sigma = sigma_sim)
  tempo_medio[i] <- 1 / (1 - F_k)
}

resultados_df <- data.frame(Valor_k = valores_r, Tempo_Medio = tempo_medio)
print(resultados_df)
plot(resultados_df$Tempo_Medio,resultados_df$Valor_k, type = "b",
     xlab = "Valor de k", ylab = "Tempo médio esperado até valor ≥ k",
     main = "Esperança do Tempo Médio até Valor Extremo ≥ k",
     col = "blue", pch = 16)
