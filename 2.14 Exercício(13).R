#2.14 Exercício 13
#vamos simular uma Frechét e fazer as mesma estiamtivas do exercicio 12
library(evir)

mu_sim <- 0   
sigma_sim <- 1  
xi_sim <- 0.4   

# Simular 2000 observações da dist. Frechét
set.seed(12)  
n <- 2000
sim_data <- rgev(n, xi = xi_sim, mu = mu_sim, sigma=sigma_sim)

periodos <- c(5, 10, 15, 25, 50, 75, 100, 120)
valores_xi <- numeric(length(periodos))
valores_mu <- numeric(length(periodos))
valores_sigma <- numeric(length(periodos))

for (i in seq_along(periodos)) {
  r <- periodos[i]
  
  # Ajustar o modelo GEV à simulação com blocos de tamanho r
  ajuste_gev <- tryCatch({
    gev(sim_data, block = r)
  }, error = function(e) {
    message(paste("Erro ao ajustar GEV para r =", r, ":", e$message))
    return(NULL)
  })
  
  # Extrair os parâmetros xi, mu, sigma, se o ajuste foi bem-sucedido
  if (!is.null(ajuste_gev)) {
    valores_xi[i] <- ajuste_gev$par.ests["xi"]
    valores_mu[i] <- ajuste_gev$par.ests["mu"]
    valores_sigma[i] <- ajuste_gev$par.ests["sigma"]
  } else {
    valores_xi[i] <- NA
    valores_mu[i] <- NA
    valores_sigma[i] <- NA
  }
}

resultados_simulacao <- data.frame(
  Período = periodos, 
  Xi = valores_xi, 
  Mu = valores_mu, 
  Sigma = valores_sigma
)
print(resultados_simulacao)
print(paste('Parâmetros verdadeiros: mu =', mu_sim, ', sigma =', sigma_sim, ', xi =', xi_sim))
# as estimativas de xi são relativamente boas, mas podemos notar que tanto sigma quanto mu são muito dependentes 
# do tamanho do bloco, de forma que blocos com periodos maiores tem maior média de extremos e maior dispersão( o que faz sentido...)

#vamos pegar os l-momentos
install.packages("lmom")
library(lmom)

samlmu(sim_data, nmom=4)
mean(sim_data)
sd(sim_data)
