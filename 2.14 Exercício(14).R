#Exercício 14
#vamos começar gerando a série de retorno simuladas de um Frechét
library(evir)
install.packages("ismev")
library(ismev)
#parametros da dist
mu_sim <- 0   
sigma_sim <- 1  
xi_sim <- 0.4   

# Simular 2000 observações da dist. Frechét
set.seed(123)  
n <- 2000
dadossim <- rgev(n, xi = xi_sim, mu = mu_sim, sigma=sigma_sim)
n_bloco <- 20 #numero do n, bloco de mínimo
dados_sim <- sapply(seq(1, length(dadossim), by = n_bloco), function(i) min(dadossim[i:min(i + n_bloco - 1, length(dadossim))]))

ajustar_gev <- function(dados) {
  ajuste <- gev.fit(dados)
  return(ajuste$mle)  # Maximum Likelihood Estimates (MLE) dos parâmetros xi, mu e sigma
}

n_boot <- 1000
# Armazenar os parâmetros ajustados para cada amostra bootstrap
parametros_bootstrap <- matrix(NA, nrow = n_boot, ncol = 3)

# Realizar o bootstrap
for (i in 1:n_boot) {
  dados_boot <- sample(dados_sim, replace = TRUE)
  parametros_bootstrap[i, ] <- ajustar_gev(dados_boot)
}

conf_level <- 0.95  # Nível de confiança 95%
alpha <- (1 - conf_level) / 2
intervalos_ic <- apply(parametros_bootstrap, 2, quantile, probs = c(alpha, 1 - alpha))

print("Intervalos de confiança para xi, mu e sigma (alpha%), feitos via bootstrap:")
print(intervalos_ic)
