#Exercício 4
#install.packages("quantmod")
library(quantmod)

symbol <- "PETR3.SA"
start_date <- "2015-01-01" # Data inicial
end_date <- Sys.Date()     # Data final
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)
data <- get(symbol)

# Calcular a variação log
log_variation <- diff(log(Cl(data)))
log_variation_series <- na.omit(log_variation)
head(log_variation_series)
hist(log_variation_series)
plot(density(log_variation_series))
alpha <- 0.01
VaR <- quantile(log_variation_series, probs = alpha)
VaR

library(evir)
number_extremes = length(log_variation_series)*0.05  #escolha meio arbitraria de u ("bateu" com o VaR empírico)
thresh <- findthresh(-log_variation_series,number_extremes) #botei negativo pra pegar as caudas da esquerda
extremes_serie <- log_variation[log_variation_series < -thresh]
extremes_serie <- na.omit(extremes_serie)
head(extremes_serie)
plot(density(extremes_serie))
length(extremes_serie)
length(log_variation_series)

## Vamos agora fitar uma GEV nos estremos
gpd_fit <- gpd(log_variation_series, nextremes = number_extremes)
gpd_fit$par.ests
params <- gpd_fit$par.ests
xi_gpd <- params[1]  # Parâmetro de forma
beta_gpd <- params[2]  # Parâmetro de escala
#mu =thresh
VaR_gpd <- -qgpd(0.99, mu = -thresh, xi = xi, beta = beta)
VaR_gpd

##Vamos fazer a mesma coisa pra uma GEV

gev_fit <- fevd(log_variation_series, type = "GEV", threshold = -thresh, method = "MLE")
params <- gev_fit$results$par
xi_gev <- params[3]  # Parâmetro de forma
sigma_gev <- params[2]  # Parâmetro de escala
mu_gev <- params[1] # Localização, que é o limiar

VaR_gev <- qgev(0.01, xi = xi_gev,mu = mu_gev, sigma = sigma_gev)
#########################
data <- data.frame(
  thresh = thresh,
  number_extremes = number_extremes,
  VaR = VaR,
  xi_gpd = xi_gpd,
  beta_gpd = beta_gpd,
  VaR_gpd = VaR_gpd,
  xi_gev = xi_gev,
  sigma_gev = sigma_gev,
  mu_gev = mu_gev,
  VaR_gev = VaR_gev
)

print(data)
