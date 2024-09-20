# Exercício 6
# Vamos rodar o cod do exercício 5 e fazer analíse dos resíduos como na seção 3.11
#install.packages("quantmod")
#install.packages("rugarch")
library(quantmod)
library(rugarch)
library(evir)

getSymbols("PETR4.SA", src = "yahoo", from = "2010-01-01")
log_returns <- diff(log(Cl(PETR4.SA))) * 100  # Log variação em porcentagem
log_returns <- na.omit(log_returns)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)
garch_fit <- ugarchfit(spec, data = log_returns)
std_residuals <- residuals(garch_fit, standardize = TRUE)
plot(std_residuals, main = "Resíduos Padronizados", type = "l")
hist(std_residuals)
plot(density(std_residuals))

#Vemos que temos caudas longas na distribuiçãos dos std_residuals, podemos refitar o garch com outras configurações,
#aqui vou usar o garch normal, e considerar que os residuos tem dist. t, vamos estimar os df's dessa t
library(MASS)
fit_t <- fitdistr(std_residuals, "t")
print(fit_t)
#deu 4.7df, com desvio de ~0.36, vamos fitar 5 por via das duvidas

sigma_t <- sigma(garch_fit)  # Desvio padrão condicional ajustado
var_99_conditional <- qt(0.01, df=4.7) * sigma_t  # VaR condicional 99%
var_999_conditional <- qt(0.001, df=4.7) * sigma_t  # VaR condicional 99.9%
plot(log_returns, type = "l", col = "blue", main = "Log-retornos com VaR Condicional", ylab = "Retornos (%)", xlab = "Tempo")
lines(var_99_conditional, col = "red", lty = 2, lwd = 2)  # VaR 99% condicional
lines(var_999_conditional, col = "green", lty = 2, lwd = 2)  # VaR 99.9% condicional

legend("topright", legend = c("Log-retornos", "VaR 99% Condicional", "VaR 99.9% Condicional"),
       col = c("blue", "red", "green"), lty = 1, cex = 0.8)

menor_99 <- log_returns[log_returns < var_99_conditional]
length(menor_99)
menor_999 <- log_returns[log_returns < var_999_conditional]
length(menor_999)

print(paste('Menor que alpha=99%:', round(length(menor_99) / length(log_returns) * 100, 2), '% dos dias'))
print(paste('Menor que alpha=99.9%:', round(length(menor_999) / length(log_returns) * 100, 2), '% dos dias'))

#Vamos fazer uma GPD nos valores extremos,menor_99

gpd99 <- gpd(log_returns, nextremes = length(menor_99))
plot(gpd99)
1  #escolher tipo de plot (1...4)
0
gpd999 <- gpd(log_returns, nextremes = length(menor_999)+1)
plot(gpd999)
1
0

##### vamos plotar as densidades dos residuos
dens_empirical <- density(std_residuals)
df_t4 <- 4
df_t5 <- 5
df_t6 <- 6
x_vals <- seq(min(std_residuals), max(std_residuals), length.out = 100)
dens_normal <- dnorm(x_vals, mean = 0, sd = 1)
dens_t4 <- dt(x_vals, df = df_t4)
dens_t5 <- dt(x_vals, df = df_t5)
dens_t6 <- dt(x_vals, df = df_t6)
plot(dens_empirical, col = "black", lwd = 2, main = "Densidades: Empírica, Normal e t-Student",
     xlab = "Resíduos Padronizados", ylab = "Densidade")
lines(x_vals, dens_normal, col = "blue", lwd = 2, lty = 2)
lines(x_vals, dens_t4, col = "red", lwd = 2, lty = 3)
lines(x_vals, dens_t5, col = "green", lwd = 2, lty = 4)
lines(x_vals, dens_t6, col = "purple", lwd = 2, lty = 5)
legend("topright", legend = c("Densidade Empírica", "Normal", "t-Student df=4", "t-Student df=5", "t-Student df=6"),
       col = c("black", "blue", "red", "green", "purple"), lty = c(1, 2, 3, 4, 5), lwd = 2)
