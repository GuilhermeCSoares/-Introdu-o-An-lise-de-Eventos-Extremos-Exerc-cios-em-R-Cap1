# Exercício 5
# Instalar pacotes necessários
install.packages("quantmod")
install.packages("rugarch")
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
#deu 4.47df, com desvio de 0.41, vamos fitar 5 por via das duvidas

sigma_t <- sigma(garch_fit)  # Desvio padrão condicional ajustado
var_99_conditional <- qt(0.01, df=6) * sigma_t  # VaR condicional 99%
var_999_conditional <- qt(0.001, df=6) * sigma_t  # VaR condicional 99.9%
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

gpd <- gpd(std_residuals, nextremes = length(log_returns)*0.05)
plot(gpd)
1
3
4
2
