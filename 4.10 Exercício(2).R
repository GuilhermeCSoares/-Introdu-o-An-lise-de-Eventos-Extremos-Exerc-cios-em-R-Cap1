# 4.10 Exercício 2
#A1(v)
A1 <- function(v, theta) {
  return(theta * v^2 + theta * v - 1)
}

# Gerar X ~ Exp(1)
set.seed(123)
n <- 1000  # Número de observações
X <- rexp(n, rate = 1)  # X segue uma distribuição exponencial com lambda = 1

# gerar Y | X = x de acordo com o modelo misto simétrico
generate_Y_given_X <- function(X, theta) {
  Y <- numeric(length(X))
  
  for (i in seq_along(X)) {
    x <- X[i]
    v <- x
    A1_value <- A1(v, theta)
    Y[i] <- A1_value + rnorm(1, mean = 0, sd = 0.5)  # Adicionar ruído normal
  }
  
  return(Y)
}

# Gerar Y | X para theta = 0, 0.8 e -0.8
theta_0 <- 0
Y_theta_0 <- generate_Y_given_X(X, theta_0)


theta_08 <- 0.8
Y_theta_08 <- generate_Y_given_X(X, theta_08)

theta_l08 <- -0.5
Y_theta_l08 <- generate_Y_given_X(X, theta_08)

# Plotar os resultados para visualização
par(mfrow = c(3, 1))
plot(X, Y_theta_l08, main = "Y | X para theta = -0.8", xlab = "X", ylab = "Y", col = "red", pch = 19)
plot(X, Y_theta_0, main = "Y | X para theta = 0", xlab = "X", ylab = "Y", col = "blue", pch = 19)
plot(X, Y_theta_08, main = "Y | X para theta = 0.8", xlab = "X", ylab = "Y", col = "green", pch = 19)
