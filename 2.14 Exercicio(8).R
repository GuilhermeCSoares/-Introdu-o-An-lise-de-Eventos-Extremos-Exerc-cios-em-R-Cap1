library(evd)
##Exercicio 8
#Vamos calcular a função empirica, e o log do complemento da função empírica
# e plotar um gráfico
set.seed(123)
n <- 1000
xi <- 0.5 # parametro da cauda pesada Csi
data <- rexp(n , xi)

Fn <- ecdf(data)

x_vals <- sort(data)
Fn_val <- Fn(x_vals)
log_1_Fn <- log(1 - Fn_val)

plot(log(x_vals), log_1_Fn)
 