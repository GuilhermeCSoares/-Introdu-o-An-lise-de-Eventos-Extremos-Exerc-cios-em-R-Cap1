#Exercício 3
library(quantmod)

# Baixar os dados históricos dos índices do Brasil (Bovespa) e Argentina (Merval)
getSymbols("^BVSP", src = "yahoo", from = "2010-01-01")
getSymbols("^MERV", src = "yahoo", from = "2010-01-01")

bvsp_close <- Cl(BVSP)  
merv_close <- Cl(MERV)

log_ret_bvsp <- diff(log(bvsp_close))  # Log-retorno do Bovespa
log_ret_merv <- diff(log(merv_close))  # Log-retorno do Merval

log_ret_bvsp <- na.omit(log_ret_bvsp)
log_ret_merv <- na.omit(log_ret_merv)

par(mfrow = c(2, 1))  # Dois gráficos em uma coluna
plot(log_ret_bvsp, main = "Log-Retornos do Bovespa", col = "blue")
plot(log_ret_merv, main = "Log-Retornos do Merval", col = "red")

# Função para calcular máximos e mínimos mensais
calculate_monthly_max_min <- function(series_monthly) {
  monthly_max <- apply.monthly(series_monthly, max)
  monthly_min <- apply.monthly(series_monthly, min)
  return(list(max = monthly_max, min = monthly_min))
}

# Calcular máximos e mínimos mensais para Bovespa e Merval
bvsp_monthly_max_min <- calculate_monthly_max_min(log_ret_bvsp)
merv_monthly_max_min <- calculate_monthly_max_min(log_ret_merv)

# Dividir a série de máximos e mínimos em 5 pedaços
divide_into_parts <- function(series, n_parts) {
  len <- length(series)
  part_size <- floor(len / n_parts)
  parts_list <- list()
  for (i in 1:n_parts) {
    start <- (i - 1) * part_size + 1
    end <- ifelse(i == n_parts, len, i * part_size)
    
    # Selecionar o pedaço da série
    part <- series[start:end]
    parts_list[[i]] <- part
  }
  return(parts_list)
}

# Dividir as séries de máximos e mínimos em 5 pedaços
n_parts <- 5
bvsp_max_parts <- divide_into_parts(bvsp_monthly_max_min$max, n_parts)
bvsp_min_parts <- divide_into_parts(bvsp_monthly_max_min$min, n_parts)
# Merval - Dividir séries de máximos e mínimos
merv_max_parts <- divide_into_parts(merv_monthly_max_min$max, n_parts)
merv_min_parts <- divide_into_parts(merv_monthly_max_min$min, n_parts)

bvsp_max_parts
bvsp_min_parts
merv_max_parts
merv_min_parts

combined_matrix <- lapply(1:length(bvsp_max_parts), function(i) {
  cbind(bvsp_max_parts[[i]], merv_max_parts[[i]])
})
# Remover NA
cleaned_matrices <- lapply(combined_matrix, function(mat) {
  na.omit(mat)  # Remove linhas com NA
})
for (i in 1:length(cleaned_matrices)) {
  cat("Parte", i, "\n")
  print(cleaned_matrices[[i]])
}

library(evd)
#vamos pegar os maximos que foram divididos em 5 grupos e plotar a função de dependencia 
## MODELOS BIVARIADOS NÃO PARAMETRICOS
pp <- "pickands"; cc <- "cfg"; tdo <- 'tdo'; pot <- 'pot'
for (i in 1:5){
titulo <- paste("Plot mod não paramétrico, parte:", i)
abvnonpar(data = cleaned_matrices[[i]], epmar = TRUE, method = pp, plot = TRUE, lty = 3, main = titulo)
abvnonpar(data = cleaned_matrices[[i]], epmar = TRUE, method = pp, add = TRUE, madj = 1, lty = 2)
abvnonpar(data = cleaned_matrices[[i]], epmar = TRUE, method = pp, add = TRUE, madj = 2, lty = 4)
abvnonpar(data = cleaned_matrices[[i]], epmar = TRUE, method = cc, add = TRUE, lty = 1)
}
## MODELOS BIVARIADOS PARAMETRICOS
for (i in 1:5){
alog <- fbvevd(cleaned_matrices[[i]], asy1 = 1, model = "alog", std.err = FALSE) #log assimetrico?
log <- fbvevd(cleaned_matrices[[i]], model = "log", std.err = FALSE) #log simetrico
bilog <- fbvevd(cleaned_matrices[[i]], model = "bilog", std.err = FALSE)
ct <- fbvevd(cleaned_matrices[[i]], model = "ct", std.err = FALSE)
amix <- fbvevd(cleaned_matrices[[i]], model = "amix", std.err = FALSE)
titulo <- paste("Plot mod paramétrico, parte", i)
ylim_range <- c(0.7, 1)
plot(alog, which = 4, nplty = 3, ylim = ylim_range, main = titulo)
plot(log, which = 4, nplty = 3, lty = 4, add = TRUE)
plot(bilog, which = 4, nplty = 3, lty = 4, add = TRUE)
plot(ct, which = 4, nplty = 3, lty = 4, add = TRUE)
plot(amix, which = 4, nplty = 3, lty = 4, add = TRUE)
}
