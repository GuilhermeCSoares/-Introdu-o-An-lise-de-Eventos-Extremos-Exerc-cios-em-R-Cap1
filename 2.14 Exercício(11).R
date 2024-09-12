#2.14 Exercício 11
install.packages("evir")
library(evir)
#Os dados são a variação, não a log-variação como comumente é usado, para alterar é só trocar os dados.csv 
#por um com valores em log-variação
dados <- read.csv("\\Datapct.csv")
dados <- na.omit(dados)
periodos <- c(20,25,30,35,40,45,50,60,70,80,90,100,110,120)
resultados_list <- list()
for (col in 2:ncol(dados)) {
  serie_nome <- colnames(dados)[col]
  serie_dados <- dados[[col]]
  valores_xi <- numeric(length(periodos))
  
  for (i in seq_along(periodos)) {
    r <- periodos[i]
    ajuste_gev <- tryCatch({
      gev(serie_dados, block = r)
    }, error = function(e) {
      message(paste("Erro ao ajustar GEV para r =", r, ":", e$message))
      return(NULL)
    })

    if (!is.null(ajuste_gev)) {
      valores_xi[i] <- ajuste_gev$par.ests["xi"]
    } else {
      valores_xi[i] <- NA
    }
  }
  resultados_df <- data.frame(Período = periodos, Xi = valores_xi)
  resultados_list[[serie_nome]] <- resultados_df
  
  # Plotar xi vs. período para a série atual
  plot(resultados_df$Período, resultados_df$Xi, type = "b",
       xlab = "Período (r)", ylab = "Parâmetro xi",
       main = paste("Estimativa do Parâmetro xi vs. r", serie_nome),
       col = "blue", pch = 16)
}

#Grafico como o da figura 2.5
plot(gev(dados$AAPL, block = 120))
1
2
0

