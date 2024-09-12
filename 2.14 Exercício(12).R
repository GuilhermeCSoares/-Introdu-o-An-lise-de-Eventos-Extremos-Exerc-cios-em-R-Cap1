#2.14 Exercício 12
#Vamos ver como se comportam as variações dos maximos para diferentes períodos
#a gente vai usar extremos mensais, quinzenais, mensais, semestrais e anuais
#ou seja, r = 5,10,20,120,256 .  Aproximações tendo em vista que os dados só tem dias úteis

dados <- read.csv("\\Datapct.csv")
dados <- na.omit(dados)
periodos <- c(5,10,20,120,256)
resultados_list <- list()
# Inicializar uma lista para armazenar os resultados finais
resultados_finais <- list()

# Iterar sobre as colunas (exceto a primeira) do dataframe
for (col in 2:ncol(dados)) {
  serie_nome <- colnames(dados)[col]
  serie_dados <- dados[[col]]
  
  # Inicializar vetores para armazenar os parâmetros xi, mu, sigma
  valores_xi <- numeric(length(periodos))
  valores_mu <- numeric(length(periodos))
  valores_sigma <- numeric(length(periodos))
  
  # Calcular os parâmetros para diferentes valores de r
  for (i in seq_along(periodos)) {
    r <- periodos[i]
    
    # Ajustar o modelo GEV à série com blocos de tamanho r
    ajuste_gev <- tryCatch({
      gev(serie_dados, block = r)
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
  
  # Criar um dataframe para armazenar os parâmetros da série atual
  resultados_serie <- data.frame(
    Série = rep(serie_nome, length(periodos)),
    Período = periodos, 
    Xi = valores_xi, 
    Mu = valores_mu, 
    Sigma = valores_sigma
  )
  
  # Adicionar os resultados ao dataframe final
  resultados_finais[[serie_nome]] <- resultados_serie
}

# Combinar os resultados de todas as séries em um único dataframe
tabela_resultados <- do.call(rbind, resultados_finais)

# Visualizar a tabela
print(tabela_resultados)

      