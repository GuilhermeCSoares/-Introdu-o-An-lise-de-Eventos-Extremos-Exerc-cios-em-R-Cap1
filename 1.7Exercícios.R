##Exercicio 1:
##i)vamos gerar um random-walk com inovações normais
n <- 1000
inovacoes <- rnorm(n, mean = 0, sd = 1)
passeio_aleatorio <- numeric(n)
passeio_aleatorio[1] <- 0  # Valor inicial
for (i in 2:n) {
  passeio_aleatorio[i] <- passeio_aleatorio[i-1] + inovacoes[i]
}
plot(passeio_aleatorio, type = "l", main = "Passeio Aleatório com Inovações Normais", 
     xlab = "Tempo", ylab = "Valor")

##ii)vamos gerar um random-walk com inovações T
inovacoest <- rt(n,df = 5)
passeio_aleatoriot <- numeric(n)
passeio_aleatoriot[1] <- 0  # Valor inicial
for (i in 2:n) {
  passeio_aleatoriot[i] <- passeio_aleatoriot[i-1] + inovacoest[i]
}
plot(passeio_aleatoriot, type = "l", main = "Passeio Aleatório com Inovações T", 
     xlab = "Tempo", ylab = "Valor")

##Exercicio 2:
#Tirar a primeira diferença de cada uma das séries
# primeira diferença da série t
diff_t  = diff(passeio_aleatoriot)
plot(diff_t)
# primeira diferença da série normal
diff_n  = diff(passeio_aleatorio)
plot(diff_n)

##Exercicio 3:
#vamos obter preços de fechamentos de ações
# Carregar o pacote
library(quantmod) #install.packages('quantmod')
#vamos pegas as ações e botar em um df
acoes <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
getSymbols(acoes, src = "yahoo", from = "2019-01-01")
df <- data.frame(
  Date = index(PETR4.SA),
  PETR4 = Cl(PETR4.SA),
  VALE3 = Cl(VALE3.SA),
  ITUB4 = Cl(ITUB4.SA),
  BBDC4 = Cl(BBDC4.SA),
  ABEV3 = Cl(ABEV3.SA)
)
df <- na.omit(df)
#pegar a log variação
log_var_df <- data.frame(
  Date = df$Date[-1],
  PETR4 = diff(log(df$PETR4)),
  VALE3 = diff(log(df$VALE3)),
  ITUB4 = diff(log(df$ITUB4)),
  BBDC4 = diff(log(df$BBDC4)),
  ABEV3 = diff(log(df$ABEV3))
)
log_var_df <- log_var_df[-1, ]
#head(log_var_df)
plot(log_var_df$Date, log_var_df$PETR4, type = "l", 
     main = "Log Variação do Preço de Fechamento - PETR4", 
     xlab = "Data", ylab = "Log Variação")

##(i) vamos agora pegar a média e desvio amostral, skew, curtosis e uma autocorrelação (1lag)
##para cada uma das séries, faremos também uma serie dos log retornos ao quadrado e
##estimaremos a autocorrelação

library(moments) # install.packeges('moments')

for (i in 2:ncol(log_var_df)) {
  coluna <- log_var_df[[i]]  # Extraindo a coluna
  
  # Calcular e exibir as estatísticas
  cat("Coluna:", colnames(log_var_df)[i], "\n")
  cat("Média:", mean(coluna), "\n")
  cat("Desvio Padrão:", sd(coluna), "\n")
  cat("Assimetria (Skewness):", skewness(coluna), "\n")
  cat("Curtose - 3:", kurtosis(coluna), "\n")
  
  cat("ACF da série:\n")
  print(acf(coluna, lag.max = 10, plot = FALSE))  
  cat("ACF da série ao quadrado:\n")
  print(acf(coluna^2, lag.max = 10, plot = FALSE))
  
  cat("\n---------------------------------\n")
}
#vamos dividir em aprox 5 partes e calcular as estatisticas
 x1 <- log_var_df$PETR4[1:280]
 x2 <- log_var_df$PETR4[281:560]
 x3 <- log_var_df$PETR4[561:840]
 x4 <- log_var_df$PETR4[841:1120]
 x5 <- log_var_df$PETR4[1120:1400]
 print(paste('Médias:', mean(x1), mean(x2), mean(x3), mean(x4), mean(x5)))
 print(paste('Médias:', sd(x1), sd(x2), sd(x3), sd(x4), sd(x5)))
 print(paste('Médias:', skewness(x1), skewness(x2), skewness(x3), skewness(x4), skewness(x5)))
 print(paste('Médias:', kurtosis(x1), kurtosis(x2), kurtosis(x3), kurtosis(x4), kurtosis(x5)))
 
#vamos repetir esse teste com um df sem estar em log:
 diff_df <- data.frame(
   Date = df$Date[-1],
   PETR4 = diff(df$PETR4),
   VALE3 = diff(df$VALE3),
   ITUB4 = diff(df$ITUB4),
   BBDC4 = diff(df$BBDC4),
   ABEV3 = diff(df$ABEV3)
 )
 x1 <- diff_df$PETR4[1:280]
 x2 <- diff_df$PETR4[281:560]
 x3 <- diff_df$PETR4[561:840]
 x4 <- diff_df$PETR4[841:1120]
 x5 <- diff_df$PETR4[1120:1400]
 print(paste('Médias:', mean(x1), mean(x2), mean(x3), mean(x4), mean(x5)))
 print(paste('Médias:', sd(x1), sd(x2), sd(x3), sd(x4), sd(x5)))
 print(paste('Médias:', skewness(x1), skewness(x2), skewness(x3), skewness(x4), skewness(x5)))
 print(paste('Médias:', kurtosis(x1), kurtosis(x2), kurtosis(x3), kurtosis(x4), kurtosis(x5)))
 
 ##Exercicio 4
 #para cada ano dos 5 anos, calcule a volatilidade, tanto da serie em logdiff quanto diff
 #vamos usar s= desvio padrão, como estimativa de escala,  
 (sd(diff_df$PETR4)*sqrt(264)/5)/(sd(diff_df$PETR4[1:281])*sqrt(264))
 (sd(diff_df$PETR4)*sqrt(264)/5)/(sd(diff_df$PETR4[281:560])*sqrt(264))
 (sd(diff_df$PETR4)*sqrt(264)/5)/(sd(diff_df$PETR4[561:840])*sqrt(264))
 (sd(diff_df$PETR4)*sqrt(264)/5)/(sd(diff_df$PETR4[841:1120])*sqrt(264))
 (sd(diff_df$PETR4)*sqrt(264)/5)/(sd(diff_df$PETR4[1120:1400])*sqrt(264))
 #vamos fazer a mesma coisa com as log_diff
 (sd(log_var_df$PETR4)*sqrt(264)/5)/(sd(log_var_df$PETR4[1:281])*sqrt(264))
 (sd(log_var_df$PETR4)*sqrt(264)/5)/(sd(log_var_df$PETR4[281:560])*sqrt(264))
 (sd(log_var_df$PETR4)*sqrt(264)/5)/(sd(log_var_df$PETR4[561:840])*sqrt(264))
 (sd(log_var_df$PETR4)*sqrt(264)/5)/(sd(log_var_df$PETR4[841:1120])*sqrt(264))
 (sd(log_var_df$PETR4)*sqrt(264)/5)/(sd(log_var_df$PETR4[1120:1400])*sqrt(264))
 
##Exercício 5
 for (i in 2:ncol(log_var_df)) {
   coluna <- log_var_df[[i]]  
   cat("Coluna:", colnames(log_var_df)[i], "\n")
   cat("Média:", mean(coluna), "\n")
   cat("Desvio Padrão:", sd(coluna), "\n")
   
   cat("Mediana Amostral:", median(coluna), "\n")
   cat("MAD (Desvio Absoluto da Mediana):", mad(coluna), "\n")
   cat("\n---------------------------------\n")
 }
 for (i in 2:ncol(diff_df)) {
   coluna <- diff_df[[i]]  
   cat("Coluna:", colnames(diff_df)[i], "\n")
   cat("Média:", mean(coluna), "\n")
   cat("Desvio Padrão:", sd(coluna), "\n")
   
   cat("Mediana Amostral:", median(coluna), "\n")
   cat("MAD (Desvio Absoluto da Mediana):", mad(coluna), "\n")
   cat("\n---------------------------------\n")
 }
 #vamos fazer as mesmas estatisticas, mas agora dividindo ambos os dataframes em 5 partes:
 # Número de partes que deseja dividir
 n_partes <- 5
 
 # Dividindo o dataframe em 5 partes com aproximadamente o mesmo número de linhas
 split_dataframes <- split(log_var_df, cut(1:nrow(log_var_df), n_partes, labels = FALSE))
 
 # Iterar sobre cada subconjunto de log_var_df
 for (parte in 1:length(split_dataframes)) {
   cat("\n==== Parte", parte, "====\n")
   log_var_parte <- split_dataframes[[parte]]
   
   for (i in 2:ncol(log_var_parte)) {
     coluna <- log_var_parte[[i]]  
     cat("Coluna:", colnames(log_var_parte)[i], "\n")
     cat("Média:", mean(coluna), "\n")
     cat("Desvio Padrão:", sd(coluna), "\n")
     
     cat("Mediana Amostral:", median(coluna), "\n")
     cat("MAD (Desvio Absoluto da Mediana):", mad(coluna), "\n")
     cat("\n---------------------------------\n")
   }
 }
 
 # Fazer o mesmo para diff_df, dividindo em 5 partes
 # aproximadamente 1 pra cada ano
 
 # Dividindo o dataframe log_var_df em 5 partes aproximadamente iguais
 n_partes <- 5
 split_dataframes <- split(log_var_df, cut(1:nrow(log_var_df), n_partes, labels = FALSE))
 
 for (i in 2:ncol(log_var_df)) {
   cat("\n==== Tabela para ação:", colnames(log_var_df)[i], "====\n")

   resultados_acao <- list(
     Media = numeric(),
     Desvio_Padrao = numeric(),
     Mediana = numeric(),
     MAD = numeric()
   )
   
   for (parte in 1:length(split_dataframes)) {
     coluna_parte <- split_dataframes[[parte]][[i]]
     resultados_acao$Media <- c(resultados_acao$Media, mean(coluna_parte))
     resultados_acao$Desvio_Padrao <- c(resultados_acao$Desvio_Padrao, sd(coluna_parte))
     resultados_acao$Mediana <- c(resultados_acao$Mediana, median(coluna_parte))
     resultados_acao$MAD <- c(resultados_acao$MAD, mad(coluna_parte))
   }
   tabela_resultados_acao <- data.frame(
     Métrica = c("Média", "Desvio Padrão", "Mediana", "MAD"),
     Parte_1 = c(resultados_acao$Media[1], resultados_acao$Desvio_Padrao[1], resultados_acao$Mediana[1], resultados_acao$MAD[1]),
     Parte_2 = c(resultados_acao$Media[2], resultados_acao$Desvio_Padrao[2], resultados_acao$Mediana[2], resultados_acao$MAD[2]),
     Parte_3 = c(resultados_acao$Media[3], resultados_acao$Desvio_Padrao[3], resultados_acao$Mediana[3], resultados_acao$MAD[3]),
     Parte_4 = c(resultados_acao$Media[4], resultados_acao$Desvio_Padrao[4], resultados_acao$Mediana[4], resultados_acao$MAD[4]),
     Parte_5 = c(resultados_acao$Media[5], resultados_acao$Desvio_Padrao[5], resultados_acao$Mediana[5], resultados_acao$MAD[5])
   )
   print(tabela_resultados_acao)
   cat("\n---------------------------------\n")
 }
 #Fazendo a mesma coisa para os dados sem log:
 n_partes <- 5
 split_dataframes <- split(diff_df, cut(1:nrow(diff_df), n_partes, labels = FALSE))
 
 for (i in 2:ncol(diff_df)) {
   cat("\n==== Tabela para ação:", colnames(diff_df)[i], "====\n")
   
   resultados_acao <- list(
     Media = numeric(),
     Desvio_Padrao = numeric(),
     Mediana = numeric(),
     MAD = numeric()
   )
   
   for (parte in 1:length(split_dataframes)) {
     coluna_parte <- split_dataframes[[parte]][[i]]
     resultados_acao$Media <- c(resultados_acao$Media, mean(coluna_parte))
     resultados_acao$Desvio_Padrao <- c(resultados_acao$Desvio_Padrao, sd(coluna_parte))
     resultados_acao$Mediana <- c(resultados_acao$Mediana, median(coluna_parte))
     resultados_acao$MAD <- c(resultados_acao$MAD, mad(coluna_parte))
   }
   tabela_resultados_acao <- data.frame(
     Métrica = c("Média", "Desvio Padrão", "Mediana", "MAD"),
     Parte_1 = c(resultados_acao$Media[1], resultados_acao$Desvio_Padrao[1], resultados_acao$Mediana[1], resultados_acao$MAD[1]),
     Parte_2 = c(resultados_acao$Media[2], resultados_acao$Desvio_Padrao[2], resultados_acao$Mediana[2], resultados_acao$MAD[2]),
     Parte_3 = c(resultados_acao$Media[3], resultados_acao$Desvio_Padrao[3], resultados_acao$Mediana[3], resultados_acao$MAD[3]),
     Parte_4 = c(resultados_acao$Media[4], resultados_acao$Desvio_Padrao[4], resultados_acao$Mediana[4], resultados_acao$MAD[4]),
     Parte_5 = c(resultados_acao$Media[5], resultados_acao$Desvio_Padrao[5], resultados_acao$Mediana[5], resultados_acao$MAD[5])
   )
   print(tabela_resultados_acao)
   cat("\n---------------------------------\n")
 }
 
 #Exercício 6, plotar ACF 12 lags do log returno e log retorno^2:
 for (i in 2:ncol(log_var_df)) {
   coluna <- log_var_df[[i]]  
   nome_coluna <- colnames(log_var_df)[i]
   cat("Coluna:", colnames(log_var_df)[i], "\n")
   cat("ACF da série:\n")
   print(acf(coluna, lag.max = 12, plot = TRUE,main = paste("ACF da Série -", nome_coluna)))  
   cat("ACF da série ao quadrado:\n")
   print(acf(coluna^2, lag.max = 12, plot = TRUE,main = paste("ACF da Série -", nome_coluna)))
   cat("\n---------------------------------\n")
 }
 
##Exercício 7
#vamos testar se tem volatilidade diferente dependendo do dia da semana
diff_df$Data <- as.Date(diff_df$Data)
diff_df$Dia_da_Semana <- weekdays(diff_df$Data)

calcula_metricas_por_dia <- function(df, dia) {
  df_dia <- df[df$Dia_da_Semana == dia, ]
  
  resultados <- list(
    Desvio_Padrao = numeric(),
    MAD = numeric()
  )
  
  for (i in 2:ncol(df_dia) - 1) {  
    coluna <- df_dia[[i]]
    
    resultados$Desvio_Padrao <- c(resultados$Desvio_Padrao, sd(coluna, na.rm = TRUE))
    resultados$MAD <- c(resultados$MAD, mad(coluna, na.rm = TRUE))
  }
  
  return(data.frame(
    Métrica = c("Desvio Padrão", "MAD"),
    Valores = c(resultados$Desvio_Padrao, resultados$MAD)
  ))
}
dias_semana <- unique(diff_df$Dia_da_Semana)
#calculando as metricas pra cada dia
for (dia in dias_semana) {
  cat("\n==== Métricas para:", dia, "====\n")
  tabela_metricas <- calcula_metricas_por_dia(diff_df, dia)
  print(tabela_metricas)
  cat("\n---------------------------------\n")
}
