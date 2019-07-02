library(readr)
library(dplyr)
library(ggpubr)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

original <- read_csv("paises.csv")

#Convertendo para numerico
valores_df <- data.frame(lapply(original[5:ncol(original)], function(col) {
  as.numeric(gsub(",", ".", col))
}))

paises <- cbind(original[1:4], valores_df)

# Setando Pais como índice
rownames(paises) <- paises$Pais
paises$Pais <- NULL
rownames(paises)

summary(paises)
str(paises)
head(paises)
View(paises)

# Matriz de correlacao (removendo a coluna clima)
chart.Correlation(valores_df[, -11], histogram = TRUE)

#Correlacoes destacadas
chart.Correlation(select(valores_df, PIBperCapita, Fones, TaxaNascimento, Alfabetismo, Servico), histogram = TRUE)

#Analisar correlacao entre PIBPerCapta e Fones
#Analisar correlacao entre TacaNascimento e Alfabetismo
#Analisar correlacao entre Servico e Fones

#Obs: Alguns dos casos acima nao se enquandram na distribuicao normal
