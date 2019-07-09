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

# Boxplot de algumas variávies relevantes (identificando a existencia de outliers)
ggboxplot(paises$Alfabetismo, width = 0.5)
ggboxplot(paises$TaxaNascimento, width = 0.5)
ggboxplot(paises$TaxaMortalidade, width = 0.5)
ggboxplot(paises$MortalidadeInf, width = 0.5)
ggboxplot(paises$PIBperCapita, width = 0.5)

# Top 5 paises com mortalidade infantil mais altas
paises %>% 
  tibble::rownames_to_column(var = "NomePais") %>%
  select(NomePais, MortalidadeInf) %>%
  arrange(desc(MortalidadeInf)) %>%
  head(5) %>%
  ggbarplot(x="NomePais", y="MortalidadeInf", color = "NomePais")


# Matriz de correlacao (removendo a coluna clima)
chart.Correlation(valores_df[, -11], histogram = TRUE)

#Correlacoes destacadas
chart.Correlation(select(valores_df, PIBperCapita, Fones, TaxaNascimento, Alfabetismo, Servico),
                  histogram = TRUE)

#Analisar correlacao entre PIBPerCapta e Fones
corr_pibpercapta_fones <- cor.test(paises$PIBperCapita, paises$Fones, method = "pearson")
corr_pibpercapta_fones

#Analisar correlacao entre TaxaNascimento e Alfabetismo
corr_txnasc_alfab <- cor.test(paises$TaxaNascimento, paises$Alfabetismo, method = "pearson")
corr_txnasc_alfab

#Analisar correlacao entre Servico e Fones
corr_serv_fones <- cor.test(paises$Servico, paises$Fones, method = "pearson")
corr_serv_fones

# Conclusoes
# Foi possivel já na análise exploratória descobrir correlaçoes entre variaveis como a PIBperCapita 
# e telefones, a taxa de mortalidade com o alfabetismo (inversamente correlacionados) 
# e tambem o servico e telefones.
# Alem disso, pode-se observar quais as variaveis estao de acordo (ou relativamente proximos) da distribuicao normal.
# Tambem, nota-se que com o boxplot conseguimos idenficiar alguns outliers.
