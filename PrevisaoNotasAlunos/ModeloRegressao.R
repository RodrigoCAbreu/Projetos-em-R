# Regressão Linear

# Definição do Problema: Prever as notas dos alunos com base em diversas métricas

# Carregando o dataset
df <- read.csv2('estudantes.csv')

# Explorando os dados
View(df)
summary(df)
str(df)
any(is.na(df))

library(dplyr)

#Obtendo apenas colunas numéricas

colunas_numericas <- sapply(df, is.numeric)
colunas_numericas

#Filtrando as colunas numéricas para correlação

data_cor <- cor(df[,colunas_numericas])
data_cor

# Pacotes para visualizar a análise de correlação
installed.packages("corrgram")
install.packages("corrplot")

library(corrplot)
library(corrgram)

# Criando um corrplot
corrplot(data_cor, method = 'color')

# Criando um histograma
library(ggplot2)
library(ggthemes)

ggplot(df, aes(x = G3)) + 
  geom_histogram(bins = 20, 
                 alpha = 0.5, fill = 'blue') + 
  theme_minimal()

# Treinando e Interpretando o Modelo
# Import Library
install.packages("caTools")
library(caTools)

# Criando as amostras de forma randômica
amostra <- sample.split(df$age, SplitRatio = 0.70)

# Criando dados de treino - 70% dos dados
treino = subset(df, amostra == TRUE)

# Criando dados de teste - 30% dos dados
teste = subset(df, amostra == FALSE)

# Gerando o Modelo (Usando todos os atributos)
modelo_v1 <- lm(G3 ~ ., treino)
modelo_v2 <- lm(G3 ~ G2 + G1, treino)
modelo_v3 <- lm(G3 ~ absences, treino)
modelo_v4 <- lm(G3 ~ Medu, treino)

# Interpretando o Modelo
summary(modelo_v1) # 0.86
summary(modelo_v2) # 0.82
summary(modelo_v3) # 0.0002675
summary(modelo_v4) # 0.06442

# Plot do Modelo
plot(modelo_v1)

# Fazendo as predições
modelo_v1 <- lm(G3 ~ ., treino)
prevendo_G3 <- predict(modelo_v1, teste)
prevendo_G3

# Visualizando os valores previstos e observados
resultados <- cbind(prevendo_G3, teste$G3) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados)

# Tratando os valores negativos
trata_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Aplicando a função para tratar valores negativos em nossa previsão
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)
resultados$Previsto

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(df$G3) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2







