# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# Seu modelo deve prever a MEDV (Valor da Mediana de ocupação das casas).

# Carregando o pacote MASS
library(MASS)

# Importando os dados do dataset Boston
set.seed(101)
dados <- Boston
View(dados)

# Resumo dos dados
str(dados)
summary(dados)
any(is.na(dados))

# Carregando o pacote para Redes Neurais
install.packages("neuralnet")
library(neuralnet)

# Normalizacao 
maxs <- apply(dados, 2, max) 
mins <- apply(dados, 2, min)

# Imprimindo os valores
maxs
mins

# Normalizando
dados_normalizados <- as.data.frame(scale(dados, center = mins, scale = maxs - mins))
head(dados_normalizados)

# Criando os dados de treino e de teste
install.packages("caTools")
library(caTools)
split = sample.split(dados_normalizados$medv, SplitRatio = 0.70)

treino = subset(dados_normalizados, split == TRUE)
teste = subset(dados_normalizados, split == FALSE)

# Obtendo o nome das colunas
coluna_nomes <- names(treino)
coluna_nomes

# Agregando
formula <- as.formula(paste("medv ~", paste(coluna_nomes[!coluna_nomes %in% "medv"], collapse = " + ")))
formula

# Treinando o Modelo
rede_neural <- neuralnet(formula, data = treino, hidden = c(5,3), linear.output = TRUE)

# Plot
plot(rede_neural)

# Fazendo previsoes com os dados de teste
rede_neural_prev <- compute(rede_neural, teste[1:13])
rede_neural_prev

# O retorno da previsao da Rede Neural é uma lista
str(rede_neural_prev)

# Convertendo os dados de teste
previsoes <- rede_neural_prev$net.result * (max(dados$medv) - min(dados$medv)) + min(dados$medv)
teste_convert <- (teste$medv) * (max(dados$medv) - min(dados$medv)) + min(dados$medv)
teste_convert

# Calculando o Mean Squared Error
MSE.nn <- sum((teste_convert - previsoes)^2)/nrow(teste)
MSE.nn

# Obtendo os erros de previsao
error.df <- data.frame(teste_convert, previsoes)
head(error.df)

# Plot dos erros
library(ggplot2)
ggplot(error.df, aes(x = teste_convert,y = previsoes)) + 
  geom_point() + stat_smooth()




















