## Coletando os Dados

dados <- read.csv("dataset.csv", stringsAsFactors = FALSE)
dim(dados)
str(dados)
View(dados)


## Pré-Processamento

# Excluindo a coluna ID

dados$id = NULL

# Ajustando o label da variável alvo
dados$diagnosis = sapply(dados$diagnosis, function(x){ifelse(x=='M', 'Maligno', 'Benigno')})

# Converter a variavel diagnosis para o tipo Fator
table(dados$diagnosis)
dados$diagnosis <- factor(dados$diagnosis, levels = c("Benigno", "Maligno"), labels = c("Benigno", "Maligno"))
str(dados$diagnosis)

# Verificando a proporção
round(prop.table(table(dados$diagnosis)) * 100, digits = 1) 

# Medidas de Tendência Central
# Detectamos um problema de escala entre os dados, que então precisam ser normalizados
# O cálculo de distância feito pelo kNN é dependente das medidas de escala nos dados de entrada.
summary(dados[c("radius_mean", "area_mean", "smoothness_mean")])

# Criando um função de normalização
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Testando a função de normalização - os resultados devem ser idênticos
normalizar(c(1, 2, 3, 4, 5))
normalizar(c(10, 20, 30, 40, 50))

# Normalizando os dados
dados_norm <- as.data.frame(lapply(dados[2:31], normalizar))
View(dados_norm)


## Treinando o modelo com KNN
library(class)
?knn

# Criando dados de treino e dados de teste
dados_treino <- dados_norm[1:469, ]
dados_teste <- dados_norm[470:569, ]
View(dados_teste)

# Criando os labels para os dados de treino e de teste
dados_treino_labels <- dados[1:469, 1]
dados_teste_labels <- dados[470:569, 1]
length(dados_treino_labels)
length(dados_teste_labels)
View(dados_teste_labels)
dim(dados_treino)

# Criando o modelo
modelo_knn_v1 <- knn(train = dados_treino, 
                     test = dados_teste,
                     cl = dados_treino_labels, 
                     k = 21)

# A função knn() retorna um objeto do tipo fator com as previsões para cada exemplo no dataset de teste
summary(modelo_knn_v1)


## Avaliando e Interpretando o Modelo

# Carregando o gmodels
library(gmodels)

# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_knn_v1, prop.chisq = FALSE)

# Falso Positivo - Erro Tipo I
# Falso Negativo - Erro Tipo II

# Taxa de acerto do Modelo: 98% (acertou 98 em 100)


## Otimizando a Performance do Modelo

# Usando a função scale() para padronizar o z-score 
?scale()
dados_z <- as.data.frame(scale(dados[-1]))

# Confirmando transformação realizada com sucesso
summary(dados_z$area_mean)

# Criando novos datasets de treino e de teste
dados_treino <- dados_z[1:469, ]
dados_teste <- dados_z[470:569, ]

dados_treino_labels <- dados[ 1: 469, 1] 
dados_teste_labels <- dados[ 470: 569, 1]

# Reclassificando
modelo_knn_v2 <- knn(train = dados_treino, 
                     test = dados_teste,
                     cl = dados_treino_labels, 
                     k = 21)

# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_knn_v2, prop.chisq = FALSE)

### Piorou a eficacia do modelo


## Construindo um Modelo com Algoritmo Support Vector Machine (SVM)

# Definindo a semente para resultados reproduzíveis
set.seed(40) 

# Prepara o dataset
dados <- read.csv("dataset.csv", stringsAsFactors = FALSE)
dados$id = NULL
dados[,'index'] <- ifelse(runif(nrow(dados)) < 0.8,1,0)
View(dados)

# Dados de treino e teste
trainset <- dados[dados$index==1,]
testset <- dados[dados$index==0,]

# Obter o índice 
trainColNum <- grep('index', names(trainset))

# Remover o índice dos datasets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

# Obter índice de coluna da variável target no conjunto de dados
typeColNum <- grep('diag',names(dados))

# Cria o modelo
# Nós ajustamos o kernel para radial, já que este conjunto de dados não tem um 
# plano linear que pode ser desenhado
install.packages("e1071")
library(e1071)
?svm
modelo_svm_v1 <- svm(diagnosis ~ ., 
                     data = trainset, 
                     type = 'C-classification', 
                     kernel = 'radial') 


# Previsões

# Previsões nos dados de treino
pred_train <- predict(modelo_svm_v1, trainset) 

# Percentual de previsões corretas com dataset de treino
mean(pred_train == trainset$diagnosis)  


# Previsões nos dados de teste
pred_test <- predict(modelo_svm_v1, testset) 

# Percentual de previsões corretas com dataset de teste
mean(pred_test == testset$diagnosis)  

# Confusion Matrix
table(pred_test, testset$diagnosis)

###Houve melhora na eficiencia


## Construindo um Modelo com Algoritmo Random Forest

# Criando o modelo
library(rpart)
modelo_rf_v1 = rpart(diagnosis ~ ., data = trainset, control = rpart.control(cp = .0005)) 

# Previsões nos dados de teste
tree_pred = predict(modelo_rf_v1, testset, type='class')

# Percentual de previsões corretas com dataset de teste
mean(tree_pred==testset$diagnosis) 

# Confusion Matrix
table(tree_pred, testset$diagnosis)

### Piorou a eficacia do modelo