# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/ClassTools.R")
  Credit <- maml.mapInputPort(1)
}else{
  source("ClassTools.R")
  Credit <- read.csv("GermanCredit.csv", header = F, stringsAsFactors = F )
  metaFrame <- data.frame(colNames, isOrdered, I(factOrder))
  Credit <- fact.set(Credit, metaFrame)
  
  # Balancear o número de casos positivos e negativos
  Credit <- equ.Frame(Credit, 2)
}

# Transformando variáveis numéricas em variáveis categóricas
toFactors <- c("Duration", "CreditAmount", "Age")
maxVals <- c(100, 1000000, 100)
facNames <- unlist(lapply(toFactors, function(x) paste(x, "_f", sep = "")))
Credit[, facNames] <- Map(function(x, y) quantize.num(Credit[, x], maxval = y), toFactors, maxVals)

# str(Credit)

# Output 
if(Azure) maml.mapOutputPort('Credit')

# Análise Exploratória de Dados

# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/ClassTools.R")
  Credit <- maml.mapInputPort(1)
}

# Plots usando ggplot2
library(ggplot2)
lapply(colNames2, function(x){
  if(is.factor(Credit[,x])) {
    ggplot(Credit, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ CreditStatus) + 
      ggtitle(paste("Total de Credito Bom/Ruim por",x))}})

# Plots CreditStatus vs CheckingAcctStat
lapply(colNames2, function(x){
  if(is.factor(Credit[,x]) & x != "CheckingAcctStat") {
    ggplot(Credit, aes(CheckingAcctStat)) +
      geom_bar() + 
      facet_grid(paste(x, " ~ CreditStatus"))+ 
      ggtitle(paste("Total de Credito Bom/Ruim CheckingAcctStat e",x))
  }})

# Feature Selection (Seleção de Variáveis)


# Variavel que controla a execucao do script
Azure <- FALSE

if(Azure){
  source("src/ClassTools.R")
  Credit <- maml.mapInputPort(1)
}  

# Modelo randomForest para criar um plot de importância das variáveis
library(randomForest)
modelo <- randomForest( CreditStatus ~ .
                        - Duration
                        - Age
                        - CreditAmount
                        - ForeignWorker
                        - NumberDependents
                        - Telephone
                        - ExistingCreditsAtBank
                        - PresentResidenceTime
                        - Job
                        - Housing
                        - SexAndStatus
                        - InstallmentRatePecnt
                        - OtherDetorsGuarantors
                        - Age_f
                        - OtherInstalments, 
                        data = Credit, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo)

outFrame <- serList(list(credit.model = modelo))

## Output 
if(Azure) maml.mapOutputPort("outFrame")


# Criar um modelo de classificação baseado em randomForest
library(randomForest)

# Cross Tabulation
?table
table(Credit$CreditStatus)

# Funcao para gerar dados de treino e dados de teste
splitData <- function(dataframe, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

# Gerando dados de treino e de teste
splits <- splitData(Credit, seed = 808)

# Separando os dados
dados_treino <- splits$trainset
dados_teste <- splits$testset

# Verificando o numero de linhas
nrow(dados_treino)
nrow(dados_teste)

# Construindo o modelo
modelo <- randomForest( CreditStatus ~ CheckingAcctStat
                        + Duration_f
                        + Purpose
                        + CreditHistory
                        + SavingsBonds
                        + Employment
                        + CreditAmount_f, 
                        data = dados_treino, 
                        ntree = 100, 
                        nodesize = 10)

# Imprimondo o resultado
print(modelo)

# Gerando previsões nos dados de teste
previsoes <- data.frame(observado = dados_teste$CreditStatus,
                        previsto = predict(modelo, newdata = dados_teste))


# Visualizando o resultado
View(previsoes)
View(dados_teste)

# Calculando a Confusion Matrix em R (existem outras formas)

# Formulas
Accuracy <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + x[1,2] + x[2,1] + x[2,2])
}

Recall <- function(x){  
  x[1,1] / (x[1,1] + x[1,2])
}

Precision <- function(x){
  x[1,1] / (x[1,1] + x[2,1])
}

W_Accuracy  <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + 5 * x[1,2] + x[2,1] + x[2,2])
}

F1 <- function(x){
  2 * x[1,1] / (2 * x[1,1] + x[1,2] + x[2,1])
}

# Criando a confusion matrix.
confMat <- matrix(unlist(Map(function(x, y){sum(ifelse(previsoes[, 1] == x & previsoes[, 2] == y, 1, 0) )},
                             c(2, 1, 2, 1), c(2, 2, 1, 1))), nrow = 2)


# Criando um dataframe com as estatisticas dos testes
df_mat <- data.frame(Category = c("Credito Ruim", "Credito Bom"),
                     Classificado_como_ruim = c(confMat[1,1], confMat[2,1]),
                     Classificado_como_bom = c(confMat[1,2], confMat[2,2]),
                     Accuracy_Recall = c(Accuracy(confMat), Recall(confMat)),
                     Precision_WAcc = c(Precision(confMat), W_Accuracy(confMat)))

print(df_mat)

# Gerando uma curva ROC em R
library("ROCR")

# Gerando as classes de dados
class1 <- predict(modelo, newdata = dados_teste, type = 'prob')
class2 <- dados_teste$CreditStatus

# Gerando a curva ROC
?prediction
?performance
pred <- prediction(class1[,2], class2)
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

# Gerando Confusion Matrix com o Caret
install.packages("caret")
library(lattice)
library(caret)
?confusionMatrix
confusionMatrix(previsoes$observado, previsoes$previsto)



