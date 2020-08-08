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