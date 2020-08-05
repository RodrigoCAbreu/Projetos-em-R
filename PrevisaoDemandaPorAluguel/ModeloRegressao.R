# Coleta e Transformação de Dados

# Variável que controla a execução do script
Azure <- FALSE

# Execução de acordo com o valor da variável Azure
if(Azure){
  source("src/Tools.R")
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  source("Tools.R")
  bikes <- read.csv("bikes.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE )
  
  # Selecionar as variáveis que serão usadas
  cols <- c("dteday", "mnth", "hr", "holiday",
            "workingday", "weathersit", "temp",
            "hum", "windspeed", "cnt")
  
  # Criando um subset dos dados
  bikes <- bikes[, cols]
  View(bikes)
  
  # Transformar o objeto de data
  bikes$dteday <- char.toPOSIXct(bikes)
  
  # Esta linha acima gera dois valores NA
  # Esta linha abaixo corrige
  bikes <- na.omit(bikes)
  
  # Normalizar as variaveis preditoras numericas 
  cols <- c("temp", "hum", "windspeed") 
  bikes[, cols] <- scale(bikes[, cols])  
}

# Criar uma nova variável para indicar dia da semana (workday)
bikes$isWorking <- ifelse(bikes$workingday & !bikes$holiday, 1, 0)  

# Adicionar uma coluna com a quantidade de meses, o que vai ajudar a criar o modelo
bikes <- month.count(bikes)

# Criar um fator ordenado para o dia da semana
bikes$dayWeek <- as.factor(weekdays(bikes$dteday))

bikes$dayWeek <- as.numeric(ordered(bikes$dayWeek, 
                                    levels = c("segunda-feira", 
                                               "terça-feira", 
                                               "quarta-feira", 
                                               "quinta-feira", 
                                               "sexta-feira", 
                                               "sábado", 
                                               "domingo")))

# Adiciona uma variável com valores únicos para o horário do dia em dias de semana e dias de fim de semana

bikes$workTime <- ifelse(bikes$isWorking, bikes$hr, bikes$hr + 24) 

# Transforma os valores de hora na madrugada, quando a demanda por bibicletas é praticamente nula 
bikes$xformHr <- ifelse(bikes$hr > 4, bikes$hr - 5, bikes$hr + 19)

# Adiciona uma variável com valores únicos para o horário do dia para dias de semana e dias de fim de semana
# Considerando horas da madrugada
bikes$xformWorkHr <- ifelse(bikes$isWorking, bikes$xformHr, bikes$xformHr + 24) 


# Análise de Correlação 

# Definindo as colunas para a análise de correlação 
cols <- c("mnth", "hr", "holiday", "workingday",
          "weathersit", "temp", "hum", "windspeed",
          "isWorking", "monthCount", "dayWeek", 
          "workTime", "xformHr", "cnt")

# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(bikes[, cols], method = method)))

head(cors)

# Preprando o plot
install.packages("lattice")
require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)


# Análise de Série Temporal 

# Avaliando a demanda por aluguel de bikes ao longo do tempo
# Construindo um time series plot para alguns determinados horários 
# em dias úteis e dias de fim de semana.
times <- c(7, 9, 12, 15, 18, 20, 22) 

# Time Series Plot
tms.plot <- function(times){
  ggplot(bikes[bikes$workTime == times, ], aes(x = dteday, y = cnt)) + 
    geom_line() +
    ylab("Numero de Bikes") +
    labs(title = paste("Demanda de Bikes as ", as.character(times), ":00", sep = "")) +
    theme(text = element_text(size = 20))
}

require(ggplot2)

lapply(times, tms.plot)

# Analisando BoxPlots

# Convertendo a variável dayWeek para fator ordenado e plotando em ordem de tempo
bikes$dayWeek <- fact.conv(bikes$dayWeek)

# Demanda de bikes x potenciais variáveis preditoras
labels <- list("Boxplots - Demanda de Bikes por Hora",
               "Boxplots - Demanda de Bikes por Estação",
               "Boxplots - Demanda de Bikes por Dia Útil",
               "Boxplots - Demanda de Bikes por Dia da Semana")

xAxis <- list("hr", "weathersit", "isWorking", "dayWeek")

# Função para criar os boxplots
plot.boxes  <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt", group = X)) + 
    geom_boxplot( ) + 
    ggtitle(label) +
    theme(text = element_text(size = 18)) 
}

Map(plot.boxes, xAxis, labels)






# Gera saida no Azure ML
if(Azure) maml.mapOutputPort('bikes')



