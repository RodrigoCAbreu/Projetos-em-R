# Estudo de Caso - Extraindo Dados da Web com Web Scraping em R

# Pacotes R para Web Scraping
# RCurl
# httr
# XML
# rvest

install.packages('rvest')
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)

# Leitura da web page
webpage <- read_html("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")
webpage

# Extraindo os registros

?html_nodes
results <- webpage %>% html_nodes(".short-desc")
results

# Construindo o dataset
records <- vector("list", length = length(results))
records

for (i in seq_along(results)) {
  date <- str_c(results[i] %>% 
                  html_nodes("strong") %>% 
                  html_text(trim = TRUE), ', 2017')
  
  lie <- str_sub(xml_contents(results[i])[2] %>% html_text(trim = TRUE), 2, -2)
  
  explanation <- str_sub(results[i] %>% 
                           html_nodes(".short-truth") %>% 
                           html_text(trim = TRUE), 2, -2)
  
  url <- results[i] %>% html_nodes("a") %>% html_attr("href")
  
  records[[i]] <- tibble(date = date, lie = lie, explanation = explanation, url = url)
}


# Dataset final
df <- bind_rows(records)


# Transformando o campo data para o formato Date em R
df$date <- mdy(df$date)


# Exportando para CSV
write_csv(df, "mentiras_trump.csv")


# Lendo os dados
df <- read_csv("mentiras_trump.csv")
View(df)

