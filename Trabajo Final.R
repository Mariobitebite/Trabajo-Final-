##Trabajo Final 
#Mario Muñiz Puertas

install.packages("readr") 
library(readr)


datos <- read_csv("C:/Users/marie/Desktop/Datos Covid.csv")

head(datos)
install.packages("dplyr")
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

head(datos) 
colnames(datos)
glimpse(datos)

datos_esp <- datos %>%
  filter(country_code == "ES" & !is.na(country_code))

library(stringr)

install.packages("tidytext")
install.packages("stopwords")
library(tidytext)
library(stopwords)
library(dplyr)

#Palabras asociadas a la ansiedad
palabras_ansiedad <- c("ansiedad", "ansiosa", "ansioso", "nervios", "nerviosa", "nervioso", "ataque", "pánico")

# Palabras asociadas a depresión
palabras_depresion <- c("depresión", "triste", "tristeza", "vacío", "llorar", "llanto", "desesperanza", "infeliz", "abatido", "deprimido", "deprimida")

# Unimos ambas listas
palabras_clave <- c(palabras_ansiedad, palabras_depresion)

# Filtramos los tweets que contienen al menos una palabra clave
library(ggplot2)
tweets_relacionados <- datos_esp %>%
  filter(str_detect(str_to_lower(text), str_c(palabras_clave, collapse = "|")))
colnames(tweets_relacionados)
tweets_relacionados <- tweets_relacionados %>%
  mutate(fecha = as.Date(created_at))
frecuencia_diaria <- tweets_relacionados %>%
  count(fecha)

#Gráfico de línea

ggplot(frecuencia_diaria, aes(x = fecha, y = n)) +
  geom_line(color = "steelblue") +
  labs(title = "Evolución diaria de tweets sobre ansiedad y depresión",
       x = "Fecha",
       y = "Cantidad de tweets")

#Nube de palabras 
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("stringr") 
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)
library(stopwords)
library(stringr)
tokens_emocionales <- tweets_relacionados %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords("es")) %>%
  filter(str_detect(word, "[a-z]"))


# Conteo de frecuencia
frecuencias <- tokens_emocionales %>%
  count(word, sort = TRUE)

library(wordcloud)

wordcloud(words = frecuencias$word,
          freq = frecuencias$n,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))


##Análisis de emociones con diccionario

nrc <- read.csv("C:/Users/marie/Documents/Data Sciencie/Trabajo-Final-/nrc_espanol.csv", encoding = "UTF-8")

nrc <- nrc %>%
  filter(valor == 1) %>%
  select(palabra, sentimiento)

# Cruzamos los tokens con las emociones del diccionario
sentimientos <- tokens_emocionales %>%
  inner_join(nrc, by = c("word" = "palabra"))

#Contamos las emociones encontradas

conteo_emociones <- sentimientos %>%
  count(sentimiento, sort = TRUE)

print(conteo_emociones)

#Visualizador en gráfico 
library(ggplot2)

ggplot(conteo_emociones, aes(x = reorder(sentimiento, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emociones detectadas en los tweets relacionados con salud mental",
       x = "Emoción",
       y = "Frecuencia")



