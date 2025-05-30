install.packages("readr")  # solo la primera vez
library(readr)

datos<-read_csv("C:/Users/marie/Desktop/Datos Covid.csv")

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