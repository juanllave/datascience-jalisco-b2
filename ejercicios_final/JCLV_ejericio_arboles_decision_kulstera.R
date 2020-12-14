#Limppiar ambiente y cargar librerías.
rm(list = ls())
library(tidyverse)
library(gmodels)
library('C50')

#Establecer directorio de trabajo
dir1 <- '~/Documents/Repos/datascience-jalisco-b2'
setwd(dir1)

#Cargar datos y eliminar filas innecesarias.
datos_e <- read.csv(paste(dir1, 'databases', 'e.csv', sep = '/')) %>%
  select(-1, -2, -6)

datos_v <- read.csv(paste(dir1, 'databases', 'v.csv', sep = '/')) %>%
  select(-1, -2, -6)


str(datos_e)#Revisar la estrucutra de la tabla

table(datos_e$diagnosis)

#Ajustar la cateogría de la columna 'diagosis'
datos_e$visitor <- factor(datos_e$visitor,
                         levels = c('true', 'false'),
                         labels = c('TRUE', 'FALSE'))

round(prop.table(table(datos_e$visitor))*100, digits = 1)

#Elegir el 80% de las filas para entrenamiento y el 20% restante para prueba
nfilas <- nrow(datos_e) * .80
set.seed(123)
index <- sample(1:nrow(datos_e), nfilas)
datos_e_train <- datos_e[index, -5]
datos_e_test <- datos_e[-index, -5]

#Separar las etiquetas de diagnóstico para corroborar los resultados
datos_e_train_labels <- datos_e[index, 5]
datos_e_test_labels <- datos_e[-index, 5]

#Genera el modelo con el 80% de las filas de entrenamiento
datos_e_model <- C5.0(datos_e_train, datos_e_train_labels)
summary(datos_e_model)


#Valida el modelo con el 20% de las filas separadas
datos_e_predict <- predict(datos_e_model, datos_e_test)

CrossTable(datos_e_test_labels, datos_e_predict)

##Boost: repite el entrenamiento n veces
datos_e_boost50_model <- C5.0(datos_e_train, datos_e_train_labels, trials = 50)
datos_e_boost50_model
summary(datos_e_boost50_model)


datos_e_boost_pred50 <- predict(datos_e_boost50_model, datos_e_test)
CrossTable(datos_e_test_labels, datos_e_boost_pred50,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicción'))

#Pasa el modelo por el dataset v.
datos_v_boost_pred50 <- predict(datos_e_boost50_model, datos_v)

#Agrega las predicciones al dataset datos_v y lo guarda como .csv
datos_v <- datos_v %>%
  mutate(prediccion = datos_v_boost_pred50)

write_csv(datos_v, 'vpred.csv')