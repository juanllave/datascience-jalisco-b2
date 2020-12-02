library(tidyverse)
library(datos)

#1. Filtra los vuelos para mostrar únicamente los aviones que han realizado al menos cien viajes.
mas_cien <- vuelos %>%  
  group_by(codigo_cola) %>%
  summarise(viajes = n()) %>%
  arrange(desc(viajes)) %>%
  filter(viajes >= 100 & codigo_cola != 'NA')

#2. Combina datos::vehiculos y datos::comunes para encontrar los registros de los modelos más comunes.
modelos_comunes <- left_join(vehiculos, comunes) %>%
  group_by(modelo) %>%
  summarise(modelos_mas_comunes = n()) %>%
  arrange(desc(modelos_mas_comunes))

#3. Encuentra las 48 horas (en el transcurso del año) que tengan los peores atrasos. Haz una referencia cruzada con la tabla clima. ¿Puedes observar patrones?
fecha_i1 <- as.data.frame(seq(as.Date('2013-01-01'), by = '2 days', length.out = (365/2)))
fecha_i2 <- seq(as.Date('2013-01-02'), by = '2 days', length.out = (365/2))

atrasos <- vuelos %>%
  select(fecha_hora, atraso_salida) %>%
  group_by(fecha_hora) %>%
  summarise(total_atrasos = sum(atraso_salida))



atrasos <- left_join(atrasos, clima)


