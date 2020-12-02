library(tidyverse)

datos <- read.csv('e.csv')
datos1 <- read_csv('e.csv')


#1) ¿Cuál es la sucursal que recibe mas conexiones?
p1 <- datos %>%
  group_by(branch_office) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#2) ¿Qué día de la semana tenemos mas visitantes?
p2 <- datos %>%
  filter(visitor == 'true') %>%
  group_by(day_of_week_tz) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

p2_2 <- datos1 %>%
  filter(visitor == 'TRUE')

table(p2_2$visitor)

#3) ¿Cuál es el tiempo promedio de conexión de un visitante?
p3 <- datos %>%
  filter(visitor == 'true')
  
p3_s <- mean(p3$tiempodeses)
p3_m <- p3_s/60

#4) ¿Cuantas persona por mes han realizado visitas?
p4 <- datos %>%
  filter(visitor == 'TRUE') %>%
  group_by(month_tz) %>%
  summarise(count = n())

p4 <-p4 %>%
  arrange(desc(count))

#55) ¿A qué hora se registran más visitantes?
p5 <- datos %>%
  filter(visitor == 'TRUE') %>%
  group_by(hour_tz) %>%
  summarise(count = n())

p5 <- p5 %>%
  arrange(desc(count))
