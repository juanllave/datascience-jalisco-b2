library(tidyverse)
library(bbplot)
library(lubridate)
rm(list = ls())
setwd('~/Documents/Repos/datascience-jalisco-b2')

#Deshabilita la notación científica
options(scipen = 999)

#Carga los datos.
datos <- read_csv('databases/e.csv')

#1) ¿Cuál es la sucursal que recibe mas conexiones?
p1 <- datos %>%
  filter(TRUE) %>%
  group_by(branch_office) %>%
  summarise(conexiones = n()) %>%
  arrange(desc(conexiones))

g_p1 <- p1 %>%
  ggplot(mapping = aes(x=reorder(branch_office, conexiones), conexiones))+
  geom_col(colour = '#2cbba4', fill = '#2cbba4')+
  coord_flip()+
  bbc_style()+
  labs(title = "Conexiones por sucursal",
       subtitle = 'La sucursal que recibió más conexiones',
       x = "Sucursal", y="Conexiones")+
  geom_text(aes(label = conexiones),hjust = 1,  vjust= .5, color= "white", size = 10, fontface = 'bold')+
  theme(plot.subtitle = element_text(size = rel(1.5)))

  
finalise_plot(plot_name = g_p1,
              source = "Fuente: base de datos de Klustera",
              save_filepath = "g_p1.png",
              width_pixels = 1920, 
              height_pixels = 1080)


#2) ¿Qué día de la semana tenemos mas visitantes?
p2 <- datos %>%
  filter(visitor == 'TRUE') %>%
  group_by(day_of_week_tz) %>%
  summarise(visitantes = n())
  
g_p2 <- p2 %>%
  ggplot(mapping = aes(x=reorder(day_of_week_tz, visitantes), visitantes))+
  #ggplot(aes(x=day_of_week_tz, y=visitantes))+
  geom_point(colour = '#227070', fill = '#227070', size = 10)+
  geom_segment(aes(x=day_of_week_tz, xend=day_of_week_tz, y=0, yend=visitantes), color = '#227070', size=1)+
  coord_flip()+
  bbc_style()+
  labs(title = "Visitantes por día de la semana",
       subtitle = 'Días de la semana según número de visitantes',
       x = "Sucursal", y="Conexiones")+
  geom_text(aes(label = visitantes),hjust = -.4,  vjust= .5, size = 6, color='#227070')+
  theme(plot.subtitle = element_text(size = rel(1.5)))

finalise_plot(plot_name = g_p2,
              source = "Fuente: base de datos de Klustera",
              save_filepath = "g_p2.png",
              width_pixels = 1920, 
              height_pixels = 1080)

#3) ¿Cuál es el tiempo promedio de conexión de un visitante?
p3 <- datos %>%
  filter(visitor == 'TRUE')

p3 <- mean(p3$tiempodeses)
p3 <- round(p3, 2)

#p3 <- round(mean(p3$tiempodeses),2) #Intenté esta opción para simplicficar. Pero me aparece un error. 

print(paste('El timepo promedio de conexión de un visitante es de', p3, 'segundos', sep = ' '))
  
#4) ¿Cuantas persona por mes han realizado visitas?
p4 <- datos %>%
  filter(visitor == 'TRUE') %>%
  group_by(month_tz) %>%
  summarise(visitantes = n()) %>%
  arrange(desc(visitantes))

g_p4 <- p4 %>%
  ggplot(mapping = aes(x=reorder(month_tz, visitantes), visitantes))+
  geom_col(colour = '#56D1D1', fill = '#56D1D1')+
  coord_flip()+
  bbc_style()+
  labs(title = "Visitantes por mes",
       subtitle = 'Número de visitantes por mes',
       x = "Mes", y="Visitantes")+
  geom_text(aes(label = visitantes),hjust = 1,  vjust= .5, color= "white", size = 10, fontface = 'bold')+
  theme(plot.subtitle = element_text(size = rel(1.5)))


finalise_plot(plot_name = g_p4,
              source = "Fuente: base de datos de Klustera",
              save_filepath = "g_p4.png",
              width_pixels = 1920, 
              height_pixels = 1080)


#55) ¿A qué hora se registran más visitantes?
p5 <- datos %>%
  filter(visitor == 'TRUE') %>%
  group_by(hour_tz, day_of_week_tz) %>%
  summarise(visitantes = n())

g_p5 <- p5 %>%
  ggplot(aes(x=hour_tz, y=day_of_week_tz, fill=visitantes)) +
  geom_tile(color="black") +
  scale_fill_continuous(low="#ffeda0", high="#f03b20") +
  labs(title="Visitantes según hora y día de la semana", 
       x="Hora", y="Día de la semana", fill="Visitantes") +
  coord_fixed() +
  theme_bw()

finalise_plot(plot_name = g_p5,
              source = "Fuente: base de datos de Klustera",
              save_filepath = "g_p5.png",
              width_pixels = 1920, 
              height_pixels = 1080)