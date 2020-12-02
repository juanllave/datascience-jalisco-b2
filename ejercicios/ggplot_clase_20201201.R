rm(list=ls())

######################
# Oscar Elton			   #
# Gráficos en ggplot #
######################

# 1) Importar todos los paquetes necesarios
library(tidyverse)
library(treemap)

# 2) Establecer los directorios
dir1 <- "~/Documents/Repos/datascience-jalisco-b2/databases" # Base de datos
dir2 <- '~/Documents/Repos/datascience-jalisco-b2/graphs' # Gráficas


# 3) Obtener la base de datos
data <- read_csv(paste(dir1, "rnped_limpia.csv", sep="/"))
# Mostrar la base de datos
str(data)
## Diccionario de datos:
# inegi: Clave dada por el INEGI a cada municipio. Primeros dos dígitos 
# corresponden al estado, los siguientes tres al municipio
# ent: Parte de la clave de INEGI que corresponde a la entidad federativa.
# nom_ent: Nombre oficial de la entidad federativa.
# year: Año en el que se denunciaron las despariciones
# sexo: Sexo de las personas desaparecidas
# rango_edad: Rango de edades de las personas en el grupo en cuestión.
# pob: Población total del municipio en ese año para ese grupo.
# total: Total de personas deparecidas de ese grupo en ese año.
# tdes: Tasa de desaparecidos en ese municipio en ese grupo
#
# Nota: Recuerda, cada observación corresponde a un grupo de personas
# desparecidas de un cierto sexo, en un cierto municipio y un cierto rango
# de edad.

########
# Bars #
########

# PREGUNTA 1: ¿cuántos desaparecidos hubo por municipio en...2011 (año más violento)?
# Esta primera  gráfica será un scatter, pero que va a cumplir la función de las barras dado que vamos 
# a graficar 2,456 municipios
# a) Preparar los datos:
data$year
tempo <- data %>%
  filter(year==2011) %>%
  group_by(inegi, nom_ent, nom_mun) %>%
  summarise(total = sum(total)) %>%
  arrange(inegi) %>%
  ungroup()

# b) Graficar datos
gr <- ggplot(tempo, aes(x=inegi, y=total)) +
  geom_point()

gr <- ggplot(tempo, aes(x=inegi, y=total)) +
  geom_point(color="#135DA8") +
  geom_text(aes(x=inegi, y=total, label=paste(nom_mun, nom_ent, total, sep=", ")),
            data=tempo[tempo$total>=75,], hjust=0, vjust=-1) +
  labs(title="Total de personas desaparecidas por municipio",
       subtitle="2011",
       x="", y="Total de personas desaparecidas") +
  theme(axis.text.x = element_blank())

## Notas:
# geom_text - texto sobre los elementos de nuestra gráfica
# hjust: negativo -> derecha, positivo -> izquierda
# vjust: positivo -> abajo, negativo -> arriba

# c) Guardar las gráficas
ggsave(paste(dir2, "1.png", sep="/"), plot=gr, width=12, height=12)

# PREGUNTA 4: ¿Cuál es la tasa de desaparecidos por cada uno de los sexos pero distinguiendo
# la información de cada estado?
tempo <- data %>%
  group_by(sexo, ent, nom_ent) %>%
  summarise(tdes = weighted.mean(tdes, pob)) %>%
  mutate(tdes = round(tdes, 2))

gr <- ggplot(tempo, aes(x=sexo, y=tdes)) +
  geom_bar(stat="identity", fill="#41b6c4") +
  geom_text(aes(label=tdes), vjust=-0.3, size=3) +
  facet_grid(sexo~nom_ent) +
  labs(title="Tasa de desaparecidos por sexo y estado \n2010 - 2015", 
       x="Sexo", y="Tasa de desaparecidos") +
  theme_bw()
ggsave(paste(dir2, "5.png", sep="/"), plot=gr, width=12, height=12)

# PREGUNTA 5: queremos ver % de hombres/mujeres que desaparecieron entre los 12 a 17 años vs 26-40 años
# Vamos a utilizar barras 2D .Nos sirven cuando queremos ver dos dimensiones de una variable con varias 
# categorias.
tempo <- data %>%
  group_by(sexo, rango_edad) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  group_by(sexo) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 2))

# Quedémonos con lo que nos importa
tempo <- tempo %>%
  select(sexo, rango_edad, porcentaje) %>%
  filter(rango_edad=="12 a 17 a\u00f1os" | rango_edad=="26 a 40 a\u00f1os") %>%
  spread(rango_edad, porcentaje)

names(tempo) <- c("sexo", "des12a17", "des26a40")


gr <- ggplot(tempo, aes(x=des12a17, y=des26a40, color=sexo)) +
  geom_segment(aes(xend=des12a17), yend=0, size=2) +
  geom_segment(aes(yend=des26a40), xend=0, size=2) +
  scale_y_continuous(limits=c(0, 42)) +
  scale_x_continuous(limits=c(0, 42)) +
  labs(title="Porcentaje de personas desaparecidas por sexo \n 12 a 17 años vs 26 a 40 años",
       x="% de personas desaparecidas 26 a 40 años",
       y="% de personas desaparecidas 12 a 17 años", color="Sexo") +
  theme_bw()
ggsave(paste(dir2, "7.png", sep="/"), plot=gr, width=12, height=12)

############
# Heatmaps #
############
tempo <- data %>%
  group_by(sexo, rango_edad) %>%
  summarise(tdes = weighted.mean(tdes, pob))

gr <- ggplot(tempo, aes(x=sexo, y=rango_edad, fill=tdes)) +
  geom_tile(color="black") +
  scale_fill_continuous(low="#ffeda0", high="#f03b20") +
  labs(title="Tasa de desapersonas desaparecidas parecidos promedio \n2010-2015", 
       x="Sexo", y="Edad", fill="Tasa de personas desaparecidas") +
  coord_fixed() +
  theme_bw()
ggsave(paste(dir2, "8.png", sep="/"), plot=gr, width = 12, height = 12)

############ 
# Treemaps #
############
# Vamos a ver el porcentaje de desaparecidos por estado en 2011. Un treemap nos ayuda a ver las partes que
# conforman un todo facilmente, utilizando areas.

tempo  <- data %>%
  filter(year==2011) %>% # Filtrar
  group_by(ent, nom_ent) %>% # Agrupar
  summarise(total = sum(total)) %>% # Reducir todo a 1 obs por grupo
  ungroup() %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 2))

png(paste(dir2, "9.png", sep="/"), width=12, height=12, units="in", res=300)
treemap(tempo, index="ent", vSize="porcentaje", vColor="index", type="index", title="Porcentaje desaperecidos por entidad - 2011", palette="Reds", title.legend="", border.col="grey", border.lwd=0.5)
dev.off()

## Mini-ejercicio:
# 1) Indica en el argumento index la columna de donde obtendrás los valores a graficar
# 2) Indicar en vColor la columna de donde se obtendrán los valores para los colores
# 3) Cambia a una paleta de azules
png(paste(dir2, "10.png", sep="/"), width=12, height=12, units="in", res=300)
treemap(tempo, index="____", vSize="porcentaje", vColor="____", type="value",
        title="Porcentaje desaperecidos por entidad - 2011",
        palette="____", title.legend="", border.col="grey", border.lwd=0.5)
dev.off()

###########
# Scatter #
###########

tempo <- data %>%
  group_by(inegi, nom_ent, nom_mun) %>%
  summarise(tdes = round(weighted.mean(tdes, pob),2),
            total = round(mean(total),2))

gr <- ggplot(tempo, aes(x=total, y=tdes)) +
  geom_point(color="maroon") +
  geom_text(aes(label=inegi), color="maroon", size=3, vjust=-1, hjust=0) +
  labs(title="Total de desaparecidos vs Tasa de desaparecidos",
       x="Total de desaparecidos", y="Tasa de desaparecidos") +
  theme_bw()
ggsave(paste(dir2, "11.png", sep="/"), plot=gr, width = 12, height = 12)

# Líneas de tendencia
#1 linea recta
gr <- ggplot(tempo, aes(x=total, y=tdes)) +
  geom_point(color="maroon") +
  geom_text(aes(label=inegi), color="maroon", size=3, vjust=-1, hjust=0) +
  geom_smooth(method="lm", se=T) +
  labs(title="Total de desaparecidos vs Tasa de desaparecidos",
       x="Total de desaparecidos", y="Tasa de desaparecidos") +
  theme_bw()
ggsave(paste(dir2, "12.png", sep="/"), plot=gr, width = 12, height = 12)

#2 escalas
gr <- ggplot(tempo, aes(x=total, y=tdes)) +
  geom_point(color="maroon") +
  geom_text(aes(label=inegi), color="maroon", size=3, vjust=-1, hjust=0) +
  geom_smooth() +
  labs(title="Total de desaparecidos vs Tasa de desaparecidos",
       x="Total de desaparecidos", y="Tasa de desaparecidos") +
  theme_bw() +
  scale_x_log10() + scale_y_log10()
ggsave(paste(dir2, "13.png", sep="/"),plot=gr, width=12, height = 12)


##########
# Fiebre #
##########
# Sirve para ver tendencia en el tiempo
tempo <- data %>%
  group_by(sexo, year) %>%
  summarise(tdes = round(weighted.mean(tdes, pob),2))

gr <- ggplot(tempo, aes(x=year, y=tdes, color=sexo)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=tdes), vjust=-1) +
  labs(title="Tendencia de la tasa de desaparecidos \n 2010-2015",
       x="Año", y="Tasa de desaparecidos", color="Sexo") +
  theme_bw() +
  scale_colour_manual(values = c("Hombre" = "red", "Mujer" = "blue"))
ggsave(paste(dir2, "15.png", sep="/"), plot=gr, width=12, height = 12)

########
# Area #
########
# Otra forma seria ver el % de desaparecidos hombres y mujeres por año
tempo <- data %>%
  group_by(sexo, year) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100,2))

gr <- ggplot(tempo, aes(x=year, y=porcentaje, fill=sexo)) +
  geom_area() +
  geom_point( data=tempo[tempo$sexo=='Mujer',])+
  geom_text(aes(label=porcentaje), vjust=-1, data=tempo[tempo$sexo=='Mujer',]) +
  scale_fill_manual(values=c("#67a9cf", "#ef8a62")) +
  labs(title="Porcentaje de desaparecidos por sexo \n 2010-2015",
       x="Año", y="% de desaparecidos", fill="Sexo") +
  theme_bw()
ggsave(paste(dir2, "16.png", sep="/"), plot=gr, width=12, height = 12)

# que tal % por estado por año?
tempo <- data %>%
  group_by(nom_ent, year) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100,2))

gr <- ggplot(tempo, aes(x=year, y=porcentaje, fill=nom_ent)) +
  geom_area(color="black") +
  labs(title="Porcentaje de desaparecidos por entidad \n 2010-2015",
       x="Año", y="% de desaparecidos", fill="Entidad") +
  theme_bw()
ggsave(paste(dir2, "17.png", sep="/"), plot=gr, width = 12, height = 12)

############
# Boxplots #
############
tempo <- data %>%
  group_by(nom_ent, year) %>%
  summarise(tdes = weighted.mean(tdes, pob))

gr <- ggplot(tempo, aes(x=nom_ent, y=tdes)) +
  geom_boxplot() +
  labs(title="Distribucion tasa de desaparecidos por estado", x="Entidad", y="Tasa de desaparecidos") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))
ggsave(paste(dir2, "18.png", sep="/"), plot=gr, width=12, height = 12)


tempo$year <- as.character(tempo$year)
gr <- ggplot(tempo, aes(x=year, y=tdes, fill=year)) +
  geom_boxplot() +
  labs(title="Distribucion tasa de desaparecidos por año", x="Año", y="Tasa de desaparecidos", fill="") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")
ggsave(paste(dir2, "19.png", sep="/"), plot=gr, width=12, height = 12)
