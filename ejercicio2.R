
######-----EJERCICIO 2----#####
library(readr)
library(tidyverse)
library(ggpubr)
library(dplyr)
library (ggplot2)
library(reshape)
library(stringr)



datos_prov <- read_csv("datos_provincias.csv")
View(datos_prov)



CodProv <- read_csv("CodProv.txt")
View(CodProv)



CodCCAA <- read_delim("CodCCAA.csv", delim = "\t", 
                      escape_double = FALSE, trim_ws = TRUE)
View(CodCCAA)
# esto es para quitar los - en codCCAA
# de esta manera creamos otra variable que contenga solo los provincia_iso para hacer el merge con datos_privincias
CodProv$provincia_iso <- str_split_fixed(CodProv$Código, "-", Inf)[,2]

todos <- merge (CodProv,datos_prov, by ="provincia_iso",sort=F)


# Provincia asignada
45147622 %% 17 # = 8 -> EXTREMADURA

# seleccionamos solo los datos de extremadura -> comunidad autonoma  = EX

#cambiamos los nombres del dataset por simplicidad
names(todos)
colnames(todos) <- c("provincia_iso" , "Codigo","Nombre_provincia","Comunidad_autónoma","fecha", "num_casos", 
                          "num_casos_prueba_pcr","num_casos_prueba_test_ac", "num_casos_prueba_otras",
                          "num_casos_prueba_desconocida")


# filtramos por exremadura
extremadura <- filter(todos, Comunidad_autónoma == "EX")

#para comprobar que todo este bien, contamos cuantos datos tenemos de cada provincia

#cuentas <- extremadura %>% count(Nombre_provincia)  # 237 datos de cada provincia
#cuantas2  <- datos_prov %>% count(provincia_iso)


#### --------- GRAFICO CASOS NUEVOS POR PROVINCIA ---------########
# grafico de puntos
ggplot(extremadura, aes(fecha, num_casos))+geom_point(aes(colour = factor(Nombre_provincia)))
# grafico de lineas
ggplot(extremadura, aes(fecha, num_casos))+ geom_line(aes(colour = factor(Nombre_provincia)))



###----------- GRAFICO DE TODAS LAS VARIABLES -----------------#######



# 1. convertirmos el data set en formato long para poder representar todas las columnas
      # en funcion de la variable fecha en este caso

todos_plot_select <- select(todos,num_casos,num_casos_prueba_pcr,
                            num_casos_prueba_test_ac,num_casos_prueba_otras,
                            num_casos_prueba_desconocida)

todos_plot <- data.frame(x = todos$fecha,
                         todos_plot_select)
todos_plot <- melt(todos_plot, id.vars = "x")

#grafico
cols <- c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA", "#9632B8")
ggplot(todos_plot, aes(x = x, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(values = cols)

############# FINAL EJERCICIO 2 #############

