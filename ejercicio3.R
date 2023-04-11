
##############---------EJERCICIO 3-------------########

library(readr)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(reshape)


library(readxl)


articulos <- read_excel("articulo.xlsx")
View(articulos)

descuentos <- read_delim("descuento_aplicar.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
View(descuentos)
descuentos
View(descuento_aplicar)
#crear nueva columna con la multiplicacion PVP * CANTIDAD

articulos$INGRESO_BRUTO <- articulos$PVP * articulos$CANTIDAD
names(articulos)



# meter otra variable (columna) al dataset articulos con el tipo de articulo que sea (A,B,C)
# calculamos los deciles y agrupamos como se indica en el bucle

deciles <- quantile(articulos$INGRESO_BRUTO, prob=seq(0, 1, length = 11))
deciles
articulos$INGRESO_BRUTO

a = 1
for (i in articulos$INGRESO_BRUTO) {
  if(i > deciles[9]) {
  articulos$TIPO[a] <- "A"
  a = a+1
  } else if(i > deciles[6] & i <= deciles[9] )  {
    articulos$TIPO[a] <- "B"
    a = a+1
  } else {
    articulos$TIPO[a] <- "C"
    a = a+1
  }
 
}

articulos

# crear otro dataframe uniendo las tablas articulos y descuentos segun corresponda con al enunciado
#cambiamos el nombre a la columna tipo de descuentos para poder unirlas mediante un merge de manera sencilla

colnames(descuentos) <- c("TIPO" , "DESCUENTO")
clientes <- merge(articulos,descuentos,by ="TIPO")

a = 1
for (i in articulos$TIPO) {
  if(i == "A") {
    clientes$PVP_DESCUENTO[a] <-clientes$PVP[a] - (clientes$PVP[a]*clientes$DESCUENTO[a]/100)
    a = a+1
  } else if(i == "B"  )  {
    clientes$PVP_DESCUENTO[a] <-clientes$PVP[a] - (clientes$PVP[a]*clientes$DESCUENTO[a]/100)
    a = a+1
  } else {
    clientes$PVP_DESCUENTO[a] <-clientes$PVP[a] - (clientes$PVP[a]*clientes$DESCUENTO[a]/100)
    a = a+1
  }
}


# PERDIDAS AL APLICAR 
clientes$INGRESO_BRUTO_DESCUENTO <- clientes$PVP_DESCUENTO * clientes$CANTIDAD
TOTAL_BRUTO = sum(clientes$INGRESO_BRUTO)
TOTAL_BRUTO
TOTAL_BRUTO_DESCUENTO = sum(clientes$INGRESO_BRUTO_DESCUENTO)
TOTAL_BRUTO_DESCUENTO

diferencia  <-  (TOTAL_BRUTO_DESCUENTO / TOTAL_BRUTO)*100
100 - diferencia  #en porcentaje

######## FIN EJERCICIO 3 ##########