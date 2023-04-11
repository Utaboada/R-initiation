
####### ------------ EJERCICIO 1 ----------##########
library(tidyverse) # para usar la funcion str_c para unir cadenas

# Constantes
numero_serie <- "0209"


# primero generamos como cadena de caracteres los numero_solo_series de la solo_serie desde el 000 -> 999

solo_serie <- 0:999
solo_serie <- formatC(solo_serie, width=nchar(999), flag = "0")
solo_serie


# ahora buscamos agregarlos al numero_solo_serie dado por el enunciado 0209 para obtener todas las combinaciones
todas <- str_c(numero_serie,solo_serie)
todas
# con esto filtramos por los ultimos caracteres

solo_serieNum <- substr(todas, start = 5, stop = 7)   



suma <- 0
for (i in (1:nchar(todas))) {
  suma <- suma + as.integer(substr(todas,i,i))
  print(suma)
}
suma


# para ver que suma se repite mas podemos utilizar ma moda.
moda <- sort(table(suma),decreasing = T)
moda
# la suma que mas se repite es 24 un total de 75 veces.

