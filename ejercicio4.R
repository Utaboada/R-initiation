
matriz <-source("matriz.R")
matriz <- matriz$value

#no contabilizaos la primera columna porque representa los sujetos
matrix <- matriz[,-1]
matrix
#hacemos una visualizacion de la columna que representa la semana 7
semana7 <- matrix[,7]
semana7

# tabulate te devuelve las frecuencias absolutas, es decir cuantas veces se repite cada puntuación de escozcor
frec_abs <- tabulate(semana7,nbins = 4)

#calculamos la  suma frecuencia absoluta
sum_frec_abs = sum(frec_abs)
sum_frec_abs


frec_rel_ <- function(semana_vec){
  frecuencys<-  tabulate(semana_vec, nbins = 4)
  semana_vec <- c(frecuencys[1]/16, 1-(frecuencys[1]/sum_frec_abs),frecuencys[2]/sum_frec_abs,1-(frecuencys[2]/sum_frec_abs),
                  (frecuencys[3]/sum_frec_abs),1-(frecuencys[3]/sum_frec_abs),(frecuencys[4]/sum_frec_abs),1- (frecuencys[4]/sum_frec_abs))#, 1-((frec_abs[i+1])/sum_frec_abs),((frec_abs[i+2])/sum_frec_abs), 1-((frec_abs[i+2])/sum_frec_abs) )
  
  return(semana_vec)
  
}

#frecuencia relativa semana 7

frec_rel_(semana7)


# Aplicarlo a todas las columns de  matrix con apply

todas <- apply(X = matrix, FUN = frec_rel_ ,MARGIN =2)

#barplot()
barplot(todas, col = c ("black", "white"))

# modificaciones del barplots

labels_semanas <- c("s1","s2","s3","s4","s5","s6","s7")
grafico<- barplot(todas, col = c ("red", "white"), 
        main="Evolución de la sensación de ardor por semanas",
        col.axis = "blue")
axis(at=grafico, labels=labels_semanas,side=3,col.axis = "blue" )
