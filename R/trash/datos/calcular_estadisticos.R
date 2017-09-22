calcular.estadisticos <- function(datos, muestra, columna = 1, tolerancia = 30, replicas = 1000) {
  #Calcula los estadísticos principales
  media <- round(mean(datos[,columna]), 2)
  desest <- round(sd(datos[,columna]), 4)
  minimo <- round(min(datos[,columna]), 2)
  maximo <- round(max(datos[,columna]), 2)
  bootstrap <- FALSE
  if(muestra < tolerancia) { #Calcula bootstraping
    resultado <- calcular.boot(datos[,columna], replicas = replicas)
    media <- round(resultado$media, 2)
    desest <- round(resultado$desest, 2)
    bootstrap <- TRUE
  }
  retorno <- list(media = media,
                  desest = desest,
                  minimo = minimo,
                  maximo = maximo,
                  bootstrap = bootstrap)
  retorno
}
