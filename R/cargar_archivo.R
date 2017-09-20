cargar.archivo <- function(archivo, columna = 1, tolerancia = 30, replicas = 1000) {
  datos <- leer.csv(archivo)

  #Calcula total de registros cargados
  total <- length(datos[,columna])

  #Calcula total de registros nulos
  totalNA <- sum(is.na(datos[,columna]))
  datos <- na.omit(datos[,columna])

  #Calcula total de registros no numéricos
  datosNN <- suppressWarnings(as.numeric(as.character(datos)))
  totalNN <- sum(is.na(datosNN))
  datos <- na.omit(data.frame(datosNN))

  #Calcula total de registros numéricos
  totalNM <- length(datos[,columna])

  #Calcula los estadísticos principales
  media <- round(mean(datos[,columna]), 2)
  desest <- round(sd(datos[,columna]), 2)
  minimo <- round(min(datos[,columna]), 2)
  maximo <- round(max(datos[,columna]), 2)
  bootstrap <- FALSE
  if(totalNM < tolerancia) { #Calcula bootstraping
    resultado <- calcular.boot(datos[,columna], replicas = replicas)
    media <- round(resultado$media, 2)
    desest <- round(resultado$desest, 2)
    bootstrap <- TRUE
  }
  datos <- data.frame(muestra = datos[,columna])
  list(datos = datos,
       total = total,
       totalNM = totalNM,
       totalNN = totalNN,
       totalNA = totalNA,
       media = media,
       desest = desest,
       minimo = minimo,
       maximo = maximo,
       bootstrap = bootstrap)
}

