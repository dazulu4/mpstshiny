calcular.totales <- function(datos, columna = 1) {
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

  retorno <- list(datos = data.frame(datos[,columna]),
                  total = total,
                  totalNM = totalNM,
                  totalNN = totalNN,
                  totalNA = totalNA)
  retorno
}
