######################################################################
### FUNCIONES PARA CARGA DE ARCHIVOS
######################################################################
cargar.archivo <- function(archivo,
                           encabezado = FALSE,
                           separador = ',',
                           comillas = '"') {
  datos <<- read.csv(file = archivo,
                     header = encabezado,
                     sep = separador,
                     quote = comillas,
                     fill = TRUE)
}

calcular.totales <- function(columna = 1) {
  total <- length(datos[,columna])
  totalNA <- sum(is.na(datos[,columna]))
  datos <<- na.omit(datos[,columna])
  datosNN <- suppressWarnings(as.numeric(as.character(datos)))
  totalNN <- sum(is.na(datosNN))
  datos <<- na.omit(data.frame(datosNN))
  totalNM <- length(datos[,columna])
  totales <<- list("Cargados" = total,
                   "Numéricos" = totalNM,
                   "No numéricos" = totalNN,
                   "Valores nulos" = totalNA)

  datosVec <<- datos[,columna]
  datos <<- data.frame(datosVec)
}

calcular.estadisticos <- function(columna = 1, tolerancia = 30, replicas = 1000) {
  media <- round(mean(datos[,columna]), 2)
  desv <- round(sd(datos[,columna]), 4)
  minimo <- round(min(datos[,columna]), 2)
  maximo <- round(max(datos[,columna]), 2)
  bootstrap <- FALSE
  if(length(datos[,columna]) < tolerancia) {
    resultado <- calcular.boot(datos[,columna], replicas)
    media <- round(resultado$media, 2)
    desv <- round(resultado$desv, 2)
    bootstrap <- TRUE
  }
  estadisticos <<- list(Media = media,
                        "Desv. estándar" = desv,
                        "Mínimo" = minimo,
                        "Máximo" = maximo,
                        Bootstrap = bootstrap)
}

calcular.media <- function(x, d) {
  return(mean(x[d]))
}

calcular.boot <- function(x, r = 1000) {
  resultado <- boot(x, statistic = calcular.media, R = r)
  list(media = resultado$t0,
       desv = sd(resultado$t[,1]))
}
