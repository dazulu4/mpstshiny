######################################################################
### FUNCIONES PARA CARGA DE ARCHIVOS
######################################################################

cargar.archivo <- function(archivo,
                           encabezado = FALSE,
                           separador = ',',
                           comillas = '"') {
  datos <- read.csv(file = archivo,
                     header = encabezado,
                     sep = separador,
                     quote = comillas,
                     fill = TRUE)
  return(datos)
}

calcular.totales <- function(datos, columna = 1) {
  total <- length(datos[,columna])
  totalNA <- sum(is.na(datos[,columna]))
  datos <- na.omit(datos[,columna])
  datosNN <- suppressWarnings(as.numeric(as.character(datos)))
  totalNN <- sum(is.na(datosNN))
  datos <- na.omit(data.frame(datosNN))
  totalNM <- length(datos[,columna])
  totales <- data.frame(Cargados = total,
                        Numericos = totalNM,
                        No_Numericos = totalNN,
                        Valores_Nulos = totalNA)
  datosVec <- datos[,columna]
  datos <- data.frame(datosVec)
  return(list(datos = datos,
              vector = datosVec,
              totales = totales))
}

calcular.estadisticos <- function(datos, columna = 1, tolerancia = 30, replicas = 1000) {
  media <- round(mean(datos[,columna]), decimales)
  desv <- round(sd(datos[,columna]), decimales)
  minimo <- round(min(datos[,columna]), decimales)
  maximo <- round(max(datos[,columna]), decimales)
  bootstrap <- FALSE
  if(length(datos[,columna]) < tolerancia) {
    resultado <- calcular.boot(datos[,columna], replicas)
    media <- round(resultado$media, decimales)
    desv <- round(resultado$desv, decimales)
    bootstrap <- TRUE
  }
  estadisticos <- data.frame(Media = media,
                              Desv_Estandar = desv,
                              Minimo = minimo,
                              Maximo = maximo,
                              Bootstrap = ifelse(bootstrap, "Si", "No"))
  return(estadisticos)
}

calcular.media <- function(x, d) {
  return(mean(x[d]))
}

calcular.boot <- function(x, r = 1000) {
  resultado <- boot(x, statistic = calcular.media, R = r)
  list(media = resultado$t0,
       desv = sd(resultado$t[,1]))
}

