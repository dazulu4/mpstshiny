cargar.archivo <- function(archivo, columna = 1, tolerancia = 30, replicas = 1000) {
  datos <- leer.csv(archivo)
  retorno <- data.frame(datos[,columna])
  retorno
}

#Test
#archivo <- paste(getwd(), '/tests/datos/cargar_archivo.csv', sep = '')
#datos <- cargar.archivo(archivo)
#datos
#totales <- calcular.totales(datos)
#estadisticos <- calcular.estadisticos(totales$datos, totales$totalNM)
