serie.tiempo <- function(datos, inicio = c(1900, 1), frecuencia = 1) {
  retorno <- ts(datos,
                start = inicio,
                frequency = frecuencia)
  return(retorno)
}
