densidad.empirica <- function(datos) {
  densidad <- density(datos)
  grafico <- lines(densidad, col = 'red', lwd = 2)
  return(grafico)
}
