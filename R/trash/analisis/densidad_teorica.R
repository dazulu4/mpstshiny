densidad.teorica <- function(datos, media, desest, muestra = 1000) {
  xt <- seq(from = min(datos), to = max(datos), length.out = muestra)
  yt <- dnorm(xt, mean = media, sd = desest)
  grafico <- lines(xt, yt, col = 'blue', lwd = 2)
  return(grafico)
}

