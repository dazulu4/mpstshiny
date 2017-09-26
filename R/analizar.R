######################################################################
### FUNCIONES PARA ANALISIS DE DATOS
######################################################################
generar.normales <- function(datos, bloques = "Sturges", media, desv, muestra = 1000) {
  xt <- seq(from = min(datos), to = max(datos), length.out = muestra)
  yt <- dnorm(xt, mean = media, sd = desv)
  # lines(xt, yt, col = "cornflowerblue", lwd = 2)

  densidad <- density(datos)
  # lines(densidad, col = "brown4", lwd = 2)

  grafico <- list(hist(datos,
                       probability = TRUE,
                       breaks = bloques,
                       col = "cornsilk2", #col = "#75AADB",
                       border = "cornsilk4",
                       main = "Histograma",
                       xlab = "Cuantiles",
                       ylab = "Densidad"),
                  lines(xt, yt, col = "cornflowerblue", lwd = 2),
                  lines(xt, yt, col = "cornflowerblue", lwd = 2),
                  legend(x = 'topright',
                         legend = c('Teórica', 'Empírica'),
                         fill = NULL,
                         col = c('cornflowerblue', 'brown4'),
                         border = "black",
                         bg = 'white',
                         lty = 'solid',
                         lwd = c(2, 2),
                         bty = "n",
                         horiz = FALSE,
                         merge = FALSE),
                  grid())
  return(grafico)
}

# densidad.teorica <- function(datos, media, desv, muestra = 1000) {
#   xt <- seq(from = min(datos), to = max(datos), length.out = muestra)
#   yt <- dnorm(xt, mean = media, sd = desv)
#   grafico <- lines(xt, yt, col = "cornflowerblue", lwd = 2)
#   return(grafico)
# }
#
# densidad.empirica <- function(datos) {
#   densidad <- density(datos)
#   grafico <- lines(densidad, col = "brown4", lwd = 2)
#   return(grafico)
# }

# densidad.leyenda <- function() {
#   legend(x = 'topright',
#          legend = c('Teórica', 'Empírica'),
#          fill = NULL,
#          col = c('cornflowerblue', 'brown4'),
#          border = "black",
#          bg = 'white',
#          lty = 'solid',
#          lwd = c(2, 2),
#          bty = "n",
#          horiz = FALSE,
#          merge = FALSE)
# }

densidad.acumulada <- function(datos) {
  grafico <- list(plot(ecdf(datos),
                       col = "black",
                       main = "Densidad Acumulada Empírica"),
                  grid())
  return(grafico)
}

cuartil.cuartil <- function(datos) {
  retorno <- list(puntos = qqnorm(datos,
                                  xlab = "Cuantiles Teóricos",
                                  ylab = "Muestra"),
                  linea = qqline(datos),
                  grid())
  return(retorno)
}

serie.tiempo <- function(datos) {
  retorno <- list(plot(datos,
                       col = "blue4",
                       #pch = 22,
                       type = "l",
                       main = "Serie de tiempo",
                       xlab = "Tiempo",
                       ylab = "Valores"),
                  grid())
  return(retorno)
}

generar.serie <- function(datos, inicio = "2000,1", frecuencia = 4) {
  inicio <- unlist(strsplit(inicio, ','))
  inicio1 <- ifelse(test = is.na(inicio[1]), yes = 2000, no = as.integer(inicio[1]))
  inicio2 <- ifelse(test = is.na(inicio[2]), yes = 1, no = as.integer(inicio[2]))
  datosSerie <<- ts(datos,
                    start = c(inicio1,inicio2),
                    frequency = frecuencia)
}
