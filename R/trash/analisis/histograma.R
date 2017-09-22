histograma <- function(datos, bloques = "Sturges", frecuencias = TRUE) {
  grafico <- hist(x = datos,
                  breaks = bloques,
                  freq = frecuencias,
                  probability = !frecuencias)
  return(grafico)
}

#Test
datos <- rnorm(100, mean = 3, sd = 1.5)
histograma(datos, frecuencias = FALSE)
densidad.teorica(datos, media = 3, desest = 1.5)
densidad.empirica(datos)
