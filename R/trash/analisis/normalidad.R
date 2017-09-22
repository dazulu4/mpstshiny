library(moments)
library(nortest)

jarque.bera <- function(datos) {
  prueba <- jarque.test(datos)
  return(list(estadistico = prueba$statistic,
              valor.p = prueba$p.value))
}

shapiro.wilk <- function(datos) {
  prueba <- shapiro.test(datos)
  return(list(estadistico = prueba$statistic,
              valor.p = prueba$p.value))
}

anderson.darling <- function(datos) {
  prueba <- ad.test(datos)
  return(list(estadistico = prueba$statistic,
              valor.p = prueba$p.value))
}

#Test
datos <- rnorm(100)
jarque.bera(datos)
shapiro.wilk(datos)
anderson.darling(datos)
