cuartil.cuartil <- function(datos) {
  retorno <- list(puntos = qqnorm(datos),
                  linea = qqline(datos))
  return(retorno)
}

#Test
datos <- rnorm(100, mean = 3, sd = 1.5)
qqplot <- cuartil.cuartil(datos)
qqplot$puntos
qqplot$linea
