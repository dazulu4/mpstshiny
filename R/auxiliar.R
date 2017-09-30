calcular.regresion.1 <- function(datos, estacion, tiempos, periodos) {
  tiempo1 <- tiempos
  It <- calcular.estacion(datos)
  modelo <- NULL
  nd.df <- NULL
  nd.1 <- seq(from = (length(tiempo1) + 1),
              by = 1,
              length.out = periodos)
  # nd.c <- calcular.ciclo.inicial(datos, periodos, It)
  if(!estacion) {
    modelo <- lm(formula = datos ~ tiempo1)
    nd.df <- data.frame(tiempo1 = nd.1)
  } else {
    modelo <- lm(formula = datos ~ tiempo1 + It)
    nd.df <- data.frame(tiempo1 = nd.1,
                        It = It[1:periodos,])
  }
  return(list(modelo = modelo,
              nuevo = nd.df))
}
calcular.regresion.2 <- function(datos, estacion, tiempos, periodos) {
  tiempo1 <- tiempos
  tiempo2 <- tiempos^2
  It <- calcular.estacion(datos)
  modelo <- NULL
  nd.df <- NULL
  nd.1 <- seq(from = (length(tiempo1) + 1),
              by = 1,
              length.out = periodos)
  # nd.c <- calcular.ciclo.inicial(datos, periodos, It)
  if(!estacion) {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2))
  } else {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + It)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        It = It[1:periodos,])
  }
  return(list(modelo = modelo,
              nuevo = nd.df))
}
calcular.regresion.3 <- function(datos, estacion, tiempos, periodos) {
  tiempo1 <- tiempos
  tiempo2 <- tiempos^2
  tiempo3 <- tiempos^3
  It <- calcular.estacion(datos)
  modelo <- NULL
  nd.df <- NULL
  nd.1 <- seq(from = (length(tiempo1) + 1),
              by = 1,
              length.out = periodos)
  # nd.c <- calcular.ciclo.inicial(datos, periodos, It)
  if(!estacion) {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + tiempo3)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        tiempo3 = (nd.1^3))
  } else {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + tiempo3 + It)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        tiempo3 = (nd.1^3),
                        It = It[1:periodos,])
  }
  return(list(modelo = modelo,
              nuevo = nd.df))
}
calcular.regresion.4 <- function(datos, estacion, tiempos, periodos) {
  tiempo1 <- tiempos
  tiempo2 <- tiempos^2
  tiempo3 <- tiempos^3
  tiempo4 <- tiempos^4
  It <- calcular.estacion(datos)
  modelo <- NULL
  nd.df <- NULL
  nd.1 <- seq(from = (length(tiempo1) + 1),
              by = 1,
              length.out = periodos)
  # nd.c <- calcular.ciclo.inicial(datos, periodos, It)
  if(!estacion) {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + tiempo3 + tiempo4)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        tiempo3 = (nd.1^3),
                        tiempo4 = (nd.1^4))
  } else {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + tiempo3 + tiempo4 + It)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        tiempo3 = (nd.1^3),
                        tiempo4 = (nd.1^4),
                        It = It[1:periodos,])
  }
  return(list(modelo = modelo,
              nuevo = nd.df))
}
calcular.regresion.5 <- function(datos, estacion, tiempos, periodos) {
  tiempo1 <- tiempos
  tiempo2 <- tiempos^2
  tiempo3 <- tiempos^3
  tiempo4 <- tiempos^4
  tiempo5 <- tiempos^5
  It <- calcular.estacion(datos)
  modelo <- NULL
  nd.df <- NULL
  nd.1 <- seq(from = (length(tiempo1) + 1),
              by = 1,
              length.out = periodos)
  # nd.c <- calcular.ciclo.inicial(datos, periodos, It)
  if(!estacion) {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + tiempo3 + tiempo4 + tiempo5)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        tiempo3 = (nd.1^3),
                        tiempo4 = (nd.1^4),
                        tiempo5 = (nd.1^5))
  } else {
    modelo <- lm(formula = datos ~ tiempo1 + tiempo2 + tiempo3 + tiempo4 + tiempo5 + It)
    nd.df <- data.frame(tiempo1 = nd.1,
                        tiempo2 = (nd.1^2),
                        tiempo3 = (nd.1^3),
                        tiempo4 = (nd.1^4),
                        tiempo5 = (nd.1^5),
                        It = It[1:periodos,])
  }
  return(list(modelo = modelo,
              nuevo = nd.df))
}

calcular.estacion <- function(datos) {
  seasonaldummy(datos)
}

### FUNCIÓN QUE GENERA EL RESULTADO DE PRONOSTICO EN FORMATO DATA.FRAME
generar.resultado.pronostico <- function(resto, pronostico, modelo = "LM") {
  tiem <- fortify(resto)$Index
  real <- round(fortify(resto)$Data, 4)
  pron <- round(pronostico$mean, 4)
  linf <- round(pronostico$lower, 4)
  lsup <- round(pronostico$upper, 4)
  errc <- round(pron - real, 4)
  err2 <- round(errc^2, 4)

  retorno <-
    data.frame(
      "Fecha" = tiem,
      "Real" = real,
      "Pron" = pron,
      "LimInf" = linf,
      "LimSup" = lsup,
      "ErrCuad" = err2
    )

  # retorno <- retorno[order("Fecha"),]

  return(retorno)

}

### FUNCIÓN QUE GENERA EL RESULTADO DE RESUMEN DE PRESICIÓN EN DATA.FRAME
generar.resultado.accuracy <- function(resto, pronostico) {
  rp <- generar.resultado.pronostico(resto, pronostico)
  df <- data.frame(accuracy(pronostico))
  df["SSE"] <- round(sum(rp["ErrCuad"]), 4)
  return(df)
}

generar.resultado.criterio <- function(modelo) {
  df <- data.frame(AIC = AIC(modelo),
                   BIC = BIC(modelo))
  return(df)
}

