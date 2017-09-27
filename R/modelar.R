######################################################################
### FUNCIONES PARA MODELADO DE DATOS
######################################################################

tendencia.funcion <- function(datos, funcion, estacion = FALSE) {
  switch(funcion,
         lineal={
           calcular.regresion(datos, estacion, 1)
         },
         quadratic={
           calcular.regresion(datos, estacion, 2)
         },
         cubic={
           calcular.regresion(datos, estacion, 3)
         },
         gfour={
           calcular.regresion(datos, estacion, 4)
         },
         gfive={
           calcular.regresion(datos, estacion, 5)
         },
         {
           return()
         }
  )
}

calcular.estacion <- function(datos) {
  seasonaldummy(datos)
}

calcular.regresion <- function(datos, estacion = FALSE, grado = 1) {
  longitud <- length(datos)
  valores <- seq(1:longitud)
  tiempo <- matrix(rep(0, longitud * grado), nrow = longitud, ncol = grado)
  for(i in 1:grado) {
    tiempo[,i] <- valores^i
  }
  # It = Variables indicadoras en función del tiempo
  if(estacion) {
    It <- calcular.estacion(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo)
  }
}

calcular.holtwinters <- function(datos) {
  modeloPron <<- HoltWinters(datos)
}

# FUNCIONES DE MODELADO DE BAJO NIVEL - PROCESAMIENTO GENERAL
tendencia.opcion <- function(datos, modelo, opcion) {
  switch(opcion,
         pronostico={
           graficar.tendencia(datos, modelo)
         },
         diagnostico={
           graficar.diagnostico(datos, modelo)
         },
         resumen={
           resumir.diagnostico(modelo)
         },
         {
           return()
         }
  )
}

holtwinters.opcion <- function(datos, modelo, opcion) {
  switch(opcion,
         pronostico={
           graficar.tendencia.hw(datos, modelo)
         },
         diagnostico={
           graficar.diagnostico.hw(datos, modelo)
         },
         resumen={
           resumir.diagnostico.hw(modelo)
         },
         {
           return()
         }
  )
}

graficar.tendencia <- function(datos, modelo) {
  tiempo <- seq(1:length(datos))
  list(real = plot(tiempo, datos, type = "o", col = "black", lwd = 2, pch = 20),
       pron = lines(modelo$fitted.values, col = "red", lwd = 2),
       leyenda = legend("topleft",
                        c("Real","Pronostico"),
                        lwd = c(2, 2),
                        col = c('black','red'),
                        bty = "n"),
       grid())
}

graficar.diagnostico <- function(datos, modelo) {
  tiempo <- seq(1:length(datos))
  residual = modelo$residuals
  list(tablero = par(mfrow=c(2,2)),
       residual = plot(tiempo, residual, type='b', ylab='', main="Residuales", col="red"),
       rlinea = abline(h=0, lty=2),
       densidad = plot(density(residual), xlab='x', main= 'Densidad residuales', col="red"),
       qpuntos = qqnorm(residual),
       qlinea = qqline(residual, col=2),
       correl = acf(residual, ci.type="ma", 60),
       grid())
}

resumir.diagnostico <- function(modelo) {
  summary(modelo)
}

graficar.tendencia.hw <- function(datos, modelo) {
  list(real = plot(modelo$x, type = "o", col = "black", lwd = 2, pch = 20),
       pron = lines(modelo$fitted[,1], col = "red", lwd = 2),
       leyenda = legend("topleft",
                        c("Real","Pronostico"),
                        lwd = c(2, 2),
                        col = c('black','red'),
                        bty = "n"),
       grid())
}

graficar.diagnostico.hw <- function(datos, modelo) {
  return()
}

resumir.diagnostico.hw <- function(modelo) {
  str(modelo)
}

