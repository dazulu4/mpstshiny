######################################################################
### FUNCIONES PARA MODELADO DE DATOS
######################################################################

# FUNCIONES PRINCIPALES PARA CALCULAR LOS MODELOS
# DE REGRESIÓN LINEAL Y RETORNAR LOS GRÁFICOS DEL
# MODELADO DE TENDENCIA SIN/CON ESTACIONALIDAD, LA
# INFORMACIÓN DE DIAGNOSTICO Y EL RESUMEN CON LOS
# INDICADORES MÁS REPRESENTATIVOS DEL MODELADO

tendencia.opcion <- function(datos, modelo, pronostico, opcion, periodos = 20, nivel = 0.95) {
  switch(opcion,
         pronostico={
           graficar.pronostico.tend(datos, modelo, pronostico, periodos, nivel)
         },
         diagnostico={
           graficar.diagnostico.tend(datos, modelo)
         },
         resumen={
           resumir.diagnostico.tend(modelo)
         },
         {
           return()
         }
  )
}

graficar.pronostico.tend <- function(datos, modelo, pronostico, periodos, nivel) {
  tiempo <- seq(1:length(datos))

  #### TODO Se debe agregar la graficación del pronostico
  #### el cual se encuentra guardado en la variable "pronostico"
  #### calculado en la función calcular.regresion

  list(real = plot(tiempo, datos, type = "o", col = "black", lwd = 2, pch = 20),
       pron = lines(modelo$fitted.values, col = "red", lwd = 2),
       leyenda = legend("topleft",
                        c("Real","Pronostico"),
                        lwd = c(2, 2),
                        col = c('black','red'),
                        bty = "n"),
       grid())
}

graficar.diagnostico.tend <- function(datos, modelo) {
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

resumir.diagnostico.tend <- function(modelo) {
  list("Measures:" = accuracy(modelo),
       "Summary:" = summary(modelo))
}

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

calcular.regresion <- function(datos, estacion = FALSE, grado = 1) {
  longitud <- length(datos)
  valores <- seq(1:longitud)
  tiempo <- matrix(rep(0, longitud * grado), nrow = longitud, ncol = grado)
  for(i in 1:grado) {
    tiempo[,i] <- valores^i
  }
  # It = Variables indicadoras en función del tiempo
  if(estacion) {
    It <- seasonaldummy(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo)
  }

  #### TODO: Elaborar el pronóstico con forecast para el modelado
  #### de regresión lineal y retornar los gráficos correspondientes
  #### El resultado se debe guardar en la variable global resultPron

  # resultPron <<- forecast(modelo, h = periodos, level = nivel)
  # print(pron)

  resultPron <<- NULL
}

######################################################################

# FUNCIONES PRINCIPALES PARA CALCULAR LOS MODELO
# DE HOLT WINTERS Y RETORNAR LOS GRÁFICOS DEL
# MODELADO, LA INFORMACIÓN DE DIAGNOSTICO Y EL RESUMEN
# CON LOS INDICADORES MÁS REPRESENTATIVOS DEL MODELADO

holtwinters.opcion <- function(datos, modelo, pronostico, opcion, periodos = 20, nivel = 0.95) {
  switch(opcion,
         pronostico={
           graficar.pronostico.hw(datos, modelo, pronostico, periodos, nivel)
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

graficar.pronostico.hw <- function(datos, modelo, pronostico, periodos, nivel) {

  #### TODO Se debe agregar la graficación del pronostico
  #### el cual se encuentra guardado en la variable "pronostico"
  #### calculado en la función calcular.holtwinters

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
  residuals = residuals(modelo)
  tiempo <- seq(1:length(residuals))

  list(par(mfrow=c(2,2)),
       r = plot(tiempo, residuals, type='b', ylab='', main="Residuales", col="red"),
       l = abline(h=0, lty=2),
       d = plot(density(residuals), xlab='x', main= 'Densidad residuales', col="red"),
       qp = qqnorm(residuals), ql = qqline(residuals, col=2),
       c = acf(residuals, ci.type="ma", 60),
       grid())
}

resumir.diagnostico.hw <- function(modelo) {
  formula <- as.character(modelo$call)
  summary <- data.frame(alpha = modelo$alpha[[1]],
                        beta = modelo$beta[[1]],
                        gamma = modelo$gamma[[1]],
                        seasonal = modelo$seasonal,
                        SSE = modelo$SSE,
                        call = paste(formula[1], "(formula = ", formula[2], ")", sep = ""))
  coefficients <- data.frame(values = modelo$coefficients)
  list("Measures" = list(),#accuracy(modelo),
       "Summary:" = summary,
       "Coefficients:" = coefficients)
}

calcular.holtwinters <- function(datos) {
  modeloPron <<- HoltWinters(datos)

  #### TODO: Elaborar el pronóstico con forecast para el modelado
  #### de Holt Winters y retornar los gráficos correspondientes
  #### El resultado se debe guardar en la variable global resultPron

  resultPron <<- NULL
}

######################################################################

# FUNCIONES PRINCIPALES PARA CALCULAR LOS MODELO
# DE AUTO ARIMA Y RETORNAR LOS GRÁFICOS DEL
# MODELADO, LA INFORMACIÓN DE DIAGNOSTICO Y EL RESUMEN
# CON LOS INDICADORES MÁS REPRESENTATIVOS DEL MODELADO

arima.opcion <- function(datos, modelo, pronostico, opcion, periodos = 20, nivel = 0.95) {
  switch(opcion,
         pronostico={
           graficar.pronostico.arima(datos, modelo, pronostico, periodos, nivel)
         },
         diagnostico={
           graficar.diagnostico.arima(datos, modelo)
         },
         resumen={
           resumir.diagnostico.arima(modelo)
         },
         {
           return()
         }
  )
}

graficar.pronostico.arima <- function(datos, modelo, pronostico, periodos, nivel) {

  #### TODO Se debe agregar la graficación del pronostico
  #### el cual se encuentra guardado en la variable "pronostico"
  #### calculado en la función calcular.arima

  list(real = plot(modelo$x, type = "o", col = "black", lwd = 2, pch = 20),
       pron = lines(modelo$fitted, col = "red", lwd = 2),
       leyenda = legend("topleft",
                        c("Real","Pronostico"),
                        lwd = c(2, 2),
                        col = c('black','red'),
                        bty = "n"),
       grid())
}

graficar.diagnostico.arima <- function(datos, modelo) {
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

resumir.diagnostico.arima <- function(modelo) {
  summary(modelo)
}

calcular.arima <- function(datos) {
  modeloPron <<- auto.arima(datos)

  #### TODO: Elaborar el pronóstico con forecast para el modelado
  #### de Auto ARIMA y retornar los gráficos correspondientes
  #### El resultado se debe guardar en la variable global resultPron

  resultPron <<- NULL
}
