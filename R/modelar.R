######################################################################
### FUNCIONES PARA MODELADO DE DATOS
######################################################################

# FUNCIONES PRINCIPALES PARA CALCULAR LOS MODELOS
# DE REGRESIÓN LINEAL Y RETORNAR LOS GRÁFICOS DEL
# MODELADO DE TENDENCIA SIN/CON ESTACIONALIDAD, LA
# INFORMACIÓN DE DIAGNOSTICO Y EL RESUMEN CON LOS
# INDICADORES MÁS REPRESENTATIVOS DEL MODELADO

tendencia.opcion <- function(serie, datos, resto, modelo, pronostico, opcion, periodos = 20) {
  switch(opcion,
         pronostico={
           graficar.pronostico.tend(serie, datos, modelo, pronostico, periodos)
         },
         resultado={
           resumir.resultado.tend(resto, pronostico)
         },
         accuracy={
           accuracy.resultado.tend(resto, pronostico)
         },
         diagnostico={
           graficar.diagnostico.tend(datos, modelo)
         },
         stats={
           stats.diagnostico.tend(modelo)
         },
         {
           return()
         }
  )
}

graficar.pronostico.tend <- function(serie, datos, modelo, pronostico, periodos) {
  # tiempo <- seq(1:length(datos))
  # list(real = plot(tiempo, datos, type = "o", col = "black", lwd = 2, pch = 20),
  #      pron = lines(modelo$fitted.values, col = "red", lwd = 2),
  #      leyenda = legend("topleft",
  #                       c("Real","Pronostico"),
  #                       lwd = c(2, 2),
  #                       col = c('black','red'),
  #                       bty = "n"),
  #      grid())

  # tiempo.a <- seq(1:length(datos))
  # tiempo.n <- seq(from = (length(tiempo.a) + 1),
  #                 by = 1,
  #                 length.out = periodos)

  na.n <- rep(NA, periodos)
  na.a <- rep(NA, length(pronostico$x) - 1)
  ult.a <- pronostico$fitted[(length(pronostico$fitted))]

  pronos.df <- data.frame(#Index = c(tiempo.a, tiempo.n),
                          Index = time(serie),
                          # Data = c(pronostico$x, na.n),
                          Data = serie,
                          Fitted = c(pronostico$fitted, na.n),
                          Forecast = c(na.a, ult.a, pronostico$mean),
                          Lower = c(na.a, ult.a, pronostico$lower),
                          Upper = c(na.a, ult.a, pronostico$upper))

  ggplot(data = pronos.df) + #xlab("Tiempo") + ylab("Valores") +
    geom_line(mapping = aes_string(x = 'Index', y = 'Data')) +
    geom_line(mapping = aes_string(x = 'Index', y = 'Fitted'), colour='red') +
    geom_line(mapping = aes_string(x = 'Index', y = 'Forecast'), colour='blue') +
    geom_ribbon(mapping = aes_string(x = 'Index', ymin = 'Lower', ymax = 'Upper'), alpha = 0.5)+
    labs( x="Tiempo", y="Valores")+ggtitle(tipo_modelo[[1]])+ #ggtitle("Modelo")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))


}

graficar.diagnostico.tend <- function(datos, modelo) {
  # list(tablero = par(mfrow=c(2,2)),
  #      residual = plot(tiempo, residual, type='b', ylab='', main="Residuales", col="red"),
  #      rlinea = abline(h=0, lty=2),
  #      densidad = plot(density(residual), xlab='x', main= 'Densidad residuales', col="red"),
  #      qpuntos = qqnorm(residual),
  #      qlinea = qqline(residual, col=2),
  #      correl = acf(residual, ci.type="ma", 60),
  #      grid())

  # d <- data.frame(tiempo=seq(1:length(datos)), datos = modelo$model$datos, residuo = residuals(modelo))
  d <- data.frame(tiempo=seq(1:length(datos)), residuo = residuals(modelo))
  size_fonts.x <- 18
  size_fonts.y <- 12

  ####Residuales##############
  gresidual <- ggplot(d, aes(x=tiempo, y=residuo), type = "dashed")  +geom_smooth(method = "lm", se = FALSE, color = "lightgrey")
  # gresidual <- gresidual + geom_segment(aes(xend = tiempo, yend = modelo$model$datos), alpha = .2)
  gresidual <- gresidual + geom_point(aes(color = residuo))
  gresidual <- gresidual + scale_color_gradient2(low = "blue", mid = "white", high = "red")
  # gresidual <- gresidual + guides(color = FALSE)
  # gresidual <- gresidual + geom_point(aes(y = modelo$model$datos), shape = 1)
  gresidual <- gresidual +labs( x="Tiempo", y="")+ggtitle("Residuales")
  gresidual <- gresidual + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gresidual <- gresidual + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Normal Q-Q plot
  gqqplot<- ggqqplot(d$residuo, xlab = "Cuantiles Teóricos", ylab = "Muestra", color = "blue", ggtheme=theme_gray())+ggtitle("Normal Q-Q Plot")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Densidad Residual
  gdresidual <- ggplot(d, aes(x=residuo)) + geom_density(fill="blue", colour=NA, alpha=.2) +   geom_line(stat = "density")
  gdresidual <-  gdresidual +  expand_limits(y = 0) + ggtitle("Densidad de residuales") + theme(plot.title = element_text(hjust = 0.5))
  gdresidual <-  gdresidual +  xlab("Residual") + ylab("Densidad")
  gdresidual <-  gdresidual + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gdresidual <-  gdresidual + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Correlacion Residual
  bacf <- acf(d$residuo, ci.type="ma", 60)
  str(bacf$type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  ci2 = qnorm((1 + .95)/2)/sqrt(length(rnorm(bacf$acf)))

  gcorrel <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0))
  gcorrel <- gcorrel +  geom_segment(mapping = aes(xend = lag, yend = 0))
  gcorrel <- gcorrel + geom_hline(yintercept = c(ci2, -ci2), color = "purple", linetype = "dashed")+ggtitle("Series Residual")
  gcorrel <- gcorrel +  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gcorrel <- gcorrel +  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  multiplot(gresidual, gqqplot, gdresidual, gcorrel, cols = 2)
}

resumir.resultado.tend <- function(resto, pronostico) {
  generar.resultado.pronostico(resto,pronostico)
}

accuracy.resultado.tend <- function(resto, pronostico) {
  generar.resultado.accuracy(resto, pronostico)
}

stats.diagnostico.tend <- function(modelo) {
  summary <- summary(modelo)
  criteria <- data.frame(AIC = AIC(modelo),
                         BIC = BIC(modelo))
  row.names(criteria) <- NULL
  list("Summary" = summary,
       "Criteria" = criteria)
}

tendencia.funcion <- function(datos, funcion, estacion = FALSE, periodos = 20, nivel = 95) {
  switch(funcion,
         lineal={
           calcular.regresion(datos, estacion, "grado1", periodos, nivel)
         },
         quadratic={
           calcular.regresion(datos, estacion, "grado2", periodos, nivel)
         },
         cubic={
           calcular.regresion(datos, estacion, "grado3", periodos, nivel)
         },
         gfour={
           calcular.regresion(datos, estacion, "grado4", periodos, nivel)
         },
         gfive={
           calcular.regresion(datos, estacion, "grado5", periodos, nivel)
         },
         {
           return()
         }
  )
}

calcular.regresion <- function(datos, estacion = FALSE, grado = "grado1", periodos = 20, nivel = 95) {
  # tiempo <- matrix(rep(0, longitud * grado), nrow = longitud, ncol = grado)
  # for(i in 1:grado) {
  #   tiempo[,i] <- valores^i
  # }

  tiempos <- seq(1:length(datos))

  valores <- NULL
  switch(grado,
         grado1={ valores <- calcular.regresion.1(datos, estacion, tiempos, periodos) },
         grado2={ valores <- calcular.regresion.2(datos, estacion, tiempos, periodos) },
         grado3={ valores <- calcular.regresion.3(datos, estacion, tiempos, periodos) },
         grado4={ valores <- calcular.regresion.4(datos, estacion, tiempos, periodos) },
         grado5={ valores <- calcular.regresion.5(datos, estacion, tiempos, periodos) },
         { valores <- NULL })

  if(!is.null(valores)) {
    valor.pronos <- forecast(valores$modelo,
                             level = nivel,
                             newdata = valores$nuevo)

    return(list(modelo = valores$modelo,
                pronos = valor.pronos))
  } else {
    return(list(modelo = NULL,
                pronos = NULL))
  }

}

######################################################################

# FUNCIONES PRINCIPALES PARA CALCULAR LOS MODELO
# DE HOLT WINTERS Y RETORNAR LOS GRÁFICOS DEL
# MODELADO, LA INFORMACIÓN DE DIAGNOSTICO Y EL RESUMEN
# CON LOS INDICADORES MÁS REPRESENTATIVOS DEL MODELADO

holtwinters.opcion <- function(serie, datos, resto, modelo, pronostico, opcion, periodos = 20, nivel = 95) {
  switch(opcion,
         pronostico={
           graficar.pronostico.hw(serie, datos, modelo, pronostico, periodos, nivel)
         },
         resultado={
           resumir.resultado.hw(resto, pronostico)
         },
         accuracy={
           accuracy.resultado.hw(resto, pronostico)
         },
         diagnostico={
           graficar.diagnostico.hw(datos, modelo)
         },
         stats={
           stats.diagnostico.hw(modelo)
         },
         {
           return()
         }
  )
}

graficar.pronostico.hw <- function(serie, datos, modelo, pronostico, periodos, nivel) {
  # list(real = plot(modelo$x, type = "o", col = "black", lwd = 2, pch = 20),
  #      pron = lines(modelo$fitted[,1], col = "red", lwd = 2),
  #
  #      leyenda = legend("topleft",
  #                       c("Real","Pronostico"),
  #                       lwd = c(2, 2),
  #                       col = c('black','red'),
  #                       bty = "n"),
  #      grid())

  na.n <- rep(NA, periodos)
  na.a <- rep(NA, length(pronostico$x) - 1)
  ult.a <- pronostico$fitted[(length(pronostico$fitted))]

  pronos.df <- data.frame(Index = time(serie),
                          # Data = c(pronostico$x, na.n),
                          Data = serie,
                          Fitted = c(pronostico$fitted, na.n),
                          Forecast = c(na.a, ult.a, pronostico$mean),
                          Lower = c(na.a, ult.a, pronostico$lower),
                          Upper = c(na.a, ult.a, pronostico$upper))

  ggplot(data = pronos.df) + xlab("Tiempo") + ylab("Valores") +
    geom_line(mapping = aes_string(x = 'Index', y = 'Data')) +
    geom_line(mapping = aes_string(x = 'Index', y = 'Fitted'), colour='red') +
    geom_line(mapping = aes_string(x = 'Index', y = 'Forecast'), colour='blue') +
    geom_ribbon(mapping = aes_string(x = 'Index', ymin = 'Lower', ymax = 'Upper'), alpha = 0.5)+
    ggtitle(tipo_modelo[[1]])+ #ggtitle("Modelo")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))


}


graficar.diagnostico.hw <- function(datos, modelo) {

  ts_modelo_fit<- fortify(modelo$fitted)
  residuo = residuals(modelo)
  tiempo <- seq(1:length(ts_modelo_fit))

  # list(par(mfrow=c(2,2)),
  #      r = plot(tiempo, residuals, type='b', ylab='', main="Residuales", col="red"),
  #      l = abline(h=0, lty=2),
  #      d = plot(density(residuals), xlab='x', main= 'Densidad residuales', col="red"),
  #      qp = qqnorm(residuals), ql = qqline(residuals, col=2),
  #      c = acf(residuals, ci.type="ma", 60),
  #      grid())

  size_fonts.x <- 18
  size_fonts.y <- 12

  d <- data.frame(tiempo=seq(1:length(ts_modelo_fit$xhat)), datos= ts_modelo_fit$xhat, residuo = residuo)

  ####Residuales##############
  gresidual <- ggplot(d, aes(x=tiempo, y=residuo), type = "dashed")  +geom_smooth(method = "lm", se = FALSE, color = "lightgrey")
  # # gresidual <- gresidual + geom_segment(aes(xend = tiempo, yend = modelo$model$datos), alpha = .2)
  gresidual <- gresidual + geom_point()
  gresidual <- gresidual + scale_color_gradient2(low = "blue", mid = "white", high = "red")
  # gresidual <- gresidual + guides(color = FALSE)
  # gresidual <- gresidual + geom_point(aes(y = modelo$model$datos), shape = 1)
  gresidual <- gresidual +labs( x="Tiempo", y="")+ggtitle("Residuales")
  gresidual <- gresidual + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gresidual <- gresidual + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  #####Normal Q-Q plot

  gqqplot<- ggqqplot(d$residuo, xlab = "Cuantiles Teóricos", ylab = "Muestra", color = "blue", ggtheme=theme_gray())+ggtitle("Normal Q-Q Plot")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Densidad Residual
  gdresidual <- ggplot(d$fitted, aes(x=residuo)) + geom_density(fill="blue", colour=NA, alpha=.2) +   geom_line(stat = "density")
  gdresidual <-  gdresidual +  expand_limits(y = 0) + ggtitle("Densidad de residuales") + theme(plot.title = element_text(hjust = 0.5))
  gdresidual <-  gdresidual +  xlab("Residual") + ylab("Densidad")
  gdresidual <-  gdresidual + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gdresidual <-  gdresidual + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Correlacion Residual
  bacf <- acf(d$residuo, ci.type="ma", 60)
  str(bacf$type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  ci2 = qnorm((1 + .95)/2)/sqrt(length(rnorm(bacf$acf)))

  gcorrel <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0))
  gcorrel <- gcorrel +  geom_segment(mapping = aes(xend = lag, yend = 0))
  gcorrel <- gcorrel + geom_hline(yintercept = c(ci2, -ci2), color = "purple", linetype = "dashed")+ggtitle("Series Residual")
  gcorrel <- gcorrel +  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gcorrel <- gcorrel +  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  multiplot(gresidual, gqqplot, gdresidual, gcorrel, cols = 2)

}

resumir.resultado.hw <- function(resto, pronostico) {
  generar.resultado.pronostico(resto,pronostico,"HW")
}

accuracy.resultado.hw <- function(resto, pronostico) {
  generar.resultado.accuracy(resto, pronostico)
}

stats.diagnostico.hw <- function(modelo) {
  formula <- as.character(modelo$call)
  summary <- data.frame(alpha = modelo$alpha[[1]],
                        beta = modelo$beta[[1]],
                        gamma = modelo$gamma[[1]],
                        seasonal = modelo$seasonal,
                        SSE = modelo$SSE,
                        call = paste(formula[1], "(formula = ", formula[2], ")", sep = ""))
  coefficients <- data.frame(values = modelo$coefficients)
  # criteria <- data.frame(AIC = AIC(modelo),
  #                        BIC = BIC(modelo))
  # row.names(criteria) <- NULL
  list("Summary:" = summary,
       "Coefficients:" = coefficients)
       # "Criteria" = criteria)
}

calcular.holtwinters <- function(datos, periodos = 20, nivel = 95) {
  modeloPron <- HoltWinters(datos)
  resultPron <- forecast(modeloPron, h = periodos, level = nivel)
  return(list(modelo = modeloPron,
              pronos = resultPron))
}

######################################################################

# FUNCIONES PRINCIPALES PARA CALCULAR LOS MODELO
# DE AUTO ARIMA Y RETORNAR LOS GRÁFICOS DEL
# MODELADO, LA INFORMACIÓN DE DIAGNOSTICO Y EL RESUMEN
# CON LOS INDICADORES MÁS REPRESENTATIVOS DEL MODELADO

arima.opcion <- function(serie, datos, resto, modelo, pronostico, opcion, periodos = 20, nivel = 95) {
  switch(opcion,
         pronostico={
           graficar.pronostico.arima(serie, datos, modelo, pronostico, periodos, nivel)
         },
         resultado={
           resumir.resultado.arima(resto, pronostico)
         },
         accuracy={
           accuracy.resultado.arima(resto, pronostico)
         },
         diagnostico={
           graficar.diagnostico.arima(datos, modelo)
         },
         stats={
           stats.diagnostico.arima(modelo)
         },
         {
           return()
         }
  )
}

graficar.pronostico.arima <- function(serie, datos, modelo, pronostico, periodos, nivel) {
  # list(real = plot(modelo$x, type = "o", col = "black", lwd = 2, pch = 20),
  #      pron = lines(modelo$fitted, col = "red", lwd = 2),
  #      leyenda = legend("topleft",
  #                       c("Real","Pronostico"),
  #                       lwd = c(2, 2),
  #                       col = c('black','red'),
  #                       bty = "n"),
  #      grid())

  na.n <- rep(NA, periodos)
  na.a <- rep(NA, length(pronostico$x) - 1)
  ult.a <- pronostico$fitted[(length(pronostico$fitted))]

  pronos.df <- data.frame(Index = time(serie),
                          # Data = c(pronostico$x, na.n),
                          Data = serie,
                          Fitted = c(pronostico$fitted, na.n),
                          Forecast = c(na.a, ult.a, pronostico$mean),
                          Lower = c(na.a, ult.a, pronostico$lower),
                          Upper = c(na.a, ult.a, pronostico$upper))

  ggplot(data = pronos.df) + xlab("Tiempo") + ylab("Valores") +
    geom_line(mapping = aes_string(x = 'Index', y = 'Data')) +
    geom_line(mapping = aes_string(x = 'Index', y = 'Fitted'), colour='red') +
    geom_line(mapping = aes_string(x = 'Index', y = 'Forecast'), colour='blue') +
    geom_ribbon(mapping = aes_string(x = 'Index', ymin = 'Lower', ymax = 'Upper'), alpha = 0.5)+
    ggtitle(tipo_modelo[[1]])+ #ggtitle("Modelo")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

}

graficar.diagnostico.arima <- function(datos, modelo) {
  tiempo <- seq(1:length(datos))
  residuo = modelo$residuals
  datosV <- fortify(datos)$Data

  # if(length(tiempo) != length(residual)) {
  #   #### TODO: Optimizar -> A veces no ajusta bien la longitud de tiempo y residual
  #   tiempo <- seq(1:length(residual))
  # }
  # list(tablero = par(mfrow=c(2,2)),
  #      residual = plot(tiempo, residual, type='b', ylab='', main="Residuales", col="red"),
  #      rlinea = abline(h=0, lty=2),
  #      densidad = plot(density(residual), xlab='x', main= 'Densidad residuales', col="red"),
  #      qpuntos = qqnorm(residual),
  #      qlinea = qqline(residual, col=2),
  #      correl = acf(residual, ci.type="ma", 60),
  #      grid())

  # d <- data.frame(tiempo=seq(1:length(datos)), residuo = residuals(modelo))
  d <- data.frame(tiempo = tiempo, datos = datosV, residuo = residuo)
  size_fonts.x <- 18
  size_fonts.y <- 12

  ####Residuales##############
  gresidual <- ggplot(d$residuo, aes(x=tiempo, y=residuo), type = "dashed")
  gresidual <- gresidual + geom_smooth(method = "lm", se = FALSE, color = "lightgrey")
  # gresidual <- gresidual + geom_segment(aes(xend = tiempo, yend = residuo), alpha = .2)
  gresidual <- gresidual + geom_point()
  # gresidual <- gresidual + scale_color_continuous(low = "blue", high = "red")
  # gresidual <- gresidual + guides(color = FALSE)
  # gresidual <- gresidual + geom_point(aes(y = modelo$model$datos), shape = 1)
  gresidual <- gresidual +labs( x="Tiempo", y="")+ggtitle("Residuales")
  gresidual <- gresidual + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gresidual <- gresidual + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Normal Q-Q plot
  gqqplot<- ggqqplot(d$residuo, xlab = "Cuantiles Teóricos", ylab = "Muestra", color = "blue", ggtheme=theme_gray())+ggtitle("Normal Q-Q Plot")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Densidad Residual
  gdresidual <- ggplot(d, aes(x=residuo)) + geom_density(fill="blue", colour=NA, alpha=.2) +   geom_line(stat = "density")
  gdresidual <-  gdresidual +  expand_limits(y = 0) + ggtitle("Densidad de residuales") + theme(plot.title = element_text(hjust = 0.5))
  gdresidual <-  gdresidual +  xlab("Residual") + ylab("Densidad")
  gdresidual <-  gdresidual + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gdresidual <-  gdresidual + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  ######Correlacion Residual
  bacf <- acf(d$residuo, ci.type="ma", 60)
  str(bacf$type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  ci2 = qnorm((1 + .95)/2)/sqrt(length(rnorm(bacf$acf)))

  gcorrel <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0))
  gcorrel <- gcorrel +  geom_segment(mapping = aes(xend = lag, yend = 0))
  gcorrel <- gcorrel + geom_hline(yintercept = c(ci2, -ci2), color = "purple", linetype = "dashed")+ggtitle("Series Residual")
  gcorrel <- gcorrel +  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.x, hjust=0.5))
  gcorrel <- gcorrel +  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=size_fonts.y))

  multiplot(gresidual , gqqplot, gdresidual, gcorrel, cols = 2)

}

resumir.resultado.arima <- function(resto, pronostico) {
  generar.resultado.pronostico(resto,pronostico,"AR")
}

accuracy.resultado.arima <- function(resto, pronostico) {
  generar.resultado.accuracy(resto, pronostico)
}

stats.diagnostico.arima <- function(modelo) {
  summary(modelo)
}

calcular.arima <- function(datos, periodos = 20, nivel = 95) {
  modeloPron <- auto.arima(datos)
  resultPron <- forecast(modeloPron, h = periodos, level = nivel)
  return(list(modelo = modeloPron,
              pronos = resultPron))
}
