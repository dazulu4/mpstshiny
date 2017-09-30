######################################################################
### FUNCIONES PARA ANALISIS DE DATOS
######################################################################

generar.normales <- function(datosO, datos, bloques = "Sturges", media, desv, muestra = 1000) {
  xt <- seq(from = min(datosO), to = max(datos), length.out = muestra)
  yt <- dnorm(xt, mean = media, sd = desv)
  densidad <- density(datos)

  plot <- ggplot(datosO, aes(x = datos))
  plot <- plot + geom_histogram(aes(y = ..density..), col = "blue", fill = "blue", bins = bloques, alpha = 0.2)
  plot <- plot + geom_density(aes(color = "Empírica"))
  plot <- plot + stat_function(aes(colour = "Teórica"), fun = dnorm, args = list(mean = media, sd = desv)) +
    scale_colour_manual("",values = c("brown", "black"))+labs( x="Cuantiles", y="Densidad")+ggtitle("Histograma")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

  # grafico <- list(plot)
  return(plot)
}

densidad.acumulada <- function(datosO, datos) {
  if(!is.null(datos) && length(datos) > 0) {

    d_acumulada <- fortify(ecdf(datos))
    d <- data.frame(x = d_acumulada$x, y= d_acumulada$y)
    ggplot(d, aes(x=d$x, y = d$y)) +
      stat_ecdf(aes(colour="Empírica")) + geom_point(aes(x= d$x, y = d$y), col = "blue") +
      scale_colour_manual("",values = c("blue"))+labs( x="x", y="Fn(x)")+ggtitle("Densidad Acumulada")+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

  } else {
    return(NULL)
  }
}

cuartil.cuartil <- function(datos) {
  if(!is.null(datos) && length(datos) > 0) {
    ggqqplot(datos, xlab = "Cuantiles Teóricos", ylab = "Valores", color = "blue", ggtheme=theme_gray())+ggtitle("Normal Q-Q Plot")+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))
  } else {
    return(NULL)
  }
}

serie.tiempo <- function(datos, es.decom = FALSE, decom = NULL) {
  if(es.decom) {
    print(decom$type)
    # dtype <- ifelse(decom$type == "additive", "Aditiva", "Multiplicativa")
    dtype <- ""
    titulo <- paste("Serie de Tiempo", dtype, sep = " ")
    serie <- autoplot(datos, ts.colour = "maroon", ts.linetype = 'solid')+labs( x="Tiempo", y="Valores")+ggtitle(titulo)+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=11))
    random <- autoplot(decom$random, ts.colour = "red", ts.linetype = 'solid')+labs( x="Tiempo", y="Valores")+ggtitle("Ruido")+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=11))
    trend <- autoplot(decom$trend, ts.colour = "green", ts.linetype = 'solid')+labs( x="Tiempo", y="Valores")+ggtitle("Tendencias")+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=11))
    seasonal <- autoplot(decom$seasonal, ts.colour = "blue", ts.linetype = 'solid')+labs( x="Tiempo", y="Valores")+ggtitle("Patrón Estacional")+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=11))
    retorno <- multiplot(serie, trend, random, seasonal, cols=2)
  } else {
    retorno <- autoplot(datos, ts.colour = "maroon", ts.linetype = 'solid')+labs( x="Tiempo", y="Valores")+ggtitle("Serie de Tiempo")+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15, hjust=0.5)) +
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=11))
  }
  return(retorno)
}

generar.serie <- function(datos, inicio = as.character(Sys.Date()), frecuencia = 1, periodos = 20) {
  datosSerie <- ts(datos,
                    start = decimal_date(ymd(inicio)),
                    frequency = frecuencia)

  datosProno <- NULL
  datosResto <- NULL
  if(length(datos) > (periodos + 20)) {
    datosProno <- ts(datos[1:(length(datos)-periodos)],
                     start = decimal_date(ymd(inicio)),
                     frequency = frecuencia)
    datosResto <- ts(datos[(length(datos)-periodos+1):length(datos)],
                     start = decimal_date(ymd(inicio)),
                     frequency = frecuencia)
  } else {
    datosProno <- datosSerie
    datosResto <- NULL
  }
  return(list(serie = datosSerie,
              prono = datosProno,
              resto = datosResto,
              decom = descomponer.serie(datosSerie)))
}

descomponer.serie <- function(datos) {
  datosDecom <- try(decompose(datos))
  if(inherits(datosDecom, "try-error")) {
    return(list(decom = NULL, es.decom = FALSE))
  }
  return(list(decom = datosDecom, es.decom = TRUE))
}



