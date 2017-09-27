######################################################################
### FUNCIONES PARA ANALISIS DE DATOS
######################################################################
generar.normales <- function(datosO,datos, bloques = "Sturges", media, desv, muestra = 1000) {
  xt <- seq(from = min(datosO), to = max(datos), length.out = muestra)
  yt <- dnorm(xt, mean = media, sd = desv)
  densidad <- density(datos)

  # grafico <- list(hist(datos,
  #                      probability = TRUE,
  #                      breaks = bloques,
  #                      col = "cornsilk2", #col = "#75AADB",
  #                      border = "cornsilk4",
  #                      main = "Histograma",
  #                      xlab = "Cuantiles",
  #                      ylab = "Densidad"),
  #                 lines(xt, yt, col = "cornflowerblue", lwd = 2),
  #                 lines(densidad, col = "brown4", lwd = 2),
  #                 legend(x = 'topright',
  #                        legend = c('Teórica', 'Empírica'),
  #                        fill = NULL,
  #                        col = c('cornflowerblue', 'brown4'),
  #                        border = "black",
  #                        bg = 'white',
  #                        lty = 'solid',
  #                        lwd = c(2, 2),
  #                        bty = "n",
  #                        horiz = FALSE,
  #                        merge = FALSE),
  #                 alpha=(.9),
  #                 grid())

  plot <- ggplot(datosO, aes(x = datos))
  plot <- plot + geom_histogram(aes(y = ..density..), col = "blue", fill = "blue", bins = bloques, alpha = 0.2)
  plot <- plot + geom_density(aes(color = "Empírica"))
  plot <- plot + stat_function(aes(colour = "Teórica"), fun = dnorm, args = list(mean = media, sd = desv)) +
    scale_colour_manual("",values = c("brown", "black"))+labs( x="Cuantiles", y="Densidad")+ggtitle("Histograma")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

  grafico <- list(plot)
  return(grafico)
}

densidad.acumulada <- function(datosO,datos) {
  # grafico <- list(plot(ecdf(datos),
  #                      col = "black",
  #                      main = "Densidad Acumulada Empírica"),
  #                 grid())

  cdf <- ggplot(datosO, aes(x=datos, sample = length(datosO))) + stat_ecdf(aes(colour="Empírica"))+
    scale_colour_manual("",values = c("blue"))+labs( x="x", y="Fn(x)")+ggtitle("Densidad Acumulada")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

  grafico <- list(cdf)
  return(grafico)
}

cuartil.cuartil <- function(datos) {
  # retorno <- list(puntos = qqnorm(datos,
  #                                 xlab = "Cuantiles Teóricos",
  #                                 ylab = "Muestra"),
  #                 linea = qqline(datos),
  #                 grid())
  g<- ggqqplot(datos, xlab = "Cuantiles Teóricos", ylab = "Muestra", color = "blue")+ggtitle("Normal Q-Q Plot")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0.5)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))
  retorno<-list(g)

  return(retorno)
}

serie.tiempo <- function(datos, es.decom = FALSE, decom = NULL) {
  # retorno <- list(plot(datos,
  #                      col = "blue4",
  #                      #pch = 22,
  #                      type = "l",
  #                      main = "Serie de tiempo",
  #                      xlab = "Tiempo",
  #                      ylab = "Valores"),
  #                 grid())
  # str(datos)

  if(es.decom) {
    dtype <- ifelse(decom$type == "additive", "Aditiva", "Multiplicativa")
    serie <- autoplot(datos, ts.colour = "maroon", ts.linetype = 'solid')+labs( x="Tiempo", y="Valores")+ggtitle(paste("Serie de Tiempo", dtype, sep = " "))+
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

generar.serie <- function(datos, inicio = as.character(Sys.Date()), frecuencia = 1.0) {
  # inicio <- unlist(strsplit(inicio, '-'))
  # if(trimestral == frecuencia) {
  #   if(as.integer(inicio[2]) %in% 1:3) {
  #     inicioVec <- c(as.integer(inicio[1]), 1)
  #   } else if(as.integer(inicio[2]) %in% 4:6) {
  #     inicioVec <- c(as.integer(inicio[1]), 2)
  #   } else if(as.integer(inicio[2]) %in% 7:9) {
  #     inicioVec <- c(as.integer(inicio[1]), 3)
  #   } else {
  #     #meses entre 10:12
  #     inicioVec <- c(as.integer(inicio[1]), 4)
  #   }
  # } else {
  #   inicioVec <- c(as.integer(inicio[1]), as.integer(inicio[2]))
  # }
  # datosSerie <<- ts(datos,
  #                   start = inicioVec,
  #                   frequency = frecuencia)
  datosSerie <<- ts(datos,
                    start = decimal_date(ymd(inicio)),
                    frequency = frecuencia)
  return(descomponer.serie(datosSerie))
}

descomponer.serie <- function(datos) {
  datosDecom <- try(decompose(datos))
  if(inherits(datosDecom, "try-error")) {
    return(list(decom = NULL, es.decom = FALSE))
  }
  return(list(decom = datosDecom, es.decom = TRUE))
}



