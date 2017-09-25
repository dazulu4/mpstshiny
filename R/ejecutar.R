######################################################################
### CARGA DE LIBRERIAS
######################################################################
library(shiny)
library(shinythemes)
library(shinyjs)
library(boot)
library(moments)
library(nortest)
library(Cairo)
library(forecast)
require(graphics)

######################################################################
### DEFINICIÓN DE VARIABLES GLOBALES
######################################################################
datos <<- list()
datosVec <<- c()

datosSerie <<- list()
modeloPron <<- list()

totales <<- list()
estadisticos <<- list()

######################################################################
### DEFINICIÓN DE INTERFAZ GRÁFICA
######################################################################
ui <- fluidPage(theme = shinytheme("cerulean"),
                #shinythemes::themeSelector(),
                navbarPage("R-MPST",
                           tabPanel("CARGAR",
                                    titlePanel("Carga tus Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("file1", h4("Selecciona archivo de texto"),
                                                  buttonLabel = "Explorar...",
                                                  placeholder = "Sin archivo",
                                                  multiple = TRUE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv")),
                                        #tags$hr(),
                                        checkboxInput("header", "Encabezado", FALSE),
                                        radioButtons("sep", h4("Separador"),
                                                     choices = c(Coma = ",",
                                                                 "Punto y Coma" = ";",
                                                                 Tabulador = "\t"),
                                                     selected = ","),
                                        #radioButtons("quote", "Comillas",
                                        #             choices = c(Ninguna = "",
                                        #                         "Comillas Dobles" = '"',
                                        #                         "Comillas Simples" = "'"),
                                        #             selected = '"'),
                                        #tags$hr(),
                                        radioButtons("disp", h4("Muestra de Datos"),
                                                     choices = c(Encabezado = "head",
                                                                 "Completo" = "all"),
                                                     selected = "head"),
                                        tags$hr(),
                                        sliderInput("tolerance",
                                                    h4("Cantidad mínima de registros"),
                                                    value = 30,
                                                    min = 20,
                                                    max = 200),
                                        sliderInput("bootstrap",
                                                    h4("Cantidad replicas bootstrap"),
                                                    value = 1000,
                                                    min = 500,
                                                    max = 5000),
                                        numericInput("column",
                                                     label = h4("Columna del archivo"),
                                                     value = 1)
                                      ),
                                      mainPanel(
                                        # tabsetPanel(type = "tabs",
                                        #             tabPanel("Datos", tableOutput("contents")),
                                        #             tabPanel("Resumen", verbatimTextOutput('summary')))
                                        column(4, wellPanel(h4("Contenido"), tableOutput("contents"))),
                                        column(8, wellPanel(h4("Resumen"), verbatimTextOutput('summary')))
                                      )

                                    )
                           ),
                           tabPanel("ANALIZAR",
                                    titlePanel("Analiza tus Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("bins",
                                                    h4("Número de bloques"),
                                                    value = 15,
                                                    min = 1,
                                                    max = 100),
                                        tags$hr(),
                                        textInput("start",
                                                  label = h4("Inicio de la serie"),
                                                  value = "2000,1"),
                                        numericInput("frequency",
                                                     label = h4("Frecuencia de la serie"),
                                                     value = 4,
                                                     min = 1,
                                                     max = 365.25)
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Densidad Normal",
                                                             plotOutput("normal")),
                                                    tabPanel("Densidad Acumulada", plotOutput("ecdf")),
                                                    tabPanel("Pruebas Normalidad",
                                                             plotOutput("qqplot"),
                                                             verbatimTextOutput('test')),
                                                    tabPanel("Serie Tiempo", plotOutput("serie"))
                                        )
                                      )
                                    )
                           ),
                           tabPanel("MODELAR",
                                    titlePanel("Modela tus Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("model", h4("Selecciona el Modelo"),
                                                    choices = c("Tendencia Simple" = "trend",
                                                                "Tendencia con Estacionalidad" = "season",
                                                                "Suavizado Holt Winters" = "smoothhw")
                                        ),
                                        tags$br(),
                                        useShinyjs(),
                                        radioButtons("func", h4("Selecciona la Función"),
                                                     choices = c("Regresión Lineal" = "lineal",
                                                                 "Regresión Cuadrática" = "quadratic",
                                                                 "Regresión Cúbica" = "cubic",
                                                                 "Regresión Grado 4" = "gfour",
                                                                 "Regresión Grado 5" = "gfive"),
                                                     selected = "lineal")
                                        # ,tags$hr(),
                                        # radioButtons("interven", "Intervenciones",
                                        #              choices = c("Ninguno" = "inone",
                                        #                          "Cambio de nivel" = "level",
                                        #                          "Cambio de pendiente" = "slope",
                                        #                          "Outliers" = "outliers"),
                                        #              selected = "inone")
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Pronóstico",
                                                   plotOutput("forecast")),
                                          tabPanel("Diagnóstico",
                                                   plotOutput("diagnosis")),
                                          tabPanel("Resumen",
                                                   verbatimTextOutput('summary1'))
                                        )
                                      )
                                    )
                           )
                )
)

######################################################################
### DEFINICIÓN DE FUNCIONES LADO DEL SERVIDOR
######################################################################
server <- function(input, output) {

  # CARGA DE ARCHIVO

  output$contents <- renderTable({
    req(input$file1)
    cargar.archivo(archivo = input$file1$datapath,
                   encabezado = input$header,
                   separador = input$sep) #comillas = input$quote)
    if(input$disp == "head") {
      return(head(datos))
    }
    else {
      return(datos)
    }
  })
  output$summary <- renderPrint({
    req(input$file1)
    calcular.totales(columna = input$column)
    calcular.estadisticos(columna = input$column,
                          tolerancia = input$tolerance,
                          replicas = input$bootstrap)
    resultado <- list("Totales" = totales,
                      "Estadísticos" = estadisticos)
    str(sapply(sprintf('%s', names(resultado)), function(key) {
      resultado[[key]]
    }, simplify = FALSE))
  })


  # ANALISIS DE DATOS

  output$normal <- renderPlot({
    req(input$file1)
    #Gráfico Histograma y Densidades
    histograma(datosVec, input$bins)
    densidad.teorica(datosVec,
                     media = estadisticos[["Media"]],
                     desv = estadisticos[["Desv. estándar"]])
    densidad.empirica(datosVec)
    densidad.leyenda()
  })
  output$ecdf <- renderPlot({
    req(input$file1)
    #Gráfico Densidad acumulada
    densidad.acumulada(datosVec)
  })
  output$qqplot <- renderPlot({
    req(input$file1)
    #Gráfico 1.2: QQPlot
    qqplot <- cuartil.cuartil(datosVec)
    qqplot$puntos
    qqplot$linea
  })
  output$test <- renderPrint({
    req(input$file1)
    #Pruebas de normalidad
    jb <- jarque.test(datosVec)
    sw <- shapiro.test(datosVec)
    ad <- ad.test(datosVec)
    data.frame("Estadístico" = c(jb$statistic,
                                 sw$statistic,
                                 ad$statistic),
               "Valor-P" = c(jb$p.value,
                             sw$p.value,
                             ad$p.value),
               row.names = c("Jarque-Bera",
                             "Shapiro-Wilk",
                             "Anderson-Darling"))
  })
  output$serie <- renderPlot({
    req(input$file1)
    #Gráfico 1.3: Serie de tiempo
    generar.serie(datosVec,
                  inicio = input$start,
                  frecuencia = input$frequency)
    serie.tiempo(datosSerie)
  })


  # MODELADO DE SERIES DE TIEMPO

  observe({
    if (input$model == "smoothhw") {
      disable(selector = "[type=radio][name=func]")
      runjs("$('[type=radio][name=func]').parent().parent().addClass('disabled').css('opacity', 0.5)")
    } else {
      enable(selector = "[type=radio][name=func]")
      runjs("$('[type=radio][name=func]').parent().parent().addClass('enabled').css('opacity', 1)")
    }
  })

  calcular.modelo <- reactive({
    generar.serie(datosVec, input$start, input$frequency);
    switch(input$model,
           "trend" = tendencia.funcion(datosSerie, input$func),
           "season" = tendencia.funcion(datosSerie, input$func, estacion = TRUE),
           "smoothhw" = calcular.holtwinters(datosSerie))
  })

  calcular.modelo.func <- reactive({
    generar.serie(datosVec, input$start, input$frequency);
    switch(input$func,
           "lineal" = calcular.lineal(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE)),
           "quadratic" = calcular.cuadratica(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE)),
           "cubic" = calcular.cubica(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE)),
           "gfour" = calcular.grado4(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE)),
           "gfive" = calcular.grado5(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE)))
  })

  output$forecast <- renderPlot({
    req(input$file1)
    calcular.modelo()
    if((input$model == "trend") || (input$model == "season")) {
      calcular.modelo.func()
    }
    modelar.opcion(datosSerie, modeloPron, "pronostico", input$model)
  })
  output$diagnosis <- renderPlot({
    req(input$file1)
    calcular.modelo()
    if((input$model == "trend") || (input$model == "season")) {
      calcular.modelo.func()
    }
    modelar.opcion(datosSerie, modeloPron, "diagnostico", input$model)
  })
  output$summary1 <- renderPrint({
    req(input$file1)
    calcular.modelo()
    if((input$model == "trend") || (input$model == "season")) {
      calcular.modelo.func()
    }
    modelar.opcion(datosSerie, modeloPron, "resumen", input$model)
  })

  # Presenta mensajes al usuario
  mostrar.mensaje <- function(titulo = "Importante!", mensaje = "No implementado") {
    showModal(modalDialog(title = titulo, mensaje, easyClose = TRUE))
  }
}


######################################################################
### FUNCIONES PARA CARGA DE ARCHIVOS
######################################################################
cargar.archivo <- function(archivo,
                           encabezado = FALSE,
                           separador = ',',
                           comillas = '"') {
  datos <<- read.csv(file = archivo,
                     header = encabezado,
                     sep = separador,
                     quote = comillas,
                     fill = TRUE)
}
calcular.totales <- function(columna = 1) {
  total <- length(datos[,columna])
  totalNA <- sum(is.na(datos[,columna]))
  datos <<- na.omit(datos[,columna])
  datosNN <- suppressWarnings(as.numeric(as.character(datos)))
  totalNN <- sum(is.na(datosNN))
  datos <<- na.omit(data.frame(datosNN))
  totalNM <- length(datos[,columna])
  totales <<- list("Cargados" = total,
                   "Numéricos" = totalNM,
                   "No numéricos" = totalNN,
                   "Valores nulos" = totalNA)

  datosVec <<- datos[,columna]
  datos <<- data.frame(datosVec)
}
calcular.estadisticos <- function(columna = 1, tolerancia = 30, replicas = 1000) {
  media <- round(mean(datos[,columna]), 2)
  desv <- round(sd(datos[,columna]), 4)
  minimo <- round(min(datos[,columna]), 2)
  maximo <- round(max(datos[,columna]), 2)
  bootstrap <- FALSE
  if(length(datos[,columna]) < tolerancia) {
    resultado <- calcular.boot(datos[,columna], replicas)
    media <- round(resultado$media, 2)
    desv <- round(resultado$desv, 2)
    bootstrap <- TRUE
  }
  estadisticos <<- list(Media = media,
                        "Desv. estándar" = desv,
                        "Mínimo" = minimo,
                        "Máximo" = maximo,
                        Bootstrap = bootstrap)
}
calcular.media <- function(x, d) {
  return(mean(x[d]))
}
calcular.boot <- function(x, r = 1000) {
  resultado <- boot(x, statistic = calcular.media, R = r)
  list(media = resultado$t0,
       desv = sd(resultado$t[,1]))
}


######################################################################
### FUNCIONES PARA ANALISIS DE DATOS
######################################################################
histograma <- function(datos, bloques = "Sturges") {
  grafico <- list(hist(datos,
                      probability = TRUE,
                      breaks = bloques,
                      col = "cornsilk2", #col = "#75AADB",
                      border = "cornsilk4",
                      main = "Histograma",
                      xlab = "Cuantiles",
                      ylab = "Densidad"),
                  grid())
  return(grafico)
}
densidad.teorica <- function(datos, media, desv, muestra = 1000) {
  xt <- seq(from = min(datos), to = max(datos), length.out = muestra)
  yt <- dnorm(xt, mean = media, sd = desv)
  grafico <- lines(xt, yt, col = "cornflowerblue", lwd = 2)
  return(grafico)
}
densidad.empirica <- function(datos) {
  densidad <- density(datos)
  grafico <- lines(densidad, col = "brown4", lwd = 2)
  return(grafico)
}
densidad.leyenda <- function() {
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
         merge = FALSE)
}
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


######################################################################
### FUNCIONES PARA MODELADO DE DATOS
######################################################################
tendencia.funcion <- function(datos, funcion, estacion = FALSE) {
  if(funcion == "lineal") {
    calcular.lineal(datos, estacion)
  }
  else if(funcion == "quadratic") {
    calcular.cuadratica(datos, estacion)
  }
  else if(funcion == "cubic") {
    calcular.cubica(datos, estacion)
  }
  else if(funcion == "gfour") {
    calcular.grado4(datos, estacion)
  }
  else if(funcion == "gfive") {
    calcular.grado5(datos, estacion)
  }
  else {
    return()
  }
}
calcular.estacion <- function(datos) {
  seasonaldummy(datos)
}
calcular.lineal <- function(datos, estacion = FALSE) {
  tiempo <- seq(1:length(datos))
  if(estacion) {
    It <- calcular.estacion(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo)
  }
}
calcular.cuadratica <- function(datos, estacion = FALSE) {
  tiempo <- seq(1:length(datos))
  tiempo2 <- tiempo^2
  if(estacion) {
    It <- calcular.estacion(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2)
  }
}
calcular.cubica <- function(datos, estacion = FALSE) {
  tiempo <- seq(1:length(datos))
  tiempo2 <- tiempo^2
  tiempo3 <- tiempo^3
  if(estacion) {
    It <- calcular.estacion(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + tiempo3 + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + tiempo3)
  }
}
calcular.grado4 <- function(datos, estacion = FALSE) {
  tiempo <- seq(1:length(datos))
  tiempo2 <- tiempo^2
  tiempo3 <- tiempo^3
  tiempo4 <- tiempo^4
  if(estacion) {
    It <- calcular.estacion(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + tiempo3 + tiempo4 + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + tiempo3 + tiempo4)
  }
}
calcular.grado5 <- function(datos, estacion = FALSE) {
  tiempo <- seq(1:length(datos))
  tiempo2 <- tiempo^2
  tiempo3 <- tiempo^3
  tiempo4 <- tiempo^4
  tiempo5 <- tiempo^5
  if(estacion) {
    It <- calcular.estacion(datos)
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + tiempo3 + tiempo4 + tiempo5 + It)
  }
  else {
    modeloPron <<- lm(formula = datos ~ tiempo + tiempo2 + tiempo3 + tiempo4 + tiempo5)
  }
}
calcular.holtwinters <- function(datos) {
  modeloPron <<- HoltWinters(datos)
}

# FUNCIONES DE MODELADO DE BAJO NIVEL - PROCESAMIENTO GENERAL
modelar.opcion <- function(datos, modelo, opcion, tipo) {
  if(opcion == "pronostico") {
    if(tipo == "smoothhw") {
      graficar.hotlwinters(datos,
                           modelo)
    } else {
      graficar.tendencia(datos,
                         modelo)
    }
  }
  else if(opcion == "diagnostico") {
    if(tipo == "smoothhw") {

    } else {
      graficar.diagnostico(datos,
                           modelo)
    }
  }
  else if(opcion == "resumen") {
    resumir.diagnostico(modelo)
  }
  else {
    return()
  }
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
graficar.hotlwinters <- function(datos, modelo) {
  # tiempo <- seq(1:length(datos))
  list(real = plot(modelo$x, type = "o", col = "black", lwd = 2, pch = 20),
       pron = lines(modelo$fitted[,1], col = "red", lwd = 2),
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


######################################################################
### DEFINICIÓN FUNCIÓN PARA EJECUTAR PROGRAMA
######################################################################
ejecutar <- function() {
  runApp(shinyApp(ui = ui, server = server))
}

shinyApp(ui = ui, server = server)


