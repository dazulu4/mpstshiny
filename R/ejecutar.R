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
library(DT)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(lubridate)
require(graphics)


######################################################################
### DEFINICIÓN DE INTERFAZ GRÁFICA SHINY
######################################################################
ui <- fluidPage(theme = shinytheme("cerulean"),
                themeSelector(),
                navbarPage("R-MPST",
                           tabPanel("CARGAR DATOS",
                                    # titlePanel("Carga tus Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("file1", h4("Selecciona archivo de texto"),
                                                  buttonLabel = "Explorar...",
                                                  placeholder = "Sin archivo",
                                                  multiple = TRUE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv")),
                                        # tags$hr(),
                                        # checkboxInput("header", "¿Tiene encabezado?", FALSE),
                                        radioButtons("sep", h4("Separador de columnas"),
                                                     choices = c(Coma = ",",
                                                                 "Punto y Coma" = ";",
                                                                 Tabulador = "\t"),
                                                     selected = ","),
                                        #radioButtons("quote", "Comillas",
                                        #             choices = c(Ninguna = "",
                                        #                         "Comillas Dobles" = '"',
                                        #                         "Comillas Simples" = "'"),
                                        #             selected = '"'),
                                        # tags$hr(),
                                        radioButtons("disp", h4("Muestra de los datos"),
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
                                                    max = 5000)
                                        #, numericInput("column",
                                        #              label = h4("Columna del archivo"),
                                        #              value = 1),
                                      ),
                                      mainPanel(
                                        # tabsetPanel(type = "tabs",
                                        #             tabPanel("Datos", tableOutput("contents")),
                                        #             tabPanel("Resumen", verbatimTextOutput('summary')))
                                        # column(4, wellPanel(h4("Contenido"), dataTableOutput("contents"))),
                                        # column(8, wellPanel(h4("Resumen"), verbatimTextOutput('summary')))
                                        wellPanel(h4("Totales del archivo"), tableOutput('summaryTotals')),
                                        wellPanel(h4("Estadísticos de los datos"), tableOutput('summaryStats')),
                                        wellPanel(h4("Contenido del archivo"), dataTableOutput("contents"))
                                      )

                                    )
                           ),
                           tabPanel("ANÁLIZAR",
                                    # titlePanel("Analiza tus Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("bins",
                                                    h4("Número de bloques"),
                                                    value = 15,
                                                    min = 1,
                                                    max = 100),
                                        tags$hr(),
                                        # textInput("start",
                                        #           label = h4("Inicio de la serie"),
                                        #           value = "2000,1"),
                                        dateInput("start", h4("Inicio de la serie"),
                                                  value = as.character(Sys.Date()),
                                                  format = "yyyy-mm-dd",
                                                  startview = "year",
                                                  weekstart = 1),
                                        span(textOutput("nodate"), style="color:red"),
                                        # numericInput("frequency",
                                        #              label = h4("Frecuencia de la serie"),
                                        #              value = 4,
                                        #              min = 1,
                                        #              max = 365.25)
                                        radioButtons("frequency", h4("Frecuencia de la serie"),
                                                     choices = c("Anual" = as.character(anual),
                                                                 "Trimestral" = as.character(trimestral),
                                                                 "Mensual" = as.character(mensual),
                                                                 "Semanal" = as.character(semanal)),
                                                     # "Diario" = as.character(diaria)),
                                                     selected = as.character(mensual))
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel(h5("Densidad Normal"), plotOutput("normal")),
                                                    tabPanel(h5("Densidad Acumulada"), plotOutput("ecdf")),
                                                    tabPanel(h5("Pruebas Normalidad"),
                                                             fluidRow(column(7, plotOutput("qqplot")),
                                                                      column(5, tableOutput("test"))
                                                             )
                                                    ),
                                                    tabPanel(h5("Serie Tiempo"), plotOutput("serie"))
                                        )
                                      )
                                    )
                           ),
                           tabPanel("AJUSTAR MODELOS",
                                    # titlePanel("Modela tus Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("model", h4("Selecciona el modelo"),
                                                    choices = c("Tendencias Simple" = "trend",
                                                                "Tendencias Estacionales" = "season",
                                                                "Suavizado HoltWinters" = "holtwinters",
                                                                "Modelado ARIMA" = "arima")),
                                        # tags$br(),
                                        useShinyjs(),
                                        selectInput("func", h4("Selecciona la función"),
                                                    choices = c("Regresión Lineal" = "lineal",
                                                                "Regresión Cuadrática" = "quadratic",
                                                                "Regresión Cúbica" = "cubic"),
                                                    # "Regresión Grado 4" = "gfour",
                                                    # "Regresión Grado 5" = "gfive"),
                                                    selected = "lineal"),
                                        # ,tags$hr(),
                                        # radioButtons("interven", "Intervenciones",
                                        #              choices = c("Ninguno" = "inone",
                                        #                          "Cambio de nivel" = "level",
                                        #                          "Cambio de pendiente" = "slope",
                                        #                          "Outliers" = "outliers"),
                                        #              selected = "inone")
                                        tags$hr(),
                                        sliderInput("h",
                                                    h4("Periodos para pronóstico"),
                                                    value = 20,
                                                    min = 10,
                                                    max = 50,
                                                    step = 10),
                                        sliderInput("level",
                                                    h4("Nivel de confianza"),
                                                    value = 95,
                                                    min = 80,
                                                    max = 99,
                                                    step = 1)
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel(h5("Pronóstico"), plotOutput("forecast")),
                                          tabPanel(h5("Diagnóstico"), plotOutput("diagnosis")),
                                          tabPanel(h5("Resumen"),
                                                   wellPanel(h4("Medidas de bondad de ajuste"),
                                                             verbatimTextOutput('summary1')),
                                                   wellPanel(h4("Estadísticos del modelo"),
                                                             verbatimTextOutput('summary2')))
                                        )
                                      )
                                    )
                           )
                )

)

######################################################################
### DEFINICIÓN DE FUNCIONES SERVIDOR SHINY
######################################################################
server <- function(input, output, session) {

  # CARGA DE ARCHIVO

  output$contents <- renderDataTable({
    req(input$file1)

    datos <<- try(cargar.archivo(archivo = input$file1$datapath,
                                 separador = input$sep))
    if(inherits(datos, "try-error")) {
      mostrar.mensaje(titulo = "Error!",
                      mensaje = paste("Ocurrio un error cargando el archivo.",
                                      "Por favor intentelo nuevamente.",
                                      sep = " "))
    }

    # Solo se hace para garantizar la carga
    # t <- calc.totales()
    # e <- calc.estadisticos()

    if(input$disp == "head") {
      nuevos.datos <- head(datos)
    }
    else {
      nuevos.datos <- datos
    }

    datatable(nuevos.datos,
              options = list(pageLength = 5),
              class = 'cell-border stripe',
              rownames = FALSE,
              style = "bootstrap")
  })

  output$summaryTotals <- renderTable({
    req(input$file1)

    totales <<- try(calcular.totales(datos))
    if(inherits(totales, "try-error")) {
      return(data.frame(Mensaje = "No existe información de totales"))
    } else {
      datos <<- totales$datos
      datosVec <<- totales$vector
      return(totales$totales)
    }

  }, spacing = "m", striped = TRUE, digits = decimales)

  output$summaryStats <- renderTable({
    req(input$file1)

    estadisticos <<- try(calcular.estadisticos(datos,
                                               tolerancia = input$tolerance,
                                               replicas = input$bootstrap))
    if(inherits(estadisticos, "try-error")) {
      return(data.frame(Mensaje = "No existe información de estadísticos"))
    } else {
      return(estadisticos)
    }

  }, spacing = "m", striped = TRUE, digits = decimales)


  # ANALISIS DE DATOS

  output$normal <- renderPlot({
    req(input$file1)
    #Gráfico Histograma y Densidades
    grafico <- try(generar.normales(datos,
                                    datosVec,
                                    bloques = input$bins,
                                    media = estadisticos[["Media"]],
                                    desv = estadisticos[["Desv_Estandar"]]))
    if(inherits(grafico, "try-error")) {
      # return(plot(x = 0,
      #             y = 0,
      #             main = paste("Ocurrió un error al generar el gráfico.",
      #                          "Verifique los parámetros de entrada.",
      #                          sep = " ")))
    } else {
      return(grafico)
    }
  })

  output$ecdf <- renderPlot({
    req(input$file1)
    #Gráfico Densidad acumulada
    grafico <- try(densidad.acumulada(datos, datosVec))
    if(inherits(grafico, "try-error")) {
      # return(plot(x = 0,
      #             y = 0,
      #             main = paste("Ocurrió un error al generar el gráfico.",
      #                          "Verifique los parámetros de entrada.",
      #                          sep = " ")))
    } else {
      return(grafico)
    }
  })

  output$qqplot <- renderPlot({
    req(input$file1)
    #Gráfico QQPlot
    grafico <- try(cuartil.cuartil(datosVec))
    if(inherits(grafico, "try-error")) {
      # return(plot(x = 0,
      #             y = 0,
      #             main = paste("Ocurrió un error al generar el gráfico.",
      #                          "Verifique los parámetros de entrada.",
      #                          sep = " ")))
    } else {
      return(grafico)
    }
  })

  output$test <- renderTable({
    req(input$file1)
    #Pruebas de normalidad
    if(!is.null(datosVec) &&
       length(datosVec) > 0) {
      jb <- jarque.test(datosVec)
      sw <- shapiro.test(datosVec)
      ad <- ad.test(datosVec)
      data.frame(Estadistico = c(jb$statistic,
                                 sw$statistic,
                                 ad$statistic),
                 Valor_P = c(round(jb$p.value, decimales),
                             round(sw$p.value, decimales),
                             round(ad$p.value, decimales)),
                 row.names = c("Jarque-Bera",
                               "Shapiro-Wilk",
                               "Anderson-Darling"))
    } else {
      data.frame(Mensaje = "No existe información de pruebas normales.")
    }
  }, rownames = TRUE, spacing = "m", striped = TRUE, digits = decimales)

  output$serie <- renderPlot({
    req(input$file1)
    #Gráfico Serie de tiempo
    serie.result <- cargar.serie.tiempo()
    if(!is.null(datosDecom) &&
       length(datosDecom) > 0) {
      if(!datosDecom$es.decom) {
        mostrar.mensaje(titulo = "Advertencia!", mensaje = error.params.serie)
      }
      serie.tiempo(datosSerie, datosDecom$es.decom, datosDecom$decom)
    }
  })

  # Validación de parámetros de la serie de tiempo

  observeEvent(input$start, {
    ifelse(!validar.fecha(input$start), Sys.Date(), input$start)
  })

  output$nodate <- renderText({
    if(!validar.fecha(input$start)) { error.inicio.serie }
  })

  validar.fecha <- function(fecha = Sys.Date()) {
    isTruthy(input$start)
  }

  fecha.inicio <- function(fecha = Sys.Date()) {
    ifelse(!validar.fecha(fecha),
           as.character(Sys.Date()),
           as.character(fecha))
  }

  cargar.serie.tiempo <- function() {
    serie.result <- try(generar.serie(datosVec,
                                      inicio = fecha.inicio(input$start),
                                      frecuencia = as.double(input$frequency)))
    if(inherits(serie.result, "try-error")) {
      datosSerie <<- list()
      datosDecom <<- list()
      return(NULL)
    } else {
      datosSerie <<- serie.result$serie
      datosDecom <<- serie.result$decom
      return(serie.result)
    }
  }


  # MODELADO DE SERIES DE TIEMPO

  observeEvent(input$model, {
    toggleState(id = "func", condition = (input$model == "trend" || input$model == "season"))
  })

  calcular.modelo <- function() {
    retorno <- try(switch(input$model,
                          "trend" = tendencia.funcion(datos = datosSerie,
                                                      funcion = input$func,
                                                      estacion = FALSE,
                                                      periodos = input$h,
                                                      nivel = input$level),
                          "season" = tendencia.funcion(datos = datosSerie,
                                                       funcion = input$func,
                                                       estacion = TRUE,
                                                       periodos = input$h,
                                                       nivel = input$level),
                          "holtwinters" = calcular.holtwinters(datosSerie, input$h, input$level),
                          "arima" = calcular.arima(datosSerie, input$h, input$level)))
    if(inherits(retorno, "try-error")) {
      return(NULL)
    } else {
      modeloPron <<- retorno$modelo
      resultPron <<- retorno$pronos
    }
  }

  calcular.modelo.func <- function() {
    retorno <- try(switch(input$func,
                          "lineal" = calcular.regresion(datos = datosSerie,
                                                        estacion = ifelse(input$model == "season", TRUE, FALSE),
                                                        grado = "grado1",
                                                        periodos = input$h,
                                                        nivel = input$level),
                          "quadratic" = calcular.regresion(datos = datosSerie,
                                                           estacion = ifelse(input$model == "season", TRUE, FALSE),
                                                           grado = "grado2",
                                                           periodos = input$h,
                                                           nivel = input$level),
                          "cubic" = calcular.regresion(datos = datosSerie,
                                                       estacion = ifelse(input$model == "season", TRUE, FALSE),
                                                       grado = "grado3",
                                                       periodos = input$h,
                                                       nivel = input$level)))
    # "gfour" = calcular.regresion(datos = datosSerie,
    #                              estacion = ifelse(input$model == "season", TRUE, FALSE),
    #                              grado = "grado4",
    #                              periodos = input$h,
    #                              nivel = input$level),
    # "gfive" = calcular.regresion(datos = datosSerie,
    #                              estacion = ifelse(input$model == "season", TRUE, FALSE),
    #                              grado = "grado5",
    #                              periodos = input$h,
    #                              nivel = input$level)))
    if(inherits(retorno, "try-error")) {
      modeloPron <<- NULL
      resultPron <<- NULL
    } else {
      modeloPron <<- retorno$modelo
      resultPron <<- retorno$pronos
    }
  }

  output$forecast <- renderPlot({
    req(input$file1)
    retorno <- try(generar.pron(input$model,
                                "pronostico",
                                input$start,
                                input$frequency,
                                input$h,
                                input$level))
    if(inherits(retorno, "try-error")) {
      return(NULL)
    } else {
      return(retorno)
    }
  })

  output$diagnosis <- renderPlot({
    req(input$file1)
    retorno <- try(generar.pron(input$model,
                                "diagnostico",
                                input$start,
                                input$frequency,
                                input$h,
                                input$level))
    if(inherits(retorno, "try-error")) {
      return(NULL)
    } else {
      return(retorno)
    }
  })

  output$summary1 <- renderPrint({
    req(input$file1)
    retorno <- try(generar.pron(input$model,
                                "resumen1",
                                input$start,
                                input$frequency,
                                input$h,
                                input$level))
    if(inherits(retorno, "try-error")) {
      return(data.frame(Mensaje = "No existe información medidas de los pronósticos."))
    } else {
      return(retorno)
    }
  })

  output$summary2 <- renderPrint({
    req(input$file1)
    retorno <- try(generar.pron(input$model,
                                "resumen2",
                                input$start,
                                input$frequency,
                                input$h,
                                input$level))
    if(inherits(retorno, "try-error")) {
      return(data.frame(Mensaje = "No existe información estadísticos de los pronósticos."))
    } else {
      return(retorno)
    }
  })

  # Función general para pronóstico
  generar.pron <- function(tipo, opcion, inicio, frecuencia, periodos = 20, nivel = 95) {
    serie.result <- cargar.serie.tiempo()
    datosSerie <<- serie.result$serie
    datosDecom <<- serie.result$decom
    if(!is.null(datosSerie) && length(datosSerie) > 0 &&
       !is.null(datosDecom) && length(datosDecom) > 0) {
      if(tipo == "trend") {
        calcular.modelo()
        calcular.modelo.func()
        return(tendencia.opcion(datosSerie, modeloPron, resultPron, opcion, periodos))
      }
      else {
        if(datosDecom$es.decom) {
          if(tipo == "season") {
            calcular.modelo()
            calcular.modelo.func()
            return(tendencia.opcion(datosSerie, modeloPron, resultPron, opcion, periodos))
          }
          else if(tipo == "holtwinters") {
            calcular.modelo()
            return(holtwinters.opcion(datosSerie, modeloPron, resultPron, opcion, nivel))
          }
          else if(tipo == "arima") {
            calcular.modelo()
            return(arima.opcion(datosSerie, modeloPron, resultPron, opcion, nivel))
          }
        }
      }
    } else {
      return()
    }
  }

  # Presenta mensajes al usuario
  mostrar.mensaje <- function(titulo = "Importante!", mensaje = "No implementado") {
    showModal(modalDialog(title = titulo,
                          mensaje,
                          easyClose = TRUE,
                          size = "m",
                          footer = tagList(modalButton("Cerrar"))))
  }
}


######################################################################
### DEFINICIÓN FUNCIÓN PARA EJECUTAR PROGRAMA
######################################################################

ejecutar <- function() {
  ### DEFINICIÓN DE VARIABLES GLOBALES
  datos <<- list()
  datosVec <<- c()
  totales <<- list()
  estadisticos <<- list()
  datosSerie <<- list()
  modeloPron <<- list()
  resultPron <<- list()
  decimales <<- 6

  # Frecuencias series de tiempo
  diaria <<- 365.28
  semanal <<- round(diaria/7, 2)
  mensual <<- 12.0
  trimestral <<- 4.0
  anual <<- 1.0

  # Mensajes de usuario
  error.params.serie <<- paste("La serie de tiempo no debe tener menos de 2 periodos.",
                               "Por favor ajuste la frecuencia de la serie de tiempo, ya que",
                               "los métodos de pronóstico estacionales y HoltWinters no funcionaran",
                               sep = " ")
  error.inicio.serie <<- "¡Valor de inicio de la serie invalido!"

  useShinyjs()
  runApp(shinyApp(ui = ui, server = server))
}

