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
### DEFINICIÓN DE VARIABLES CONSTANTES
######################################################################
decimales <<- 6

# Frecuencias series de tiempo
diaria <<- 365.28
semanal <<- round(diaria/7, 2)
mensual <<- 12.0
trimestral <<- 4.0
anual <<- 1.0

# Mensajes de usuario
error.params.serie <- paste("La serie de tiempo no debe tener menos de 2 periodos.",
                     "Por favor ajuste la frecuencia de la serie de tiempo, ya que",
                     "los métodos de pronóstico estacionales y HoltWinters no funcionaran",
                     sep = " ")
error.inicio.serie <- "¡Valor de inicio de la serie invalido!"

######################################################################
### DEFINICIÓN DE VARIABLES GLOBALES
######################################################################
datos <<- list()
datosVec <<- c()
estadisticos <<- list()
datosSerie <<- list()
modeloPron <<- list()

######################################################################
### DEFINICIÓN DE INTERFAZ GRÁFICA SHINY
######################################################################
ui <- fluidPage(theme = shinytheme("cerulean"),
                themeSelector(),
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
                                        tags$hr(),
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
                                        tags$hr(),
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
                                        wellPanel(h4("Contenido del Archivo"), dataTableOutput("contents")),
                                        wellPanel(h4("Totales del Archivo"), tableOutput('summaryTotals')),
                                        wellPanel(h4("Estadísticos de los Datos"), tableOutput('summaryStats'))
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
                                                                "Semanal" = as.character(semanal),
                                                                "Diario" = as.character(diaria)),
                                                    selected = as.character(mensual))
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel(h5("Densidad Normal"), plotOutput("normal")),
                                                    tabPanel(h5("Densidad Acumulada"), plotOutput("ecdf")),
                                                    tabPanel(h5("Pruebas Normalidad"),
                                                             plotOutput("qqplot"),
                                                             wellPanel(h4("Pruebas de normalidad"), tableOutput("test"))),
                                                    tabPanel(h5("Serie Tiempo"), plotOutput("serie"))
                                        )
                                      )
                                    )
                           ),
                           tabPanel("MODELAR",
                                    titlePanel("Modela tus Datos"),
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
                                                                 "Regresión Cúbica" = "cubic",
                                                                 "Regresión Grado 4" = "gfour",
                                                                 "Regresión Grado 5" = "gfive"),
                                                     selected = "lineal"),
                                        # ,tags$hr(),
                                        # radioButtons("interven", "Intervenciones",
                                        #              choices = c("Ninguno" = "inone",
                                        #                          "Cambio de nivel" = "level",
                                        #                          "Cambio de pendiente" = "slope",
                                        #                          "Outliers" = "outliers"),
                                        #              selected = "inone")
                                        tags$hr(),
                                        sliderInput("ahead",
                                                    h4("Periodos para pronóstico"),
                                                    value = 50,
                                                    min = 20,
                                                    max = 100),
                                        sliderInput("confidence",
                                                    h4("Nivel de confianza"),
                                                    value = 0.95,
                                                    min = 0.85,
                                                    max = 0.99,
                                                    step = 0.025)
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Pronóstico",
                                                   plotOutput("forecast")),
                                          tabPanel("Diagnóstico",
                                                   plotOutput("diagnosis")),
                                          tabPanel("Resumen",
                                                   verbatimTextOutput('summary'))
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

  output$contents <- DT::renderDataTable({
    req(input$file1)
    cargar.archivo(archivo = input$file1$datapath,
                   encabezado = input$header,
                   separador = input$sep) #comillas = input$quote)
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
    data.frame(calcular.totales())
  }, spacing = "m", striped = TRUE, digits = decimales)

  output$summaryStats <- renderTable({
    req(input$file1)
    data.frame(calcular.estadisticos(tolerancia = input$tolerance,
                                     replicas = input$bootstrap))
  }, spacing = "m", striped = TRUE, digits = decimales)


  # ANALISIS DE DATOS

  output$normal <- renderPlot({
    req(input$file1)
    #Gráfico Histograma y Densidades
    generar.normales(datos,
                     datosVec,
                     bloques = input$bins,
                     media = estadisticos[["Media"]],
                     desv = estadisticos[["Desv_Estandar"]])
  })

  output$ecdf <- renderPlot({
    req(input$file1)
    #Gráfico Densidad acumulada
    densidad.acumulada(datos,datosVec)
  })

  output$qqplot <- renderPlot({
    req(input$file1)
    #Gráfico QQPlot
    cuartil.cuartil(datosVec)
  })

  output$test <- renderTable({
    req(input$file1)
    #Pruebas de normalidad
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
  }, rownames = TRUE, spacing = "m", striped = TRUE, digits = decimales)

  output$serie <- renderPlot({
    req(input$file1)
    #Gráfico Serie de tiempo
    serie.result <- generar.serie(datosVec,
                                  inicio = fecha.inicio(input$start),
                                  frecuencia = as.double(input$frequency))
    if(!serie.result$es.decom) {
      mostrar.mensaje(titulo = "Advertencia!", mensaje = error.params.serie)
    }
    serie.tiempo(datosSerie, serie.result$es.decom, serie.result$decom)
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


  # MODELADO DE SERIES DE TIEMPO

  observeEvent(input$model, {
    toggleState(id = "func", condition = (input$model == "trend" || input$model == "season"))
  })

  calcular.modelo <- reactive({
    switch(input$model,
           "trend" = tendencia.funcion(datosSerie, input$func),
           "season" = tendencia.funcion(datosSerie, input$func, estacion = TRUE),
           "holtwinters" = calcular.holtwinters(datosSerie),
           "arima" = calcular.arima(datosSerie))
  })

  calcular.modelo.func <- reactive({
    switch(input$func,
           "lineal" = calcular.regresion(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE), 1),
           "quadratic" = calcular.regresion(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE), 2),
           "cubic" = calcular.regresion(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE), 3),
           "gfour" = calcular.regresion(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE), 4),
           "gfive" = calcular.regresion(datosSerie, estacion = ifelse(input$model == "season", TRUE, FALSE), 5))
  })

  output$forecast <- renderPlot({
    req(input$file1)
    # es.error.serie <- generar.serie(datosVec,
    #                                 fecha.inicio(input$start),
    #                                 as.double(input$frequency));
    # if(!es.error.serie) {
    #   calcular.modelo()
    #   if((input$model == "trend") || (input$model == "season")) {
    #     calcular.modelo.func()
    #     tendencia.opcion(datosSerie, modeloPron, "pronostico")
    #   } else {
    #     holtwinters.opcion(datosSerie, modeloPron, "pronostico")
    #   }
    # }

    generar.pron(input$model,
                 "pronostico",
                 input$start,
                 input$frequency,
                 input$ahead,
                 input$confidence)
  })

  output$diagnosis <- renderPlot({
    req(input$file1)
    # es.error.serie <- generar.serie(datosVec,
    #                                 fecha.inicio(input$start),
    #                                 as.double(input$frequency));
    # if(!es.error.serie) {
    #   calcular.modelo()
    #   if((input$model == "trend") || (input$model == "season")) {
    #     calcular.modelo.func()
    #     tendencia.opcion(datosSerie, modeloPron, "diagnostico")
    #   } else {
    #     holtwinters.opcion(datosSerie, modeloPron, "diagnostico")
    #   }
    # }

    generar.pron(input$model,
                 "diagnostico",
                 input$start,
                 input$frequency,
                 input$ahead,
                 input$confidence)
  })

  output$summary <- renderPrint({
    req(input$file1)
    # es.error.serie <- generar.serie(datosVec,
    #                                 fecha.inicio(input$start),
    #                                 as.double(input$frequency));
    # if(!es.error.serie) {
    #   calcular.modelo()
    #   if((input$model == "trend") || (input$model == "season")) {
    #     calcular.modelo.func()
    #     tendencia.opcion(datosSerie, modeloPron, "resumen")
    #   } else {
    #     holtwinters.opcion(datosSerie, modeloPron, "resumen")
    #   }
    # }

    generar.pron(input$model,
                 "resumen",
                 input$start,
                 input$frequency,
                 input$ahead,
                 input$confidence)
  })

  # Función general para pronóstico
  generar.pron <- function(tipo, opcion, inicio, frecuencia, periodos = 20, nivel = 0.95) {
    serie.result <- generar.serie(datosVec,
                                  fecha.inicio(inicio),
                                  as.double(frecuencia));
    if(tipo == "trend") {
      calcular.modelo()
      calcular.modelo.func()
      return(tendencia.opcion(datosSerie, modeloPron, opcion, periodos, nivel))
    }
    else {
      if(serie.result$es.decom) {
        if(tipo == "season") {
          calcular.modelo()
          calcular.modelo.func()
          return(tendencia.opcion(datosSerie, modeloPron, opcion, periodos, nivel))
        }
        else if(tipo == "holtwinters") {
          calcular.modelo()
          return(holtwinters.opcion(datosSerie, modeloPron, opcion, periodos, nivel))
        }
        else if(tipo == "arima") {
          calcular.modelo()
          return(arima.opcion(datosSerie, modeloPron, opcion, periodos, nivel))
        }
      }
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
  useShinyjs()
  runApp(shinyApp(ui = ui, server = server))
}

# shinyApp(ui = ui, server = server)

