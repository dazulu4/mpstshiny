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
### DEFINICIÓN DE INTERFAZ GRÁFICA SHINY
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
### DEFINICIÓN DE FUNCIONES SERVIDOR SHINY
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
    generar.normales(datosVec,
                     bloques = input$bins,
                     media = estadisticos[["Media"]],
                     desv = estadisticos[["Desv. estándar"]])
    # densidad.teorica(datosVec,
    #                  media = estadisticos[["Media"]],
    #                  desv = estadisticos[["Desv. estándar"]])
    # densidad.empirica(datosVec)
    # densidad.leyenda()
  })

  output$ecdf <- renderPlot({
    req(input$file1)
    #Gráfico Densidad acumulada
    densidad.acumulada(datosVec)
  })

  output$qqplot <- renderPlot({
    req(input$file1)
    #Gráfico QQPlot
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
    #Gráfico Serie de tiempo
    generar.serie(datosVec,
                  inicio = input$start,
                  frecuencia = input$frequency)
    serie.tiempo(datosSerie)
  })


  # MODELADO DE SERIES DE TIEMPO

  # observe({
  #   if (input$model == "smoothhw") {
  #     disable(selector = "[type=radio][name=func]")
  #     runjs("$('[type=radio][name=func]').parent().parent().addClass('disabled').css('opacity', 0.5)")
  #   } else {
  #     enable(selector = "[type=radio][name=func]")
  #     runjs("$('[type=radio][name=func]').parent().parent().addClass('enabled').css('opacity', 1)")
  #   }
  # })

  observeEvent(input$model, {
    toggleState(id = "func", condition = (input$model != "smoothhw"))
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
      tendencia.opcion(datosSerie, modeloPron, "pronostico")
    } else {
      holtwinters.opcion(datosSerie, modeloPron, "pronostico")
    }
  })

  output$diagnosis <- renderPlot({
    req(input$file1)
    calcular.modelo()
    if((input$model == "trend") || (input$model == "season")) {
      calcular.modelo.func()
      tendencia.opcion(datosSerie, modeloPron, "diagnostico")
    } else {
      holtwinters.opcion(datosSerie, modeloPron, "diagnostico")
    }
  })

  output$summary1 <- renderPrint({
    req(input$file1)
    calcular.modelo()
    if((input$model == "trend") || (input$model == "season")) {
      calcular.modelo.func()
      tendencia.opcion(datosSerie, modeloPron, "resumen")
    } else {
      holtwinters.opcion(datosSerie, modeloPron, "resumen")
    }
  })

  # Presenta mensajes al usuario
  mostrar.mensaje <- function(titulo = "Importante!", mensaje = "No implementado") {
    showModal(modalDialog(title = titulo, mensaje, easyClose = TRUE))
  }
}


######################################################################
### DEFINICIÓN FUNCIÓN PARA EJECUTAR PROGRAMA
######################################################################
ejecutar <- function() {
  useShinyjs()
  runApp(shinyApp(ui = ui, server = server))
}

# ejecutar()

shinyApp(ui = ui, server = server)


