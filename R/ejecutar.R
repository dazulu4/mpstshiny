######################################################################
### CARGA DE LIBRERIAS
######################################################################
library(shiny)
library(shinythemes)
library(boot)
library(moments)
library(nortest)
library(Cairo)
library(data.table)
require(graphics)

######################################################################
### DEFINICIÓN DE VARIABLES GLOBALES
######################################################################
datos <- list()
datosVec <- c()
datosSerie <- list()
totales <- list()
estadisticos <- list()

######################################################################
### DEFINICIÓN DE INTERFAZ GRÁFICA
######################################################################
ui <- fluidPage(theme = shinytheme("cerulean"),
                #shinythemes::themeSelector(),
                navbarPage("R-mpstshiny",
                           tabPanel("Cargar",
                                    titlePanel("Carga de Archivo"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("file1", "Selecciona archivo de texto",
                                                  buttonLabel = "Explorar...",
                                                  placeholder = "Sin archivo",
                                                  multiple = TRUE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv")),
                                        #tags$hr(),
                                        checkboxInput("header", "Encabezado", FALSE),
                                        radioButtons("sep", "Separador",
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
                                        radioButtons("disp", "Mostrar Datos",
                                                     choices = c(Encabezado = "head",
                                                                 "Compleo" = "all"),
                                                     selected = "head"),
                                        tags$hr(),
                                        sliderInput("tolerance",
                                                    "Tolerancia bootstrap",
                                                    value = 30,
                                                    min = 20,
                                                    max = 200),
                                        sliderInput("bootstrap",
                                                    "Replicas bootstrap",
                                                    value = 1000,
                                                    min = 500,
                                                    max = 5000),
                                        numericInput("column",
                                                     label = "Columna del archivo",
                                                     value = 1)
                                      ),
                                      mainPanel(
                                        # tabsetPanel(type = "tabs",
                                        #             tabPanel("Datos", tableOutput("contents")),
                                        #             tabPanel("Resumen", verbatimTextOutput('summary')))
                                        column(3, wellPanel("Datos", tableOutput("contents"))),
                                        column(9, wellPanel("Resumen", verbatimTextOutput('summary')))
                                      )

                                    )
                           ),
                           tabPanel("Análisis",
                                    titlePanel("Analisis de Datos"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("bins",
                                                    "Número de bloques",
                                                    value = 15,
                                                    min = 1,
                                                    max = 100),
                                        tags$hr(),
                                        textInput("start",
                                                  label = "Inicio de la serie",
                                                  value = "2000,1"),
                                        sliderInput("frecuency",
                                                    "Frecuencia de la serie",
                                                    value = 4,
                                                    min = 1,
                                                    max = 365)
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
                           tabPanel("Modelado",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Tab 1",
                                                 h4("Datos"),
                                                 tableOutput("table"), #Output: aca van los datos de la tabla
                                                 h4("Verbatim text output"),
                                                 verbatimTextOutput("txtout"),
                                                 h1("Header 1"),
                                                 h2("Header 2"),
                                                 h3("Header 3"),
                                                 h4("Header 4"),
                                                 h5("Header 5")
                                        ),
                                        tabPanel("Tab 2", "This panel is intentionally left blank"),
                                        tabPanel("Tab 3", "This panel is intentionally left blank")
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
    #Gráfico 1.1: Histograma
    histograma(datosVec, input$bins)
    densidad.teorica(datosVec,
                     media = estadisticos[["Media"]],
                     desest = estadisticos[["Desviación estándar"]])
    densidad.empirica(datosVec)
    densidad.leyenda()
  })
  output$qqplot <- renderPlot({
    req(input$file1)
    #Gráfico 1.2: QQPlot
    qqplot <- cuartil.cuartil(datosVec)
    qqplot$puntos
    qqplot$linea
  })
  output$serie <- renderPlot({
    req(input$file1)
    #Gráfico 1.3: Serie de tiempo
    inicio <- unlist(strsplit(input$start, ','))
    inicio1 <- ifelse(test = is.na(inicio[1]), yes = 2000, no = as.integer(inicio[1]))
    inicio2 <- ifelse(test = is.na(inicio[2]), yes = 1, no = as.integer(inicio[2]))
    serie.tiempo(datosVec,
                 inicio = c(inicio1, inicio2),
                 frecuencia = input$frecuency)
  })
  output$ecdf <- renderPlot({
    req(input$file1)
    #Gráfico 1.4: Densidad acumulada
    densidad.acumulada(datosVec)
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


  # MODELADO DE SERIES DE TIEMPO


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
  totales <<- list("Registros totales" = total,
                   "Registros numéricos" = totalNM,
                   "Registros no numéricos" = totalNN,
                   "Registros nulos" = totalNA)

  datosVec <<- datos[,columna]
  datos <<- data.frame(datosVec)
}
calcular.estadisticos <- function(columna = 1, tolerancia = 30, replicas = 1000) {
  media <- round(mean(datos[,columna]), 2)
  desest <- round(sd(datos[,columna]), 4)
  minimo <- round(min(datos[,columna]), 2)
  maximo <- round(max(datos[,columna]), 2)
  bootstrap <- FALSE
  if(length(datos[,columna]) < tolerancia) {
    resultado <- calcular.boot(datos[,columna], replicas)
    media <- round(resultado$media, 2)
    desest <- round(resultado$desest, 2)
    bootstrap <- TRUE
  }
  estadisticos <<- list(Media = media,
                        "Desviación estándar" = desest,
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
       desest = sd(resultado$t[,1]))
}


######################################################################
### FUNCIONES PARA ANALISIS DE DATOS
######################################################################
histograma <- function(datos, bloques = "Sturges") {
  grafico <- hist(datos,
                  probability = TRUE,
                  breaks = bloques,
                  col = "aliceblue", #col = "#75AADB",
                  border = "gray",
                  main = "Histograma",
                  xlab = "Cuantiles",
                  ylab = "Densidad")
  return(grafico)
}
densidad.teorica <- function(datos, media, desest, muestra = 1000) {
  xt <- seq(from = min(datos), to = max(datos), length.out = muestra)
  yt <- dnorm(xt, mean = media, sd = desest)
  grafico <- lines(xt, yt, col = "cornflowerblue", lwd = 2)
  return(grafico)
}
densidad.empirica <- function(datos) {
  densidad <- density(datos)
  grafico <- lines(densidad, col = "brown4", lwd = 2)
  return(grafico)
}
densidad.acumulada <- function(datos) {
  grafico <- plot(ecdf(datos),
                  col = "black",
                  main = "Densidad Acumulada Empírica")
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
         horiz = FALSE,
         merge = FALSE)
}
cuartil.cuartil <- function(datos) {
  retorno <- list(puntos = qqnorm(datos,
                                  xlab = "Cuantiles Teóricos",
                                  ylab = "Muestra"),
                  linea = qqline(datos))
  return(retorno)
}
serie.tiempo <- function(datos, inicio = c(1900, 1), frecuencia = 1) {
  datosSerie <<- ts(datos,
                    start = inicio,
                    frequency = frecuencia)
  retorno <- plot(datosSerie,
                  col = "brown4",
                  pch = 20,
                  type = "l",
                  main = "Serie de tiempo",
                  xlab = "Tiempo",
                  ylab = "Valores")

  return(retorno)
}


######################################################################
### FUNCIONES PARA MODELADO DE DATOS
######################################################################



######################################################################
### DEFINICIÓN FUNCIÓN PARA EJECUTAR PROGRAMA
######################################################################
ejecutar <- function() {
  runApp(shinyApp(ui = ui, server = server))
}


#shinyApp(ui = ui, server = server)


