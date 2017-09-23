# library(shiny)
# library(boot)
# library(data.table)
#
# datos <- list()
# datosVec <- c()
# datosSerie <- list()
# totales <- list()
# estadisticos <- list()
#
# ui <- fluidPage(
#   titlePanel("Carga de Archivo"),
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("file1", "Selecciona archivo de texto",
#                 buttonLabel = "Explorar...",
#                 placeholder = "Sin archivo",
#                 multiple = TRUE,
#                 accept = c("text/csv",
#                            "text/comma-separated-values,text/plain",
#                            ".csv")),
#       #tags$hr(),
#       checkboxInput("header", "Encabezado", FALSE),
#       radioButtons("sep", "Separador",
#                    choices = c(Coma = ",",
#                                "Punto y Coma" = ";",
#                                Tabulador = "\t"),
#                    selected = ","),
#       #radioButtons("quote", "Comillas",
#       #             choices = c(Ninguna = "",
#       #                         "Comillas Dobles" = '"',
#       #                         "Comillas Simples" = "'"),
#       #             selected = '"'),
#       #tags$hr(),
#       radioButtons("disp", "Mostrar Datos",
#                    choices = c(Encabezado = "head",
#                                "Compleo" = "all"),
#                    selected = "head"),
#       tags$hr(),
#       numericInput("column",
#                    label = "Columna del archivo",
#                    value = 1),
#       sliderInput("tolerance",
#                   "Tolerancia bootstrap",
#                   value = 30,
#                   min = 20,
#                   max = 200),
#       sliderInput("bootstrap",
#                   "Replicas bootstrap",
#                   value = 1000,
#                   min = 500,
#                   max = 5000)
#     ),
#     mainPanel(
#       # tabsetPanel(type = "tabs",
#       #             tabPanel("Datos", tableOutput("contents")),
#       #             tabPanel("Resumen", verbatimTextOutput('summary')))
#       column(3, wellPanel("Datos", tableOutput("contents"))),
#       column(9, wellPanel("Resumen", verbatimTextOutput('summary')))
#     )
#
#   )
# )
#
# server <- function(input, output) {
#
#   output$contents <- renderTable({
#     req(input$file1)
#     cargar.archivo(archivo = input$file1$datapath,
#                    encabezado = input$header,
#                    separador = input$sep)
#     #comillas = input$quote)
#     if(input$disp == "head") {
#       return(head(datos))
#     }
#     else {
#       return(datos)
#     }
#   })
#
#   output$summary <- renderPrint({
#     req(input$file1)
#     calcular.totales(columna = input$column)
#     calcular.estadisticos(columna = input$column,
#                           tolerancia = input$tolerance,
#                           replicas = input$bootstrap)
#     resultado <- list("Totales" = totales,
#                       "Estadísticos" = estadisticos)
#     str(sapply(sprintf('%s', names(resultado)), function(key) {
#       resultado[[key]]
#     }, simplify = FALSE))
#   })
#
# }
#
# ######################################################################
# ### FUNCIONES PARA CARGA DE ARCHIVOS
# ######################################################################
# cargar.archivo <- function(archivo,
#                            encabezado = FALSE,
#                            separador = ',',
#                            comillas = '"') {
#   datos <<- read.csv(file = archivo,
#                      header = encabezado,
#                      sep = separador,
#                      quote = comillas,
#                      fill = TRUE)
# }
# calcular.totales <- function(columna = 1) {
#   total <- length(datos[,columna])
#   totalNA <- sum(is.na(datos[,columna]))
#   datos <<- na.omit(datos[,columna])
#   datosNN <- suppressWarnings(as.numeric(as.character(datos)))
#   totalNN <- sum(is.na(datosNN))
#   datos <<- na.omit(data.frame(datosNN))
#   totalNM <- length(datos[,columna])
#   totales <<- list("Registros totales" = total,
#                    "Registros numéricos" = totalNM,
#                    "Registros no numéricos" = totalNN,
#                    "Registros nulos" = totalNA)
#
#   datosVec <<- datos[,columna]
#   datos <<- data.frame(datosVec)
# }
# calcular.estadisticos <- function(columna = 1, tolerancia = 30, replicas = 1000) {
#   media <- round(mean(datos[,columna]), 2)
#   desest <- round(sd(datos[,columna]), 4)
#   minimo <- round(min(datos[,columna]), 2)
#   maximo <- round(max(datos[,columna]), 2)
#   bootstrap <- FALSE
#   if(length(datos[,columna]) < tolerancia) {
#     resultado <- calcular.boot(datos[,columna], replicas)
#     media <- round(resultado$media, 2)
#     desest <- round(resultado$desest, 2)
#     bootstrap <- TRUE
#   }
#   estadisticos <<- list(Media = media,
#                         "Desviación estándar" = desest,
#                         "Mínimo" = minimo,
#                         "Máximo" = maximo,
#                         Bootstrap = bootstrap)
# }
# calcular.media <- function(x, d) {
#   return(mean(x[d]))
# }
# calcular.boot <- function(x, r = 1000) {
#   resultado <- boot(x, statistic = calcular.media, R = r)
#   list(media = resultado$t0,
#        desest = sd(resultado$t[,1]))
# }
#
# shinyApp(ui, server)
