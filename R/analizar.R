# library(shiny)
# library(moments)
# library(nortest)
# require(graphics)
# library(Cairo)
#
# #TODO no se requiere, variable global ################
# datos <- data.frame(rnorm(183))
# datosVec <- c(datos[,1])
# datosSerie <- list()
# totales <- list()
# estadisticos <- list("Media" = median(datos[,1]),
#                      "Desviación estándar" = sd(datos[,1]))
# ######################################################
#
# ui <- fluidPage(
#   titlePanel("Analisis de Datos"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Número de bloques",
#                   value = 15,
#                   min = 1,
#                   max = 100),
#       tags$hr(),
#       textInput("start",
#                 label = "Inicio de la serie",
#                 value = "1900,1"),
#       sliderInput("frecuency",
#                   "Frecuencia de la serie",
#                   value = 4,
#                   min = 1,
#                   max = 365)
#     ),
#     mainPanel(
#       tabsetPanel(type = "tabs",
#                   tabPanel("Densidad Normal",
#                            plotOutput("normal")),
#                   tabPanel("Densidad Acumulada", plotOutput("ecdf")),
#                   tabPanel("Pruebas Normalidad",
#                            plotOutput("qqplot"),
#                            verbatimTextOutput('test')),
#                   tabPanel("Serie Tiempo", plotOutput("serie"))
#       )
#     )
#   )
# )
#
# # Define server logic for random distribution app ----
# server <- function(input, output) {
#
#   datosVec <- datos[,1] #TODO input$column
#
#   output$normal <- renderPlot({
#     #Gráfico 1.1: Histograma
#     histograma(datosVec, input$bins)
#     densidad.teorica(datosVec,
#                      media = estadisticos[["Media"]],
#                      desest = estadisticos[["Desviación estándar"]])
#     densidad.empirica(datosVec)
#
#   })
#
#   output$qqplot <- renderPlot({
#     #Gráfico 1.2: QQPlot
#     qqplot <- cuartil.cuartil(datosVec)
#     qqplot$puntos
#     qqplot$linea
#   })
#
#   output$serie <- renderPlot({
#     #Gráfico 1.3: Serie de tiempo
#     inicio <- unlist(strsplit(input$start, ','))
#     inicio1 <- ifelse(test = is.na(inicio[1]), yes = 1900, no = as.integer(inicio[1]))
#     inicio2 <- ifelse(test = is.na(inicio[2]), yes = 1, no = as.integer(inicio[2]))
#     serie.tiempo(datosVec,
#                  inicio = c(inicio1, inicio2),
#                  frecuencia = input$frecuency)
#   })
#
#   output$ecdf <- renderPlot({
#     #Gráfico 1.4: Densidad acumulada
#     densidad.acumulada(datosVec)
#   })
#
#   output$test <- renderPrint({
#     jb <- jarque.test(datosVec)
#     sw <- shapiro.test(datosVec)
#     ad <- ad.test(datosVec)
#     data.frame("Estadístico" = c(jb$statistic,
#                                  sw$statistic,
#                                  ad$statistic),
#                "Valor-P" = c(jb$p.value,
#                              sw$p.value,
#                              ad$p.value),
#                row.names = c("Jarque-Bera",
#                              "Shapiro-Wilk",
#                              "Anderson-Darling"))
#   })
#
#
# }
#
# ######################################################################
# ### FUNCIONES
# ######################################################################
# histograma <- function(datos, bloques = "Sturges") {
#   grafico <- hist(datos,
#                   probability = TRUE,
#                   breaks = bloques,
#                   col = "aliceblue", #col = "#75AADB",
#                   border = "gray",
#                   main = "Histograma",
#                   xlab = "Cuantiles",
#                   ylab = "Densidad")
#   return(grafico)
# }
# densidad.teorica <- function(datos, media, desest, muestra = 1000) {
#   xt <- seq(from = min(datos), to = max(datos), length.out = muestra)
#   yt <- dnorm(xt, mean = media, sd = desest)
#   grafico <- lines(xt, yt, col = "cornflowerblue", lwd = 2)
#   return(grafico)
# }
# densidad.empirica <- function(datos) {
#   densidad <- density(datos)
#   grafico <- lines(densidad, col = "brown4", lwd = 2)
#   return(grafico)
# }
# densidad.acumulada <- function(datos) {
#   grafico <- plot(ecdf(datos),
#                   col = "black",
#                   main = "Densidad Acumulada Empírica")
#   return(grafico)
# }
# cuartil.cuartil <- function(datos) {
#   retorno <- list(puntos = qqnorm(datos,
#                                   xlab = "Cuantiles Teóricos",
#                                   ylab = "Muestra"),
#                   linea = qqline(datos))
#   return(retorno)
# }
# serie.tiempo <- function(datos, inicio = c(1900, 1), frecuencia = 1) {
#   datosSerie <<- ts(datos,
#                     start = inicio,
#                     frequency = frecuencia)
#   retorno <- plot(datosSerie,
#                   col = "brown4",
#                   pch = 20,
#                   type = "l",
#                   main = "Serie de tiempo",
#                   xlab = "Tiempo",
#                   ylab = "Valores")
#
#   return(retorno)
# }
# ######################################################################
# ######################################################################
#
# shinyApp(ui, server)
#
