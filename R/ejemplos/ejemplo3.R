# y = AirPassengers
# plot(y, type='l')
# hw = HoltWinters(y)
# plot(hw)
# summary(hw)
# attributes(hw)
# str(hw)
#
# hw$x
# attributes(hw$fitted)
#
# hw$fitted[,1]
#
# observe({
#   if (input$product != "A" && input$month !="All") {
#     shinyjs::disable(selector = "[type=radio][value=trendp]")
#     shinyjs::runjs("$('[type=radio][value=trendp]').parent().parent().addClass('disabled').css('opacity', 0.4)")
#   }
# })
#
#
# fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
# fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
# accuracy(fit1)
# accuracy(fit2)
# accuracy(fit1,EuStockMarkets[201:300,1])
# accuracy(fit2,EuStockMarkets[201:300,1])
# plot(fit1)
# lines(EuStockMarkets[1:300,1])

# library(shiny)
# runApp(list(
#   ui = bootstrapPage(
#     numericInput('n', 'Number of obs', 100),
#     textOutput('text1'),
#     tags$head(tags$style("#text1{color: red;
#                          font-size: 20px;
#                          font-style: italic;
#                          }"
#     )
#     )
#   ),
#   server = function(input, output) {
#     output$text1 <- renderText({ paste("hello input is",input$n) })
#   }
# ))


# library(shiny)
# library(shinydashboard)
# library(DT)
# library(shinyjs) # requires shinyjs !
#
# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(checkboxInput("chk", label = "NULL: ", value = T)),
#   dashboardBody(useShinyjs(),
#                 box(width = 8, DT::dataTableOutput("test"))
#   )
# )
#
# server <- function(input, output, session) {
#   observeEvent(input$chk, {
#     if (input$chk) hide("test") else show("test")
#   })
#
#   output$test <- DT::renderDataTable({
#     datatable(data.frame(A = 1:20, B = 2*1:20))
#   })
# }
#
# shinyApp(ui = ui, server = server)

# datos <- c(5, c(6, 3), 4, -1, 6, 7, -5)
# datos
# grado <- 2
# vector <- seq(1:(length(datos)))
# vector
# tiempo <- matrix(rep(0, grado * length(datos)), nrow = length(datos), ncol = grado)
# tiempo
# tiempo[,1] <- vector
# for(i in 1:grado) {
#   tiempo[,i] <- vector
# }
# tiempo


