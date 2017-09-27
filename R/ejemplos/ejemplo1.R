# require(graphics)
#
# ## Datos de la serie
# x <- ts(data = c( 12.82, 13.03, 11.58, 11.77,
#                   12.84, 13.07, 12.36, 12.14,
#                   12.96, 13.63, 12.83, 12.16,
#                   13.98, 13.88, 13.14, 13.27,
#                   13.90, 14.24, 13.46, 13.25,
#                   14.54, 14.45, 14.08, 13.83),
#         freq = 4,
#         start = c(2010,01))
#
# t <- seq(1:length(x))
# It <- seasonaldummy(x)
# m <- lm(x ~ t + It)
# attributes(m)
# # n <- data.frame(tiempo = seq(tsp(x)[2] + deltat(x), by = deltat(x), length.out = 24))
# n <- data.frame(tiempo = t)
# py <- predict(m, n, se.fit = TRUE)
# pp <- predict(m, n, interval = "prediction")
# pc <- predict(m, n, interval = "confidence")
#
# y <- ts(data = py$fit,
#         freq = 4,
#         start = c(2010,01))
#
# plot(x, type = "l")
# lines(y, col="red")
# grid()



# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   geom_smooth()
#
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   geom_smooth(span = 0.3)
#
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
#
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)


# x <- rnorm(1000)
# y <- density(x)
# str(y)

# shinyApp(
#   ui = fluidPage(
#     dateInput("dateSelect","Date"),
#     verbatimTextOutput("out"),
#     textOutput("text")
#   ),
#   server = function(input,output,session){
#
#     output$text <- renderText({
#       if(!isTruthy(input$dateSelect)){
#         "NO DATE"
#       } else {
#         paste("The chosen date is:",input$dateSelect)
#       }
#     })
#     output$out<- renderPrint({str(input$dateSelect)})
#   }
# )
