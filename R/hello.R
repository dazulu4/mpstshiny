library(shiny)

mymyData <- c()

# Define the UI
ui <- bootstrapPage(plotOutput('plot'))

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(myData, type='l', col='red')
  })
}

myshinyplot <- function(x)
{
  myData <<- x
  runApp(shinyApp(ui = ui, server = server ))
}

mymyData <- rnorm(50)
myshinyplot(mymyData)
