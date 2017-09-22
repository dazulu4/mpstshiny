
library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  #shinythemes::themeSelector(),
  navbarPage("R-mpstshiny",

             tabPanel("Cargar",
                      sidebarPanel(
                        titlePanel(h3("Carga de archivos")),
                        fileInput("file1", "Choose CSV File", buttonLabel = "Explorar...",placeholder = "Sin archivo",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),

                        # Horizontal line ----
                        tags$hr(),

                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),

                        # Input: Select separator ----
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),

                        # Input: Select quotes ----
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),

                        # Horizontal line ----
                        tags$hr(),

                        # Input: Select number of rows to display ----
                        radioButtons("disp", "Display",
                                     choices = c(Head = "head",
                                                 All = "all"),
                                     selected = "head")
                      ),
                      mainPanel(
                        column(6, wellPanel("Datos:",
                                  tableOutput("contents")
                                )),
                        column(6, wellPanel("Estadisticos:"
                        ))
                      )
                      ),
             tabPanel("Análisis"),
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
                      ))

  )
))
