library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Mi Primera App Shiny"),

  sidebarLayout(
    sidebarPanel(
      h3("Controles"),
      sliderInput("num", "Elige un número:", min = 1, max = 100, value = 50),
      textInput("texto", "Escribe algo:", value = "Hola")
    ),

    mainPanel(
      h3("Resultados"),
      textOutput("resultado"),
      br(),
      textOutput("doble")
    )
  )
)

# Server
server <- function(input, output) {
  output$resultado <- renderText({
    paste("Tu número es:", input$num)
  })

  output$doble <- renderText({
    paste("El doble es:", input$num * 2, "y escribiste:", input$texto)
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)