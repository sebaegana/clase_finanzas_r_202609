#install.packages(c("shiny", "readr", "dplyr", "tidyr", "ggplot2", "scales"))


library(shiny)
library(readr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Datos y parámetros
# -----------------------------------------------------------------------------

rutas_datos <- c(
  "data/portafolio_retornos.csv",
  "../data/portafolio_retornos.csv",
  "clases_ia/data/portafolio_retornos.csv"
)

ruta_datos <- rutas_datos[file.exists(rutas_datos)][1]

if (is.na(ruta_datos)) {
  stop(
    "No se encontró portafolio_retornos.csv. Ejecuta la app desde "
    , "la carpeta shiny_portafolio o desde la raíz del repositorio."
  )
}

datos <- read_csv(ruta_datos, show_col_types = FALSE)
activos <- c("SPY", "EFA", "IJS", "EEM", "AGG")
retornos <- as.matrix(datos[activos])
retornos_medios <- colMeans(retornos)
covarianzas <- cov(retornos)

# -----------------------------------------------------------------------------
# 2. Funciones financieras
# -----------------------------------------------------------------------------

normalizar_pesos <- function(pesos) {
  if (sum(pesos) == 0) {
    return(rep(1 / length(pesos), length(pesos)))
  }

  pesos / sum(pesos)
}

calcular_metricas <- function(pesos, retornos_medios, covarianzas,
                              tasa_libre = 0.02) {
  retorno <- sum(pesos * retornos_medios) * 12
  varianza <- as.numeric(t(pesos) %*% covarianzas %*% pesos)
  volatilidad <- sqrt(varianza) * sqrt(12)
  sharpe <- (retorno - tasa_libre) / volatilidad

  list(
    retorno = retorno,
    volatilidad = volatilidad,
    sharpe = sharpe
  )
}

# -----------------------------------------------------------------------------
# 3. Simulación para el gráfico
# -----------------------------------------------------------------------------

set.seed(42)
simulacion <- replicate(3000, {
  pesos <- normalizar_pesos(runif(length(activos)))
  metricas <- calcular_metricas(pesos, retornos_medios, covarianzas)

  c(
    retorno = metricas$retorno,
    volatilidad = metricas$volatilidad,
    sharpe = metricas$sharpe
  )
})

simulacion <- as.data.frame(t(simulacion))

# -----------------------------------------------------------------------------
# 4. Interfaz de usuario
# -----------------------------------------------------------------------------

crear_slider <- function(activo, valor) {
  sliderInput(
    inputId = paste0("peso_", tolower(activo)),
    label = paste0("Peso de ", activo, " (%)"),
    min = 0,
    max = 100,
    value = valor,
    step = 1
  )
}

ui <- fluidPage(
  titlePanel("Laboratorio de Portafolios"),

  p("Modifica los pesos y observa el efecto sobre riesgo y retorno."),

  sidebarLayout(
    sidebarPanel(
      h4("Pesos del portafolio"),
      crear_slider("SPY", 40),
      crear_slider("EFA", 20),
      crear_slider("IJS", 10),
      crear_slider("EEM", 10),
      crear_slider("AGG", 20),
      hr(),
      actionButton("conservador", "Conservador"),
      actionButton("balanceado", "Balanceado"),
      actionButton("agresivo", "Agresivo")
    ),

    mainPanel(
      h3("Resultados"),
      textOutput("pesos_finales"),
      tableOutput("metricas"),
      plotOutput("grafico_riesgo_retorno", height = "500px")
    )
  )
)

# -----------------------------------------------------------------------------
# 5. Servidor reactivo
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  pesos_reactivos <- reactive({
    pesos <- c(
      input$peso_spy,
      input$peso_efa,
      input$peso_ijs,
      input$peso_eem,
      input$peso_agg
    )

    normalizar_pesos(pesos)
  })

  metricas_reactivas <- reactive({
    calcular_metricas(
      pesos = pesos_reactivos(),
      retornos_medios = retornos_medios,
      covarianzas = covarianzas
    )
  })

  output$pesos_finales <- renderText({
    pesos <- round(pesos_reactivos() * 100, 1)
    nombres <- paste(activos, pesos, "%", sep = " ")
    paste("Pesos normalizados:", paste(nombres, collapse = " | "))
  })

  output$metricas <- renderTable({
    metricas <- metricas_reactivas()

    data.frame(
      Metrica = c("Retorno anual", "Volatilidad anual", "Ratio de Sharpe"),
      Valor = c(
        paste0(round(metricas$retorno * 100, 2), "%"),
        paste0(round(metricas$volatilidad * 100, 2), "%"),
        round(metricas$sharpe, 3)
      ),
      check.names = FALSE
    )
  })

  output$grafico_riesgo_retorno <- renderPlot({
    elegido <- metricas_reactivas()

    ggplot(simulacion, aes(x = volatilidad, y = retorno, color = sharpe)) +
      geom_point(alpha = 0.35, size = 1.5) +
      geom_point(
        data = data.frame(
          volatilidad = elegido$volatilidad,
          retorno = elegido$retorno
        ),
        aes(x = volatilidad, y = retorno),
        color = "red",
        size = 5
      ) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Riesgo y retorno de portafolios",
        subtitle = "El punto rojo corresponde a tu selección",
        x = "Volatilidad anual",
        y = "Retorno anual",
        color = "Sharpe"
      ) +
      theme_minimal()
  })

  observeEvent(input$conservador, {
    updateSliderInput(session, "peso_spy", value = 15)
    updateSliderInput(session, "peso_efa", value = 10)
    updateSliderInput(session, "peso_ijs", value = 5)
    updateSliderInput(session, "peso_eem", value = 5)
    updateSliderInput(session, "peso_agg", value = 65)
  })

  observeEvent(input$balanceado, {
    updateSliderInput(session, "peso_spy", value = 30)
    updateSliderInput(session, "peso_efa", value = 15)
    updateSliderInput(session, "peso_ijs", value = 10)
    updateSliderInput(session, "peso_eem", value = 10)
    updateSliderInput(session, "peso_agg", value = 35)
  })

  observeEvent(input$agresivo, {
    updateSliderInput(session, "peso_spy", value = 40)
    updateSliderInput(session, "peso_efa", value = 25)
    updateSliderInput(session, "peso_ijs", value = 15)
    updateSliderInput(session, "peso_eem", value = 15)
    updateSliderInput(session, "peso_agg", value = 5)
  })
}

shinyApp(ui = ui, server = server)