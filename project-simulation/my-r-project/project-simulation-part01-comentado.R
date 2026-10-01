## ============================================================================
## PROYECTO: ANÁLISIS DE PORTAFOLIOS - MARKOWITZ vs SIMULACIÓN MONTE CARLO
## ============================================================================

# SECCIÓN 1: CARGAR LIBRERÍAS ================================================
library(readr)      # Lectura de archivos CSV
library(tidyverse)  # Suite de manipulación (dplyr, tidyr, ggplot2)
library(skimr)      # Resumen estadístico rápido
library(corrplot)   # Visualización de correlaciones
library(kableExtra) # Tablas HTML formateadas
library(quadprog)   # Programación cuadrática para optimización
library(ggplot2)    # Visualizaciones avanzadas
library(viridis)    # Paletas de colores perceptualmente uniformes
library(patchwork)  # Combinación de gráficos

# SECCIÓN 2: CARGAR DATOS ===================================================
portafolio_retornos <- read_csv("data/portafolio_retornos.csv")
# Datos mensuales de 5 activos: SPY, EFA, IJS, EEM, AGG (2012-2021)

# SECCIÓN 3: EXPLORACIÓN PRELIMINAR =========================================
head(portafolio_retornos)  # Ver primeras 6 filas
str(portafolio_retornos)   # Estructura: 108 filas × 6 columnas

# SECCIÓN 4: ESTADÍSTICAS DESCRIPTIVAS ======================================
# Resumen rápido de los 5 activos de interés
summary(portafolio_retornos |> select(SPY, EFA, IJS, EEM, AGG))

# Estadísticas de todos (excluyen fecha)
summary(portafolio_retornos |> select(-fecha))
skim(portafolio_retornos |> select(-fecha))

# Tabla de estadísticas por activo (media, sd, min, max, n)
stats_por_activo <- portafolio_retornos |>
  select(-fecha) |>
  summarise(across(everything(),
    list(
      media = ~mean(., na.rm = TRUE),      # Media
      sd = ~sd(., na.rm = TRUE),           # Desviación estándar
      min = ~min(., na.rm = TRUE),         # Mínimo
      max = ~max(., na.rm = TRUE),         # Máximo
      n = ~n()                             # Número de obs
    ),
    .names = "{.col}_{.fn}"
  ))

print(stats_por_activo)

# SECCIÓN 5: GRÁFICOS EXPLORATORIOS =========================================
# Histograma: Distribución de retornos por activo
portafolio_retornos |>
  select(-fecha) |>
  pivot_longer(everything(), names_to = "activo", values_to = "retorno") |>
  ggplot(aes(x = retorno, fill = activo)) +
  geom_histogram(alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~activo, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distribución de Retornos Mensuales (2012-2021)",
    x = "Retorno",
    y = "Frecuencia"
  ) +
  theme(legend.position = "bottom")

# Gráfico de series de tiempo: Retornos en el período
portafolio_retornos |>
  mutate(fecha = as.Date(fecha)) |>
  pivot_longer(-fecha, names_to = "activo", values_to = "retorno") |>
  ggplot(aes(x = fecha, y = retorno, color = activo)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~activo, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Retornos Mensuales en el Tiempo",
    x = "Fecha",
    y = "Retorno"
  ) +
  theme(legend.position = "bottom")

# SECCIÓN 6: ANÁLISIS DE CORRELACIÓN ========================================
# Convertir a matriz de retornos (sin fecha)
retornos_matrix <- portafolio_retornos |>
  select(-fecha) |>
  as.matrix()

# Matriz de correlación (Pearson)
correlacion <- cor(retornos_matrix)
corrplot(correlacion,
         method = "circle",
         type = "upper",
         main = "Matriz de Correlaciones entre Activos",
         mar = c(0, 0, 2, 0))

# Matriz de covarianza (necesaria para optimización)
covarianza <- cov(retornos_matrix)
cat("\n=== MATRIZ DE COVARIANZA ===\n")
print(round(covarianza, 6))

# Tabla ordenada de correlaciones (pares únicos, ordenados por magnitud)
correlacion_df <- as.data.frame(correlacion) |>
  rownames_to_column("activo1") |>
  pivot_longer(-activo1, names_to = "activo2", values_to = "corr") |>
  filter(as.character(activo1) < as.character(activo2)) |>  # Evitar duplicados
  arrange(desc(abs(corr)))  # Ordenar por correlación absoluta

kable(correlacion_df, digits = 3, caption = "Correlaciones entre Activos")

# SECCIÓN 7: OPTIMIZACIÓN MARKOWITZ (FRONTERA EFICIENTE) ====================
# Calcular retornos esperados y matriz de covarianza
retornos_esperados <- colMeans(retornos_matrix)  # Media de retornos
cov_matrix <- cov(retornos_matrix)               # Matriz de varianzas-covarianzas
tasa_libre_riesgo <- 0.02 / 12                   # Tasa mensual = 2% anual / 12

# Número de activos y rango de retornos objetivo
n_activos <- ncol(retornos_matrix)
retornos_objetivo <- seq(0.08, 0.20, length.out = 50) / 12  # Retornos mensuales objetivo

# Inicializar lista para almacenar resultados de frontera
frontera_markowitz <- list(
  retorno = numeric(length(retornos_objetivo)),    # Retornos anualizados
  volatilidad = numeric(length(retornos_objetivo)),# Volatilidades anualizadas
  sharpe = numeric(length(retornos_objetivo)),     # Ratios de Sharpe
  pesos = matrix(NA, nrow = length(retornos_objetivo), ncol = n_activos)  # Pesos óptimos
)

# Loop: Resolver problema de optimización para cada retorno objetivo
for (i in seq_along(retornos_objetivo)) {
  r_obj <- retornos_objetivo[i]

  # Formulación estándar para quadprog: minimizar w'*H*w + f'*w
  H <- 2 * cov_matrix              # Matriz Hessiana (2 × covarianza)
  f <- rep(0, n_activos)           # Coeficientes lineales (cero)

  # Restricciones: A*w >= b0, primeras meq como igualdad
  A <- cbind(
    rep(1, n_activos),             # Restricción: sum(w) = 1
    retornos_esperados,            # Restricción: w'*r = r_obj
    diag(n_activos)                # Restricción: w >= 0 (long-only)
  )

  b0 <- c(1, r_obj, rep(0, n_activos))

  # Resolver con manejo de errores
  tryCatch({
    resultado <- solve.QP(H, f, A, b0, meq = 2)  # Resolver cuadrático
    w_opt <- resultado$solution                    # Pesos óptimos
    vol_opt <- sqrt(t(w_opt) %*% cov_matrix %*% w_opt) * sqrt(12)  # Volatilidad anualizada

    # Guardar resultados
    frontera_markowitz$retorno[i] <- r_obj * 12
    frontera_markowitz$volatilidad[i] <- vol_opt
    frontera_markowitz$pesos[i, ] <- w_opt
  }, error = function(e) {
    warning(paste("solve.QP falló en iteración", i))
  })
}

# Calcular Sharpe ratio = (retorno - tasa libre riesgo) / volatilidad
frontera_markowitz$sharpe <- ifelse(
  frontera_markowitz$volatilidad > 1e-10,
  (frontera_markowitz$retorno - 0.02) / frontera_markowitz$volatilidad,
  NA
)

# Identificar portafolio tangente (máximo Sharpe ratio)
idx_tangente <- which.max(frontera_markowitz$sharpe)
portafolio_tangente <- list(
  retorno = frontera_markowitz$retorno[idx_tangente],
  volatilidad = frontera_markowitz$volatilidad[idx_tangente],
  sharpe = frontera_markowitz$sharpe[idx_tangente],
  pesos = frontera_markowitz$pesos[idx_tangente, ]
)

cat("\n=== PORTAFOLIO TANGENTE (MARKOWITZ) ===\n")
cat("Retorno: ", round(portafolio_tangente$retorno * 100, 3), "%\n")
cat("Volatilidad: ", round(portafolio_tangente$volatilidad * 100, 3), "%\n")
cat("Sharpe Ratio: ", round(portafolio_tangente$sharpe, 4), "\n")

# Gráfico: Frontera eficiente de Markowitz
frontera_markowitz_df <- as.data.frame(frontera_markowitz) |>
  mutate(
    retorno_pct = retorno * 100,
    volatilidad_pct = volatilidad * 100
  )

portafolio_tangente_plot <- data.frame(
  retorno = portafolio_tangente$retorno * 100,
  volatilidad = portafolio_tangente$volatilidad * 100,
  sharpe = portafolio_tangente$sharpe
)

ggplot(frontera_markowitz_df, aes(x = volatilidad_pct, y = retorno_pct)) +
  geom_line(linewidth = 1.2, color = "darkred", alpha = 0.8) +
  geom_point(aes(color = sharpe), size = 3, alpha = 0.6) +
  geom_point(data = portafolio_tangente_plot,
             aes(x = volatilidad, y = retorno),
             color = "gold", size = 5, shape = 17, inherit.aes = FALSE) +
  geom_hline(yintercept = 2, linetype = "dashed",
             color = "gray50", alpha = 0.7, linewidth = 0.8) +
  scale_color_gradient(name = "Sharpe Ratio", low = "lightblue", high = "navy") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Frontera Eficiente de Markowitz",
    subtitle = "Triángulo Dorado = Portafolio Tangente (Mayor Sharpe)",
    x = "Volatilidad Anual (%)",
    y = "Retorno Anual Esperado (%)"
  )

# SECCIÓN 8: SIMULACIÓN MONTE CARLO (10,000 PORTAFOLIOS ALEATORIOS) ========
set.seed(42)  # Reproducibilidad

n_sims <- 10000  # Número de simulaciones

# Generar pesos aleatorios normalizados (long-only)
pesos_matrix <- matrix(NA, nrow = n_sims, ncol = n_activos)

for (i in 1:n_sims) {
  w <- runif(n_activos)      # Pesos aleatorios uniformes
  w <- w / sum(w)            # Normalizar a suma = 1
  pesos_matrix[i, ] <- w
}

# Calcular retorno, volatilidad y Sharpe para cada portafolio simulado
retornos_port <- pesos_matrix %*% retornos_esperados * 12  # Retorno anualizado
volatilidades_port <- sqrt(diag(pesos_matrix %*% cov_matrix %*% t(pesos_matrix))) * sqrt(12)  # Vol anualizada
sharpe_ratios <- (retornos_port - 0.02) / volatilidades_port  # Sharpe ratio

# Compilar resultados en tibble
resultados_sim <- tibble(
  retorno_anual = as.numeric(retornos_port),
  volatilidad_anual = as.numeric(volatilidades_port),
  sharpe_ratio = as.numeric(sharpe_ratios)
)

# Identificar mejor portafolio simulado
idx_mejor <- which.max(resultados_sim$sharpe_ratio)
mejor_sim <- resultados_sim[idx_mejor, ]
mejor_pesos_sim <- pesos_matrix[idx_mejor, ]

cat("\n=== MEJOR PORTAFOLIO SIMULADO ===\n")
cat("Retorno: ", round(mejor_sim$retorno_anual * 100, 3), "%\n")
cat("Volatilidad: ", round(mejor_sim$volatilidad_anual * 100, 3), "%\n")
cat("Sharpe Ratio: ", round(mejor_sim$sharpe_ratio, 4), "\n")

# SECCIÓN 9: HISTOGRAMAS DE DISTRIBUCIONES =================================
# Preparar datos en porcentaje para visualización
resultados_sim_plot <- resultados_sim |>
  mutate(
    retorno_pct = retorno_anual * 100,
    volatilidad_pct = volatilidad_anual * 100
  )

# Histograma 1: Distribución de retornos
h_retorno <- ggplot(resultados_sim_plot, aes(x = retorno_pct)) +
  geom_histogram(bins = 40, fill = viridis(1, option = "plasma"),
                 color = "white", alpha = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribución de Retornos Anuales",
    x = "Retorno Anual (%)",
    y = "Frecuencia"
  )

# Histograma 2: Distribución de volatilidades
h_volatilidad <- ggplot(resultados_sim_plot, aes(x = volatilidad_pct)) +
  geom_histogram(bins = 40, fill = viridis(1, option = "inferno"),
                 color = "white", alpha = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribución de Volatilidades Anuales",
    x = "Volatilidad Anual (%)",
    y = "Frecuencia"
  )

# Histograma 3: Distribución de Sharpe ratios
h_sharpe <- ggplot(resultados_sim, aes(x = sharpe_ratio)) +
  geom_histogram(bins = 40, fill = viridis(1, option = "mako"),
                 color = "white", alpha = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribución de Sharpe Ratios",
    x = "Sharpe Ratio",
    y = "Frecuencia"
  )

# Combinar tres histogramas en un gráfico
combined_plot <- (h_retorno / h_volatilidad / h_sharpe) +
  plot_annotation(
    title = "Distribución de 10,000 Escenarios de Portafolios Simulados",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  )

print(combined_plot)

# Guardar como PNG
ggsave(
  "distribuciones_escenarios.png",
  combined_plot,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "white"
)

# SECCIÓN 10: GRÁFICO COMPARATIVO ============================================
# Preparar datos para visualización
mejor_sim_plot <- data.frame(
  retorno_pct = mejor_sim$retorno_anual * 100,
  volatilidad_pct = mejor_sim$volatilidad_anual * 100,
  sharpe_ratio = mejor_sim$sharpe_ratio
)

frontera_markowitz_overlay <- as.data.frame(frontera_markowitz) |>
  mutate(
    retorno_pct = retorno * 100,
    volatilidad_pct = volatilidad * 100
  )

portafolio_tangente_overlay <- data.frame(
  retorno_pct = portafolio_tangente$retorno * 100,
  volatilidad_pct = portafolio_tangente$volatilidad * 100,
  sharpe = portafolio_tangente$sharpe
)

# Gráfico combinado: simulaciones + frontera exacta
ggplot() +
  geom_point(data = resultados_sim_plot,
             aes(x = volatilidad_pct, y = retorno_pct, color = sharpe_ratio),
             alpha = 0.3, size = 1.5) +
  geom_line(data = frontera_markowitz_overlay,
            aes(x = volatilidad_pct, y = retorno_pct),
            color = "darkred", size = 1.5, linetype = "solid", inherit.aes = FALSE) +
  geom_point(data = mejor_sim_plot,
             aes(x = volatilidad_pct, y = retorno_pct),
             color = "red", size = 5, shape = 21, stroke = 2, inherit.aes = FALSE) +
  geom_point(data = portafolio_tangente_overlay,
             aes(x = volatilidad_pct, y = retorno_pct),
             color = "gold", size = 5, shape = 17, inherit.aes = FALSE) +
  scale_color_viridis_c(direction = 1, name = "Sharpe (Sim)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "italic"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Frontera Eficiente: Simulación vs Markowitz",
    subtitle = "Rojo = Mejor Simulado | Oro = Tangente | Línea Roja = Frontera Exacta",
    x = "Volatilidad Anual (%)",
    y = "Retorno Anual Esperado (%)"
  )

# SECCIÓN 11: TABLA COMPARATIVA ==============================================
# Calcular brechas entre simulación y Markowitz
brecha_retorno <- abs(mejor_sim$retorno_anual - portafolio_tangente$retorno)
brecha_vol <- abs(mejor_sim$volatilidad_anual - portafolio_tangente$volatilidad)
brecha_sharpe <- abs(mejor_sim$sharpe_ratio - portafolio_tangente$sharpe)

# Compilar tabla comparativa
comparacion <- tibble(
  metrica = c("Retorno Anual", "Volatilidad Anual", "Sharpe Ratio"),
  simulado = c(
    round(mejor_sim$retorno_anual * 100, 3),
    round(mejor_sim$volatilidad_anual * 100, 3),
    round(mejor_sim$sharpe_ratio, 4)
  ),
  markowitz = c(
    round(portafolio_tangente$retorno * 100, 3),
    round(portafolio_tangente$volatilidad * 100, 3),
    round(portafolio_tangente$sharpe, 4)
  ),
  brecha = c(
    round(brecha_retorno * 100, 3),
    round(brecha_vol * 100, 3),
    round(brecha_sharpe, 4)
  ),
  error_pct = c(
    round(brecha_retorno / abs(portafolio_tangente$retorno) * 100, 2),
    round(brecha_vol / portafolio_tangente$volatilidad * 100, 2),
    round(brecha_sharpe / portafolio_tangente$sharpe * 100, 2)
  )
)

cat("\n=== BRECHA DE OPTIMIZACIÓN ===\n")
cat("Simulación (10,000) vs Markowitz (Exacta)\n\n")
print(comparacion)

# SECCIÓN 12: COMPOSICIÓN DE PESOS ===========================================
# Tabla de pesos: comparar estrategias
pesos_comparacion <- tibble(
  activo = colnames(retornos_matrix),
  markowitz = portafolio_tangente$pesos,
  simulado = as.numeric(mejor_pesos_sim),  # Convertir vector a numérico
  diferencia = abs(portafolio_tangente$pesos - as.numeric(mejor_pesos_sim))
) |>
  mutate(
    markowitz_pct = round(markowitz * 100, 2),
    simulado_pct = round(simulado * 100, 2),
    diferencia_pct = round(diferencia * 100, 2)
  ) |>
  select(activo, markowitz_pct, simulado_pct, diferencia_pct)

cat("\n=== COMPOSICIÓN DE PESOS ===\n\n")
print(pesos_comparacion)

# Conclusiones
cat("\n✓ Markowitz: Concentra en activos específicos\n")
cat("✓ Simulado: Diversificación más equilibrada\n")
cat("✓ Error Sharpe: ", round(brecha_sharpe / portafolio_tangente$sharpe * 100, 2),
    "% respecto a Markowitz\n")

# FIN DEL ANÁLISIS ===========================================================
cat("\n✓ Análisis completado exitosamente\n")
