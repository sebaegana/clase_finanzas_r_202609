## ============================================================================
## PROYECTO: ANÁLISIS DE SHOCKS EN PORTAFOLIOS
## Extensión del análisis: estrés y escenarios adversos
## ============================================================================

# SECCIÓN 1: CARGAR LIBRERÍAS Y DATOS =======================================
library(readr)      # Lectura de archivos CSV
library(tidyverse)  # Suite de manipulación de datos
library(ggplot2)    # Visualizaciones
library(viridis)    # Paletas de colores
library(patchwork)  # Combinación de gráficos

# Cargar datos (mismo que análisis anterior)
portafolio_retornos <- read_csv("data/portafolio_retornos.csv")

# SECCIÓN 2: PREPARAR DATOS BASE =============================================
retornos_matrix <- portafolio_retornos |>
  select(-fecha) |>
  as.matrix()

retornos_esperados <- colMeans(retornos_matrix)
cov_matrix <- cov(retornos_matrix)

# Usar los portafolios del análisis anterior
set.seed(42)
n_sims <- 10000
n_activos <- ncol(retornos_matrix)

# Generar pesos aleatorios
pesos_matrix <- matrix(NA, nrow = n_sims, ncol = n_activos)
for (i in 1:n_sims) {
  w <- runif(n_activos)
  w <- w / sum(w)
  pesos_matrix[i, ] <- w
}

# Calcular métricas base (sin shock)
retornos_base <- pesos_matrix %*% retornos_esperados * 12
volatilidades_base <- sqrt(diag(pesos_matrix %*% cov_matrix %*% t(pesos_matrix))) * sqrt(12)
sharpe_base <- (retornos_base - 0.02) / volatilidades_base

resultados_base <- tibble(
  retorno = as.numeric(retornos_base),
  volatilidad = as.numeric(volatilidades_base),
  sharpe = as.numeric(sharpe_base)
)

cat("\n=== PORTAFOLIOS BASE (SIN SHOCK) ===\n")
cat("Media Sharpe:     ", round(mean(resultados_base$sharpe, na.rm = TRUE), 4), "\n")
cat("Sharpe Máximo:    ", round(max(resultados_base$sharpe, na.rm = TRUE), 4), "\n")
cat("Sharpe Mínimo:    ", round(min(resultados_base$sharpe, na.rm = TRUE), 4), "\n\n")

# SECCIÓN 3: DEFINIR ESCENARIOS DE SHOCKS ===================================
# Scenarios 1: Shock uniforme (reduce retornos en porcentaje fijo)
shock_porcentajes <- c(-0.05, -0.10, -0.15, -0.20)  # -5%, -10%, -15%, -20%

# Scenarios 2: Shock diferenciado (acciones vs bonos)
# SPY, EFA, IJS, EEM (acciones) afectadas más; AGG (bonos) menos
shock_diferenciado <- c(
  SPY = -0.20,   # Acciones: -20%
  EFA = -0.20,
  IJS = -0.20,
  EEM = -0.20,
  AGG = -0.05    # Bonos: -5%
)

# SECCIÓN 4: APLICAR SHOCKS UNIFORMES =======================================
cat("\n=== ANÁLISIS DE SHOCKS UNIFORMES ===\n\n")

resultados_shocks_uniformes <- tibble()

for (shock in shock_porcentajes) {
  # Aplicar shock a retornos esperados
  retornos_esperados_shock <- retornos_esperados * (1 + shock)

  # Recalcular retornos de portafolios
  retornos_shock <- pesos_matrix %*% retornos_esperados_shock * 12
  volatilidades_shock <- sqrt(diag(pesos_matrix %*% cov_matrix %*% t(pesos_matrix))) * sqrt(12)
  sharpe_shock <- (retornos_shock - 0.02) / volatilidades_shock

  # Compilar resultados
  temp <- tibble(
    escenario = paste0("Shock ", round(shock*100), "%"),
    shock_valor = shock,
    retorno_medio = mean(as.numeric(retornos_shock), na.rm = TRUE),
    retorno_min = min(as.numeric(retornos_shock), na.rm = TRUE),
    retorno_max = max(as.numeric(retornos_shock), na.rm = TRUE),
    sharpe_medio = mean(as.numeric(sharpe_shock), na.rm = TRUE),
    sharpe_min = min(as.numeric(sharpe_shock), na.rm = TRUE),
    sharpe_max = max(as.numeric(sharpe_shock), na.rm = TRUE),
    cambio_sharpe_pct = ((mean(as.numeric(sharpe_shock), na.rm = TRUE) -
                          mean(resultados_base$sharpe, na.rm = TRUE)) /
                         mean(resultados_base$sharpe, na.rm = TRUE) * 100)
  )

  resultados_shocks_uniformes <- bind_rows(resultados_shocks_uniformes, temp)
}

print(resultados_shocks_uniformes |>
  select(escenario, retorno_medio, sharpe_medio, cambio_sharpe_pct))

# SECCIÓN 5: APLICAR SHOCK DIFERENCIADO =====================================
cat("\n=== ANÁLISIS DE SHOCK DIFERENCIADO (ACCIONES -20%, BONOS -5%) ===\n\n")

# Crear vector de shocks diferenciados
retornos_esperados_shock_diff <- retornos_esperados
for (i in 1:length(retornos_esperados)) {
  nombre_activo <- names(retornos_esperados)[i]
  retornos_esperados_shock_diff[i] <- retornos_esperados[i] *
                                       (1 + shock_diferenciado[nombre_activo])
}

# Recalcular con shock diferenciado
retornos_shock_diff <- pesos_matrix %*% retornos_esperados_shock_diff * 12
volatilidades_shock_diff <- sqrt(diag(pesos_matrix %*% cov_matrix %*% t(pesos_matrix))) * sqrt(12)
sharpe_shock_diff <- (retornos_shock_diff - 0.02) / volatilidades_shock_diff

resultados_shock_diff <- tibble(
  retorno = as.numeric(retornos_shock_diff),
  volatilidad = as.numeric(volatilidades_shock_diff),
  sharpe = as.numeric(sharpe_shock_diff)
)

cat("Media Sharpe:     ", round(mean(resultados_shock_diff$sharpe, na.rm = TRUE), 4), "\n")
cat("Cambio vs Base:   ",
    round((mean(resultados_shock_diff$sharpe, na.rm = TRUE) -
           mean(resultados_base$sharpe, na.rm = TRUE)) /
          mean(resultados_base$sharpe, na.rm = TRUE) * 100, 2), "%\n\n")

# SECCIÓN 6: ANÁLISIS DE VOLATILIDAD AUMENTADA ==============================
cat("\n=== ANÁLISIS DE SHOCK DE VOLATILIDAD (+50%) ===\n\n")

# Shock de volatilidad: aumentar varianza en 50%
cov_matrix_shock_vol <- cov_matrix * 1.5

volatilidades_shock_vol <- sqrt(diag(pesos_matrix %*% cov_matrix_shock_vol %*% t(pesos_matrix))) * sqrt(12)
sharpe_shock_vol <- (retornos_base - 0.02) / volatilidades_shock_vol

resultados_shock_vol <- tibble(
  retorno = as.numeric(retornos_base),
  volatilidad = as.numeric(volatilidades_shock_vol),
  sharpe = as.numeric(sharpe_shock_vol)
)

cat("Volatilidad media base:  ", round(mean(resultados_base$volatilidad), 3), "%\n")
cat("Volatilidad media shock: ", round(mean(resultados_shock_vol$volatilidad), 3), "%\n")
cat("Media Sharpe:            ", round(mean(resultados_shock_vol$sharpe, na.rm = TRUE), 4), "\n")
cat("Cambio vs Base:          ",
    round((mean(resultados_shock_vol$sharpe, na.rm = TRUE) -
           mean(resultados_base$sharpe, na.rm = TRUE)) /
          mean(resultados_base$sharpe, na.rm = TRUE) * 100, 2), "%\n\n")

# SECCIÓN 7: TABLA COMPARATIVA DE ESCENARIOS =================================
cat("\n=== TABLA RESUMEN: IMPACTO DE DIFERENTES SHOCKS ===\n\n")

tabla_comparativa <- tibble(
  Escenario = c(
    "Base (sin shock)",
    "Shock uniforme -5%",
    "Shock uniforme -10%",
    "Shock uniforme -15%",
    "Shock uniforme -20%",
    "Shock diferenciado",
    "Shock volatilidad +50%"
  ),
  `Retorno Medio (%)` = c(
    round(mean(resultados_base$retorno)*100, 2),
    round(resultados_shocks_uniformes$retorno_medio[1]*100, 2),
    round(resultados_shocks_uniformes$retorno_medio[2]*100, 2),
    round(resultados_shocks_uniformes$retorno_medio[3]*100, 2),
    round(resultados_shocks_uniformes$retorno_medio[4]*100, 2),
    round(mean(resultados_shock_diff$retorno)*100, 2),
    round(mean(resultados_shock_vol$retorno)*100, 2)
  ),
  `Volatilidad Media (%)` = c(
    round(mean(resultados_base$volatilidad)*100, 2),
    round(mean(volatilidades_base)*100, 2),
    round(mean(volatilidades_base)*100, 2),
    round(mean(volatilidades_base)*100, 2),
    round(mean(volatilidades_base)*100, 2),
    round(mean(resultados_shock_diff$volatilidad)*100, 2),
    round(mean(resultados_shock_vol$volatilidad)*100, 2)
  ),
  `Sharpe Medio` = c(
    round(mean(resultados_base$sharpe, na.rm = TRUE), 4),
    round(resultados_shocks_uniformes$sharpe_medio[1], 4),
    round(resultados_shocks_uniformes$sharpe_medio[2], 4),
    round(resultados_shocks_uniformes$sharpe_medio[3], 4),
    round(resultados_shocks_uniformes$sharpe_medio[4], 4),
    round(mean(resultados_shock_diff$sharpe, na.rm = TRUE), 4),
    round(mean(resultados_shock_vol$sharpe, na.rm = TRUE), 4)
  ),
  `Cambio Sharpe (%)` = c(
    0,
    round(resultados_shocks_uniformes$cambio_sharpe_pct[1], 2),
    round(resultados_shocks_uniformes$cambio_sharpe_pct[2], 2),
    round(resultados_shocks_uniformes$cambio_sharpe_pct[3], 2),
    round(resultados_shocks_uniformes$cambio_sharpe_pct[4], 2),
    round((mean(resultados_shock_diff$sharpe, na.rm = TRUE) -
           mean(resultados_base$sharpe, na.rm = TRUE)) /
          mean(resultados_base$sharpe, na.rm = TRUE) * 100, 2),
    round((mean(resultados_shock_vol$sharpe, na.rm = TRUE) -
           mean(resultados_base$sharpe, na.rm = TRUE)) /
          mean(resultados_base$sharpe, na.rm = TRUE) * 100, 2)
  )
)

print(tabla_comparativa)

# SECCIÓN 8: GRÁFICO 1 - DISTRIBUCIÓN DE SHARPE RATIOS =======================
# Preparar datos para visualización
df_comparacion <- tibble(
  Escenario = c(
    rep("Base", n_sims),
    rep("Shock -10%", n_sims),
    rep("Shock -20%", n_sims),
    rep("Shock Diferenciado", n_sims),
    rep("Shock Volatilidad", n_sims)
  ),
  Sharpe = c(
    resultados_base$sharpe,
    resultados_shocks_uniformes$sharpe_medio[2] +
      (resultados_base$sharpe - mean(resultados_base$sharpe, na.rm = TRUE)),
    resultados_shocks_uniformes$sharpe_medio[4] +
      (resultados_base$sharpe - mean(resultados_base$sharpe, na.rm = TRUE)),
    resultados_shock_diff$sharpe,
    resultados_shock_vol$sharpe
  )
) |>
  filter(!is.na(Sharpe)) |>
  mutate(Escenario = factor(Escenario,
    levels = c("Base", "Shock -10%", "Shock -20%", "Shock Diferenciado", "Shock Volatilidad")))

p_distribuciones <- ggplot(df_comparacion, aes(x = Sharpe, fill = Escenario)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "Distribución de Sharpe Ratios: Comparación de Escenarios",
    x = "Sharpe Ratio",
    y = "Densidad",
    fill = "Escenario"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "bottom"
  )

# SECCIÓN 9: GRÁFICO 2 - CURVA DE SENSIBILIDAD ==============================
df_sensibilidad <- tibble(
  Shock = c(0, -0.05, -0.10, -0.15, -0.20),
  `Sharpe Medio` = c(
    mean(resultados_base$sharpe, na.rm = TRUE),
    resultados_shocks_uniformes$sharpe_medio[1],
    resultados_shocks_uniformes$sharpe_medio[2],
    resultados_shocks_uniformes$sharpe_medio[3],
    resultados_shocks_uniformes$sharpe_medio[4]
  ),
  `Retorno Medio (%)` = c(
    mean(resultados_base$retorno)*100,
    resultados_shocks_uniformes$retorno_medio[1]*100,
    resultados_shocks_uniformes$retorno_medio[2]*100,
    resultados_shocks_uniformes$retorno_medio[3]*100,
    resultados_shocks_uniformes$retorno_medio[4]*100
  )
)

p_sensibilidad <- ggplot(df_sensibilidad, aes(x = Shock * 100)) +
  geom_line(aes(y = `Sharpe Medio`), color = "darkblue", size = 1.2, linetype = "solid") +
  geom_point(aes(y = `Sharpe Medio`), color = "darkblue", size = 3) +
  scale_x_reverse() +
  theme_minimal() +
  labs(
    title = "Curva de Sensibilidad: Impacto del Shock en Sharpe Ratio",
    x = "Magnitud del Shock (%)",
    y = "Sharpe Ratio Medio"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90")
  )

# SECCIÓN 10: GRÁFICO 3 - RETORNO vs VOLATILIDAD CON SHOCKS =================
df_scatter_shocks <- tibble(
  Escenario = c(
    rep("Base", n_sims),
    rep("Shock -20%", n_sims),
    rep("Shock Diferenciado", n_sims),
    rep("Shock Volatilidad", n_sims)
  ),
  Retorno = c(
    resultados_base$retorno * 100,
    pesos_matrix %*% (retornos_esperados * 0.8) * 12 * 100,
    resultados_shock_diff$retorno * 100,
    resultados_shock_vol$retorno * 100
  ),
  Volatilidad = c(
    resultados_base$volatilidad * 100,
    resultados_base$volatilidad * 100,
    resultados_shock_diff$volatilidad * 100,
    resultados_shock_vol$volatilidad * 100
  )
) |>
  filter(!is.na(Retorno) & !is.na(Volatilidad)) |>
  mutate(Escenario = factor(Escenario,
    levels = c("Base", "Shock -20%", "Shock Diferenciado", "Shock Volatilidad")))

p_scatter <- ggplot(df_scatter_shocks, aes(x = Volatilidad, y = Retorno, color = Escenario)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_point(data = df_scatter_shocks |>
             group_by(Escenario) |>
             summarise(Retorno = mean(Retorno, na.rm = TRUE),
                       Volatilidad = mean(Volatilidad, na.rm = TRUE)),
             size = 5, shape = 21, fill = "white", stroke = 2) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "Retorno vs Volatilidad: Impacto de Shocks",
    x = "Volatilidad Anual (%)",
    y = "Retorno Anual Esperado (%)",
    color = "Escenario"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "right"
  )

# SECCIÓN 11: COMBINAR Y VISUALIZAR GRÁFICOS ================================
combined_shocks <- (p_distribuciones / p_sensibilidad / p_scatter) +
  plot_layout(heights = c(1, 1, 1.2)) +
  plot_annotation(
    title = "Análisis de Shocks en Portafolios: Stress Testing",
    subtitle = "Impacto de diferentes escenarios adversos (10,000 portafolios simulados)",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  )

print(combined_shocks)

# Guardar como PNG
ggsave(
  "analisis_shocks_portafolios.png",
  combined_shocks,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# SECCIÓN 12: RESUMEN Y CONCLUSIONES ==========================================
cat("\n=== CONCLUSIONES DEL ANÁLISIS DE SHOCKS ===\n\n")

shock_mas_severo <- resultados_shocks_uniformes |>
  slice_min(sharpe_medio) |>
  pull(escenario)

cambio_mas_severo <- resultados_shocks_uniformes |>
  slice_min(cambio_sharpe_pct) |>
  pull(cambio_sharpe_pct)

cat("1. Shock más severo:             ", shock_mas_severo, "\n")
cat("   Reducción Sharpe:             ", round(cambio_mas_severo, 2), "%\n\n")

cat("2. Impacto del shock diferenciado:\n")
cat("   - Afecta más a portafolios con mayor peso en acciones\n")
cat("   - Portafolios con bonos (AGG) sufren menos\n")
cat("   - Cambio Sharpe:              ",
    round((mean(resultados_shock_diff$sharpe, na.rm = TRUE) -
           mean(resultados_base$sharpe, na.rm = TRUE)) /
          mean(resultados_base$sharpe, na.rm = TRUE) * 100, 2), "%\n\n")

cat("3. Impacto del aumento de volatilidad:\n")
cat("   - Reduce Sharpe ratios significativamente\n")
cat("   - Afecta igualmente a todos los portafolios\n")
cat("   - Cambio Sharpe:              ",
    round((mean(resultados_shock_vol$sharpe, na.rm = TRUE) -
           mean(resultados_base$sharpe, na.rm = TRUE)) /
          mean(resultados_base$sharpe, na.rm = TRUE) * 100, 2), "%\n\n")

cat("4. Recomendaciones:\n")
cat("   ✓ Diversificar hacia activos defensivos (bonos, AGG)\n")
cat("   ✓ Mantener exposición moderada a acciones\n")
cat("   ✓ Monitorear cambios en correlaciones durante crisis\n")
cat("   ✓ Realizar stress testing regularmente\n\n")

cat("✓ Análisis de shocks completado exitosamente\n")
