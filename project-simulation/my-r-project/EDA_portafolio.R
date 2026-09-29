# =============================================================================
# ANÁLISIS EXPLORATORIO DE DATOS (EDA) - PORTAFOLIO DE 5 ACTIVOS
# =============================================================================
# Datos: portafolio_retornos.csv
# Activos: SPY, EFA, IJS, EEM, AGG
# Frecuencia: Retornos mensuales
# =============================================================================

# Librerías necesarias
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(knitr)

# =============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

# Cargar datos
datos <- read.csv("data/portafolio_retornos.csv", stringsAsFactors = FALSE)

# Convertir fecha a formato Date
datos$fecha <- as.Date(datos$fecha)

# Activos
activos <- c("SPY", "EFA", "IJS", "EEM", "AGG")

# Vista preliminar
head(datos)
dim(datos)
str(datos)

# =============================================================================
# 2. ESTADÍSTICA DESCRIPTIVA
# =============================================================================

cat("\n=== ESTADÍSTICA DESCRIPTIVA ===\n")

# Crear tabla de estadísticas descriptivas
stats_desc <- data.frame(
  Activo = activos,
  Media = apply(datos[, activos], 2, mean),
  SD = apply(datos[, activos], 2, sd),
  Min = apply(datos[, activos], 2, min),
  Q1 = apply(datos[, activos], 2, quantile, 0.25),
  Mediana = apply(datos[, activos], 2, median),
  Q3 = apply(datos[, activos], 2, quantile, 0.75),
  Max = apply(datos[, activos], 2, max),
  NA_Count = colSums(is.na(datos[, activos]))
)

print(stats_desc, digits = 4)

# Guardar en formato tabla
write.csv(stats_desc, "output/estadistica_descriptiva.csv", row.names = FALSE)

# =============================================================================
# 3. HISTOGRAMAS DE RETORNOS POR ACTIVO
# =============================================================================

cat("\n=== GENERANDO HISTOGRAMAS ===\n")

# Preparar datos para ggplot
datos_long <- datos %>%
  pivot_longer(cols = all_of(activos),
               names_to = "Activo",
               values_to = "Retorno")

# Crear histogramas por activo
p_histogramas <- ggplot(datos_long, aes(x = Retorno, fill = Activo)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~Activo, scales = "free") +
  labs(title = "Distribución de Retornos Mensuales por Activo",
       x = "Retorno",
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_histogramas)
ggsave("output/histogramas_retornos.png", p_histogramas, width = 12, height = 8)

# =============================================================================
# 4. SERIES TEMPORALES DE RETORNOS
# =============================================================================

cat("\n=== GENERANDO SERIES TEMPORALES ===\n")

# Gráfico de series temporales
p_series <- ggplot(datos_long, aes(x = fecha, y = Retorno, color = Activo)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~Activo, scales = "free_y") +
  labs(title = "Series Temporales de Retornos Mensuales",
       x = "Fecha",
       y = "Retorno") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_series)
ggsave("output/series_temporales.png", p_series, width = 12, height = 8)

# Serie temporal de todos los activos en un gráfico
p_series_combined <- ggplot(datos_long, aes(x = fecha, y = Retorno, color = Activo)) +
  geom_line(alpha = 0.7) +
  labs(title = "Series Temporales de Retornos - Todos los Activos",
       x = "Fecha",
       y = "Retorno",
       color = "Activo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_series_combined)
ggsave("output/series_temporales_combinadas.png", p_series_combined, width = 12, height = 6)

# =============================================================================
# 5. MATRIZ DE CORRELACIÓN
# =============================================================================

cat("\n=== MATRIZ DE CORRELACIÓN ===\n")

# Calcular matriz de correlación
matriz_corr <- cor(datos[, activos])
print(matriz_corr)

# Guardar en CSV
write.csv(matriz_corr, "output/matriz_correlacion.csv")

# Visualizar matriz de correlación
png("output/matriz_correlacion.png", width = 800, height = 700)
corrplot(matriz_corr,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         diag = TRUE,
         tl.col = "black",
         tl.srt = 45,
         title = "Matriz de Correlación - Portafolio")
dev.off()

# =============================================================================
# 6. MATRIZ DE COVARIANZA
# =============================================================================

cat("\n=== MATRIZ DE COVARIANZA ===\n")

# Calcular matriz de covarianza
matriz_cov <- cov(datos[, activos])
print(matriz_cov)

# Guardar en CSV
write.csv(matriz_cov, "output/matriz_covarianza.csv")

# Visualizar matriz de covarianza con escala de colores
png("output/matriz_covarianza.png", width = 800, height = 700)
corrplot(matriz_cov,
         method = "color",
         type = "upper",
         is.corr = FALSE,
         addCoef.col = "black",
         diag = TRUE,
         tl.col = "black",
         tl.srt = 45,
         title = "Matriz de Covarianza - Portafolio")
dev.off()

# =============================================================================
# 7. ANÁLISIS ADICIONAL: RETORNOS ACUMULADOS
# =============================================================================

cat("\n=== RETORNOS ACUMULADOS ===\n")

# Calcular retornos acumulados
datos_acum <- datos %>%
  mutate(across(all_of(activos),
                list(acum = ~cumprod(1 + .))))

# Convertir a formato largo
datos_acum_long <- datos_acum %>%
  select(fecha, ends_with("_acum")) %>%
  pivot_longer(cols = -fecha,
               names_to = "Activo",
               values_to = "Retorno_Acumulado") %>%
  mutate(Activo = str_remove(Activo, "_acum"))

# Gráfico de retornos acumulados
p_acum <- ggplot(datos_acum_long, aes(x = fecha, y = Retorno_Acumulado, color = Activo)) +
  geom_line(size = 1) +
  labs(title = "Retornos Acumulados - Portafolio",
       x = "Fecha",
       y = "Retorno Acumulado (1 + r)",
       color = "Activo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_acum)
ggsave("output/retornos_acumulados.png", p_acum, width = 12, height = 6)

# =============================================================================
# 8. MATRIZ DE DISPERSIÓN (SCATTER PLOT PAIRS)
# =============================================================================

cat("\n=== MATRIZ DE DISPERSIÓN ===\n")

# Crear matriz de dispersión
png("output/matriz_dispersion.png", width = 1000, height = 1000)
pairs(datos[, activos],
      main = "Matriz de Dispersión - Retornos Mensuales",
      pch = 19,
      col = rgb(0, 0, 0, 0.5))
dev.off()

# =============================================================================
# 9. RESUMEN DE RESULTADOS
# =============================================================================

cat("\n=== RESUMEN DEL ANÁLISIS EDA ===\n")
cat("\nArchivos generados en carpeta 'output/':\n")
cat("1. estadistica_descriptiva.csv - Tabla de estadísticas\n")
cat("2. histogramas_retornos.png - Distribución de retornos\n")
cat("3. series_temporales.png - Series por activo\n")
cat("4. series_temporales_combinadas.png - Todas las series juntas\n")
cat("5. matriz_correlacion.csv - Tabla de correlaciones\n")
cat("6. matriz_correlacion.png - Visualización de correlaciones\n")
cat("7. matriz_covarianza.csv - Tabla de covarianzas\n")
cat("8. matriz_covarianza.png - Visualización de covarianzas\n")
cat("9. retornos_acumulados.png - Evolución acumulada\n")
cat("10. matriz_dispersion.png - Scatter plots entre activos\n")

cat("\nPeríodo de datos: ", as.character(min(datos$fecha)), " a ",
    as.character(max(datos$fecha)), "\n", sep = "")
cat("Número de observaciones: ", nrow(datos), "\n", sep = "")
cat("Activos analizados: ", paste(activos, collapse = ", "), "\n\n", sep = "")

# =============================================================================
# FIN DEL ANÁLISIS
# =============================================================================
