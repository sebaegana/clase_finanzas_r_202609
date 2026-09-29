library(readr)
library(dplyr)
library(quadprog)

portafolio_retornos <- read_csv("data/portafolio_retornos.csv")

retornos_matrix <- portafolio_retornos |>
  select(-fecha) |>
  as.matrix()

retornos_esperados <- colMeans(retornos_matrix)
cov_matrix <- cov(retornos_matrix)
tasa_libre_riesgo <- 0.02 / 12

n_activos <- ncol(retornos_matrix)
retornos_objetivo <- seq(0.08, 0.20, length.out = 50) / 12  # mensuales

frontera_markowitz <- list(
  retorno = numeric(length(retornos_objetivo)),
  volatilidad = numeric(length(retornos_objetivo)),
  sharpe = numeric(length(retornos_objetivo)),
  pesos = matrix(NA, nrow = length(retornos_objetivo), ncol = n_activos)
)

for (i in seq_along(retornos_objetivo)) {
  r_obj <- retornos_objetivo[i]

  # Formulación quadprog
  H <- 2 * cov_matrix
  f <- rep(0, n_activos)

  # Restricciones: sum(w)=1, w'*r=r_obj, w>=0
  A <- cbind(
    rep(1, n_activos),          # suma = 1
    retornos_esperados,          # retorno objetivo
    diag(n_activos)              # w >= 0
  )

  b0 <- c(1, r_obj, rep(0, n_activos))

  tryCatch({
    resultado <- solve.QP(H, f, A, b0, meq = 2)
    w_opt <- resultado$solution
    vol_opt <- sqrt(t(w_opt) %*% cov_matrix %*% w_opt) * sqrt(12)

    frontera_markowitz$retorno[i] <- r_obj * 12
    frontera_markowitz$volatilidad[i] <- vol_opt
    frontera_markowitz$pesos[i, ] <- w_opt
  }, error = function(e) {
    warning(paste("solve.QP falló en iteración", i, ":", conditionMessage(e)))
  })
}

# Calcular Sharpe con protección
frontera_markowitz$sharpe <- ifelse(
  frontera_markowitz$volatilidad > 1e-10,
  (frontera_markowitz$retorno - 0.02) / frontera_markowitz$volatilidad,
  NA
)

# Identificar portafolio tangente (ignorar NA)
idx_tangente <- which.max(frontera_markowitz$sharpe)
portafolio_tangente <- list(
  retorno = frontera_markowitz$retorno[idx_tangente],
  volatilidad = frontera_markowitz$volatilidad[idx_tangente],
  sharpe = frontera_markowitz$sharpe[idx_tangente],
  pesos = frontera_markowitz$pesos[idx_tangente, ]
)

pesos_limpiados <- ifelse(abs(portafolio_tangente$pesos) < 1e-10, 0,
                          round(portafolio_tangente$pesos, 4))

portafolio_tangente_df <- tibble(
  retorno = portafolio_tangente$retorno,
  volatilidad = portafolio_tangente$volatilidad,
  sharpe = portafolio_tangente$sharpe,
  pesos = pesos_limpiados
)

print(portafolio_tangente_df)

# Preparar datos para visualización
library(ggplot2)

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

# Gráfico: Frontera Eficiente de Markowitz
ggplot(frontera_markowitz_df, aes(x = volatilidad_pct, y = retorno_pct)) +
  geom_line(linewidth = 1.2, color = "darkred", alpha = 0.8) +
  geom_point(aes(color = sharpe), size = 3, alpha = 0.6) +
  geom_point(data = portafolio_tangente_plot,
             aes(x = volatilidad, y = retorno),
             color = "gold", size = 5, shape = 17, inherit.aes = FALSE) +
  geom_hline(yintercept = 2, linetype = "dashed",
             color = "gray50", alpha = 0.7, linewidth = 0.8) +
  scale_color_gradient(name = "Ratio Sharpe", low = "lightblue", high = "navy") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Frontera Eficiente de Markowitz",
    subtitle = "Triángulo Dorado = Portafolio Tangente (Mayor Ratio de Sharpe)",
    x = "Volatilidad Anual (%)",
    y = "Retorno Anual Esperado (%)"
  ) +
  annotate("text", x = max(frontera_markowitz_df$volatilidad_pct) * 0.7,
           y = 2.3, label = "Tasa Libre de Riesgo (2%)",
           size = 3, color = "gray50")




