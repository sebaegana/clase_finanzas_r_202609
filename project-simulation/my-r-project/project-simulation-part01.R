## Carga de librerías

library(readr)
library(tidyverse)
library(skimr)
library(corrplot)
library(kableExtra)

## Carga de dataset

portafolio_retornos <- read_csv("data/portafolio_retornos.csv")

## Revisión preliminar dataset

head(portafolio_retornos)
str(portafolio_retornos)

## EDA

summary(portafolio_retornos |> select(SPY, EFA, IJS, EEM, AGG))

summary(portafolio_retornos |> select(-fecha))
skim(portafolio_retornos |> select(-fecha))

stats_por_activo <- portafolio_retornos |>
  select(-fecha) |>
  summarise(across(everything(),
    list(
      media = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      n = ~n()
    ),
    .names = "{.col}_{.fn}"
  ))

print(stats_por_activo)

## Gráficos

portaforlio_retornos_long <- portafolio_retornos |>
  select(-fecha) |>
  pivot_longer(everything(), names_to = "activo", values_to = "retorno")

portafolio_retornos |>
  select(-fecha) |>
  pivot_longer(everything(), names_to = "activo", values_to = "retorno") |>
  ggplot(aes(x = retorno, fill = activo)) +
  geom_histogram(alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~activo, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de Retornos Mensuales (2012-2021)",
       x = "Retorno", y = "Frecuencia")

portafolio_retornos |>
  mutate(fecha = as.Date(fecha)) |>
  pivot_longer(-fecha, names_to = "activo", values_to = "retorno") |>
  ggplot(aes(x = fecha, y = retorno, color = activo)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~activo, scales = "free_y") +
  theme_minimal() +
  labs(title = "Retornos Mensuales en el Tiempo",
       x = "Fecha", y = "Retorno")

## Correlación

retornos_matrix <- portafolio_retornos |>
  select(-fecha) |>
  as.matrix()

correlacion <- cor(retornos_matrix)
corrplot(correlacion, method = "circle", type = "upper",
         title = "Matriz de Correlaciones entre Activos",
         mar = c(0, 0, 2, 0))

covarianza <- cov(retornos_matrix)
print("Matriz de Covarianza:")
print(round(covarianza, 6))

correlacion_df <- as.data.frame(correlacion) |>
  rownames_to_column("activo1") |>
  pivot_longer(-activo1, names_to = "activo2", values_to = "corr") |>
  filter(activo1 < activo2) |>
  arrange(desc(abs(corr)))

kable(correlacion_df, digits = 3, caption = "Correlaciones entre Activos")