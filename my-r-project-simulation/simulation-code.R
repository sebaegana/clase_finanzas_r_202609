library(ggplot2)
library(dplyr)
library(triangle)
set.seed(1234567)

reps <- 10000

utilidad <- matrix(NA, nrow = reps, ncol = 1)
for (i in 1:reps) {
  x <- sample(
    c("A", "B", "C"),
    1,
    replace = TRUE,
    prob = c(1 / 4, 1 / 2, 1 / 4)
  )
  if (x == "A") {
    precio <- 12
    cantidad <- 110
  } else if (x == "B") {
    precio <- 13
    cantidad <- 100
  } else {
    precio <- 16
    cantidad <- 80
  }
  costo_variable_unitario <- rtriangle(1, 9, 13, 11)
  costo_fijo <- 150
  utilidad[i] <- (precio - costo_variable_unitario) * cantidad - costo_fijo
}

utilidad <- data.frame(utilidad) # tibble

ggplot(utilidad) +
  geom_histogram(
    aes(x = utilidad, y = after_stat(density)),
    col = "black",
    bins = 35
  ) +
  labs(x = "Utilidad", y = "Densidad") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line()
  )


paste0("Prob(utilidad<0) = ", round(mean(utilidad$utilidad<0),2))
paste0("Valor esperado utilidad = ", round(mean(utilidad$utilidad),2))

## Comandos renv

renv::init()
renv::status()
renv::snapshot()

quarto::quarto_render("simulation-document.qmd")
