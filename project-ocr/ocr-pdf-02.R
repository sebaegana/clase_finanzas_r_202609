library(pdftools)
library(stringr)
library(dplyr)

# ============================================================================
# Paso 1: Extraer tabla del PDF
# ============================================================================

sector_profile <- pdf_text("usbpstatsfy2017sectorprofile.pdf")
sector_profile <- strsplit(sector_profile, "\n")
sector_profile <- sector_profile[[1]]
sector_profile <- trimws(sector_profile)

# Filtrar para tomar solo de Miami a Nationwide Total
sector_profile <- sector_profile[grep("Miami", sector_profile):
                                   grep("Nationwide Total", sector_profile)]

# Dividir por espacios dobles y convertir a dataframe
sector_profile <- str_split_fixed(sector_profile, " {2,}", 10)
sector_profile <- data.frame(sector_profile)
names(sector_profile) <- c("sector",
                           "agent_staffing",
                           "apprehensions",
                           "other_than_mexican_apprehensions",
                           "marijuana_pounds",
                           "cocaine_pounds",
                           "accepted_prosecutions",
                           "assaults",
                           "rescues",
                           "deaths")

# ============================================================================
# Paso 2: Filtrar filas vacías y limpiar nombres de sectores
# ============================================================================

# Filtrar filas donde el sector esté vacío (evita filas de encabezado duplicado)
sector_profile <- sector_profile %>%
  filter(sector != "")

sector_profile <- sector_profile %>%
  mutate(
    # Eliminar asteriscos finales
    sector = str_remove_all(sector, "\\*+"),
    # Eliminar notas entre paréntesis (Ej: "Big Bend (formerly Marfa)" -> "Big Bend")
    sector = str_remove(sector, "\\s*\\(.*\\)"),
    # Limpiar espacios
    sector = str_squish(sector)
  )

# ============================================================================
# Paso 3: Marcar subtotales y asignar región
# ============================================================================

sector_profile <- sector_profile %>%
  mutate(
    # Marcar si es subtotal
    tipo = ifelse(str_detect(sector, "Total"), "subtotal", "sector"),
    # Asignar región basado en el orden y nombres
    region = case_when(
      sector %in% c("Miami", "New Orleans", "Ramey") ~ "Coastal",
      sector %in% c("Blaine", "Buffalo", "Detroit", "Grand Forks", "Havre", "Houlton", "Spokane", "Swanton") ~ "Northern",
      sector %in% c("Big Bend", "Del Rio", "El Centro", "El Paso", "Laredo", "Rio Grande Valley", "San Diego", "Tucson", "Yuma") ~ "Southwest",
      str_detect(sector, "Coastal") ~ "Coastal",
      str_detect(sector, "Northern") ~ "Northern",
      str_detect(sector, "Southwest") ~ "Southwest",
      str_detect(sector, "Nationwide") ~ "Nationwide",
      TRUE ~ NA_character_
    )
  )

# ============================================================================
# Paso 4: Convertir columnas numéricas
# ============================================================================

sector_profile <- sector_profile %>%
  mutate(
    across(agent_staffing:deaths, ~{
      # Eliminar comas y asteriscos
      x <- str_remove_all(., "[,*]")
      # Convertir N/A a NA
      x <- ifelse(x == "N/A" | x == "NA", NA_character_, x)
      # Convertir a numerico
      as.numeric(x)
    })
  )

# ============================================================================
# Paso 5: Validaciones
# ============================================================================

cat("=== VALIDACIONES ===\n")
cat("Total de filas:", nrow(sector_profile), "\n")
cat("Filas de sectores:", sum(sector_profile$tipo == "sector"), "\n")
cat("Filas de subtotales:", sum(sector_profile$tipo == "subtotal"), "\n\n")

cat("Distribución por región:\n")
print(table(sector_profile$region, sector_profile$tipo))
cat("\n")

# Validar que no hay NA fuera de rescues/deaths en sectores (no subtotales)
na_in_sectors <- sector_profile %>%
  filter(tipo == "sector") %>%
  select(agent_staffing:assaults) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

cat("NA en columnas numéricas de SECTORES (excepto rescues/deaths):\n")
print(na_in_sectors)
cat("\nNota: Las columnas rescues/deaths pueden tener NA según el PDF\n\n")

# Validación de consistencia: verificar que subtotales cuadren
cat("=== VALIDACIÓN DE SUBTOTALES ===\n")
for (region in c("Coastal", "Northern", "Southwest")) {
  subtotal_row <- sector_profile %>%
    filter(tipo == "subtotal" & region == region)

  sectores_suma <- sector_profile %>%
    filter(tipo == "sector" & region == region) %>%
    summarise(suma = sum(apprehensions, na.rm = TRUE))

  if (nrow(subtotal_row) > 0) {
    cat("\n", region, ":\n")
    cat("  Subtotal reportado - Apprehensions:", subtotal_row$apprehensions[1], "\n")
    cat("  Suma de sectores  - Apprehensions:", sectores_suma$suma[1], "\n")
    cat("  ✓ Cuadra:", subtotal_row$apprehensions[1] == sectores_suma$suma[1], "\n")
  }
}

# Validar estructuras básicas
cat("Estructura del dataframe:\n")
str(sector_profile)
cat("\n")

# ============================================================================
# Paso 6: Separar en dos tablas
# ============================================================================

sector_detalle <- sector_profile %>%
  filter(tipo == "sector") %>%
  select(-tipo)

sector_totales <- sector_profile %>%
  filter(tipo == "subtotal") %>%
  select(-tipo)

cat("\nDetalle de sectores:\n")
print(head(sector_detalle))
cat("\nSubtotales:\n")
print(sector_totales)

# ============================================================================
# Paso 7: Exportar
# ============================================================================

# Crear directorio data si no existe
if (!dir.exists("data")) {
  dir.create("data")
}

# Exportar a CSV
write.csv(sector_profile,
          "data/usbp_sector_profile_fy2017.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

# Exportar a RDS
saveRDS(sector_profile, "data/usbp_sector_profile_fy2017.rds")

cat("\n✓ Tabla completa exportada a: data/usbp_sector_profile_fy2017.csv\n")
cat("✓ Tabla completa exportada a: data/usbp_sector_profile_fy2017.rds\n")
