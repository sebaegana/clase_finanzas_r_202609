# 📊 Simulación de Portafolios: Markowitz vs Monte Carlo

Análisis comparativo de optimización de portafolios utilizando **Teoría Moderna de Portafolios (Markowitz)** vs **Simulación Monte Carlo** en R.

## 🎯 Objetivo

Comparar dos enfoques para encontrar portafolios óptimos:
1. **Optimización exacta (Markowitz):** Frontera eficiente mediante programación cuadrática
2. **Simulación estocástica:** 10,000 portafolios aleatorios generados mediante Monte Carlo

## 📂 Estructura del Repositorio

```
proyecto-simulation/
├── my-r-project/
│   ├── README.md                              # Este archivo
│   ├── project-simulation-part01-comentado.R  # Análisis principal (recomendado)
│   ├── project-simulation-part03.R            # Visualizaciones adicionales
│   ├── distribuciones_escenarios.png          # Gráfico de histogramas
│   ├── data/
│   │   └── portafolio_retornos.csv            # Datos de 5 activos (2012-2021)
│   └── output/
│       └── distribuciones_escenarios.png      # PNG generado por el análisis
```

## 📋 Descripción del Análisis

### Sección 1: Exploración de Datos (EDA)
- **Datos:** Retornos mensuales de 5 activos (SPY, EFA, IJS, EEM, AGG)
- **Período:** 2013-2021 (108 observaciones)
- **Análisis:** Estadísticas descriptivas, distribuciones y gráficos de series temporales

### Sección 2: Análisis de Correlación
- Matriz de correlaciones (Pearson)
- Matriz de covarianzas
- Visualización con `corrplot`
- Tabla ordenada de pares de activos

**Hallazgos clave:**
- Mayor correlación: EFA-SPY (0.857)
- Menor correlación: AGG-SPY (-0.004)
- AGG actúa como diversificador (correlaciones bajas)

### Sección 3: Optimización Markowitz (Frontera Eficiente)
- **Método:** Programación cuadrática con restricciones
  - Restricción 1: Suma de pesos = 1
  - Restricción 2: Retorno objetivo alcanzado
  - Restricción 3: Long-only (pesos ≥ 0)
- **Output:** 50 portafolios en la frontera eficiente
- **Portafolio tangente:** Máximo ratio de Sharpe

**Resultados Markowitz:**
```
Retorno:      9.224%
Volatilidad:  7.073%
Sharpe Ratio: 1.0214
Pesos: SPY (51.9%), AGG (48.1%), otros (0%)
```

### Sección 4: Simulación Monte Carlo (10,000 Portafolios)
- **Método:** Generación aleatoria de pesos normalizados
- **Escenarios:** 10,000 portafolios simulados
- **Cálculos:** Retorno, volatilidad y Sharpe ratio para cada portafolio
- **Identificación:** Mejor portafolio por Sharpe ratio

**Resultados Simulación:**
```
Retorno:      8.639%
Volatilidad:  7.033%
Sharpe Ratio: 0.9439
Pesos: SPY (45.0%), EFA (5.1%), IJS (0.9%), EEM (1.0%), AGG (48.0%)
```

### Sección 5: Análisis Comparativo

| Métrica | Simulado | Markowitz | Brecha | Error % |
|---------|----------|-----------|--------|---------|
| Retorno Anual | 8.64% | 9.22% | 0.586% | 6.35% |
| Volatilidad Anual | 7.03% | 7.07% | 0.040% | 0.56% |
| Sharpe Ratio | 0.944 | 1.021 | 0.078 | **7.59%** |

**Conclusión:** Con 10,000 simulaciones, la brecha de Sharpe es del 7.59%, muy razonable para un enfoque estocástico.

### Sección 6: Composición de Pesos

**Markowitz (Concentración):**
- SPY: 51.9%
- AGG: 48.1%
- Otros: 0.0%

**Simulación (Diversificación):**
- SPY: 45.0%
- AGG: 48.0%
- EFA: 5.1%
- IJS: 0.9%
- EEM: 1.0%

## 🛠️ Requisitos

### Librerías R necesarias:
```r
library(readr)      # Lectura de datos
library(tidyverse)  # Manipulación de datos (dplyr, tidyr, ggplot2)
library(skimr)      # Resumen estadístico rápido
library(corrplot)   # Visualización de correlaciones
library(kableExtra) # Tablas formateadas
library(quadprog)   # Optimización cuadrática
library(ggplot2)    # Gráficos
library(viridis)    # Paletas de colores
library(patchwork)  # Combinación de gráficos
```

### Instalación:
```r
# Instalar paquetes faltantes
install.packages(c("readr", "tidyverse", "skimr", "corrplot", 
                   "kableExtra", "quadprog", "viridis", "patchwork"))
```

## 🚀 Cómo usar

### Opción 1: Ejecutar el script principal
```bash
cd proyecto-simulation/my-r-project
Rscript project-simulation-part01-comentado.R
```

### Opción 2: Ejecutar en RStudio
1. Abrir `project-simulation-part01-comentado.R`
2. Presionar `Ctrl + A` (o `Cmd + A` en Mac)
3. Presionar `Ctrl + Enter` (o `Cmd + Enter` en Mac)

### Opción 3: Ejecutar sección por sección
- Colocar el cursor en cualquier sección
- Presionar `Ctrl + Enter` para ejecutar línea por línea

## 📊 Outputs Esperados

### Consola:
1. Estadísticas descriptivas de 5 activos
2. Matriz de correlaciones
3. Matriz de covarianzas
4. Parámetros de portafolio tangente (Markowitz)
5. Mejor portafolio simulado
6. Tabla comparativa de métricas
7. Composición de pesos

### Gráficos (en ventana de R):
1. Histogramas de retornos (distribución)
2. Histogramas de volatilidades
3. Histogramas de Sharpe ratios
4. Frontera eficiente de Markowitz
5. Gráfico combinado: simulaciones vs frontera exacta

### Archivos generados:
- `distribuciones_escenarios.png` - Gráfico de distribuciones (300 DPI)

## 📈 Interpretación de Resultados

### ¿Qué significa el error de 7.59%?
Es la diferencia entre el mejor Sharpe ratio simulado y el Markowitz exacto. **Es muy bueno** porque:
- 10,000 simulaciones es un número razonable
- El error es <10% en métrica clave
- La simulación diversifica más (5 activos vs 2)

### ¿Por qué Markowitz concentra?
La optimización exacta busca eficiencia máxima sin restricciones. AGG (bonos) tiene baja correlación con acciones, creando un portafolio muy eficiente con solo 2 activos.

### ¿Por qué la simulación diversifica?
Los pesos aleatorios distribuyen naturalmente entre activos. El "mejor" simulado mantiene casi todos los activos porque la probabilidad de concentración extrema es baja.

## 🔬 Metodología Técnica

### Programación Cuadrática (Markowitz)
Minimizar: $w^T \Sigma w$

Sujeto a:
- $\sum w_i = 1$ (restricción presupuestaria)
- $\sum w_i r_i = r_{objetivo}$ (retorno objetivo)
- $w_i \geq 0$ (long-only)

**Solver:** `quadprog::solve.QP()`

### Simulación Monte Carlo
Para cada una de 10,000 simulaciones:
1. Generar pesos aleatorios: $w_i \sim U(0,1)$
2. Normalizar: $w_i \leftarrow w_i / \sum w_j$
3. Calcular: retorno = $w^T r$, volatilidad = $\sqrt{w^T \Sigma w}$
4. Calcular: Sharpe = $(retorno - r_f) / volatilidad$

## 📚 Referencias

- Markowitz, H. (1952). "Portfolio Selection". *The Journal of Finance*
- Bodie, Z., Kane, A., & Marcus, A. J. (2014). *Investments* (10th ed.)
- R Documentation: `quadprog` package

## 👤 Autor

**Sebastian Egana**  
Email: sebaegana@gmail.com  
Repositorio: https://github.com/sebaegana/clase_finanzas_r_202609

## 📅 Fecha

Octubre 2026

## 📝 Notas Importantes

1. **Datos:** Los retornos están en formato decimal (0.05 = 5%)
2. **Periodicidad:** Los datos son mensuales; se anualizan multiplicando por 12
3. **Tasa libre de riesgo:** Fija en 2% anual (0.02)
4. **Reproducibilidad:** `set.seed(42)` asegura resultados consistentes
5. **Warnings:** Los mensajes sobre deprecación de `size` en ggplot2 pueden ignorarse

## 🔄 Workflow Recomendado

```
1. Ejecutar EDA (sección 1-2)
   ↓
2. Analizar correlaciones (sección 4-6)
   ↓
3. Ejecutar Markowitz (sección 7)
   ↓
4. Ejecutar Monte Carlo (sección 8)
   ↓
5. Ver gráficos (sección 9-10)
   ↓
6. Interpretar resultados (sección 11-12)
```

## 🤝 Contribuciones

Para reportar bugs o sugerir mejoras:
1. Abrir un issue en GitHub
2. Describir el problema/mejora
3. Incluir reproducible example si es posible

---

**Estado:** ✅ Funcional y probado  
**Última actualización:** 2026-10-01  
**Versión R:** 4.5.3+
