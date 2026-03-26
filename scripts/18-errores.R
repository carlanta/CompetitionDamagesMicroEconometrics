# =============================================================================
# Capítulo 18: Errores frecuentes en informes periciales de daños
# Cuantificación de Daños Antitrust
# =============================================================================
#
# Autor: Carlos de Anta Puig
#        Economista · Perito Financiero
#        Miembro del Colegio de Economistas de Madrid
#        Miembro del Instituto Español de Analistas Financieros (IEAF)
#        Profesor de Econometría y Microeconometría
#        carlos@cwconsultores.com
#
# =============================================================================

library(tidyverse)

cat("\n========================================\n")
cat("  Capítulo 18: Errores frecuentes\n")
cat("  10 trampas con ejemplo numérico\n")
cat("========================================\n\n")

# No genera CSV propio (capítulo conceptual con ejemplos numéricos)

# --- Error 1: Confundir sobreprecio con margen ---
cat("--- Error 1: Sobreprecio vs. margen ---\n")
cat("  Precio observado:      100\n")
cat("  Coste:                  70\n")
cat("  Margen empresarial:     30%\n")
cat("  Precio competitivo:     85\n")
cat("  Sobreprecio real:       17.6%\n")
cat("  Sobreestimación:       ", round((30 - 17.6) / 17.6 * 100, 0), "%\n\n")

# --- Error 7: Omisión del efecto volumen ---
cat("--- Error 7: Efecto volumen omitido ---\n")
area_A <- 1296; area_B <- 252
cat("  Sobreprecio (A):       ", area_A, "\n")
cat("  Efecto volumen (B):    ", area_B, "\n")
cat("  Daño total (A+B):      ", area_A + area_B, "\n")
cat("  Subestimación sin B:   ", round(area_B / (area_A + area_B) * 100, 1), "%\n\n")

# --- Tabla resumen ---
errores <- tibble(
  `Nº` = 1:10,
  Error = c("Confundir sobreprecio con margen", "Período previo contaminado",
            "Ignorar estacionalidad", "Comparable inadecuado",
            "Tendencias paralelas no verificadas", "Endogeneidad ignorada",
            "Omisión del efecto volumen", "Tasa de descuento inadecuada",
            "Sin análisis de sensibilidad", "Cherry picking"),
  Consecuencia = c("Sobreestimación", "Subestimación", "Sesgo variable",
                    "Contrafactual inválido", "Causalidad no creíble",
                    "Sesgo en sobreprecio", "Subestimación ~16%",
                    "Sesgo en actualización", "Pérdida credibilidad",
                    "Vulnerabilidad a impugnación")
)

cat("--- Los 10 errores ---\n")
print(knitr::kable(errores, format = "pipe", align = c("r", "l", "l")))

cat("\n========================================\n")
cat("  Capítulo 18 completado\n")
cat("========================================\n")
