# =============================================================================
# Capítulo 19: Redacción y defensa del informe pericial
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
cat("  Capítulo 19: Informe pericial\n")
cat("  Estructura, comunicación, defensa\n")
cat("========================================\n\n")

# Este capítulo es conceptual; el script genera la tabla-ejemplo
# que se usaría en un informe real

cat("--- Estructura recomendada del informe ---\n")
estructura <- tibble(
  Sección = c("1. Resumen ejecutivo", "2. Mandato y cualificación",
              "3. Hechos relevantes", "4. Metodología",
              "5. Datos utilizados", "6. Resultados",
              "7. Sensibilidad", "8. Conclusiones", "Anexos"),
  `Público` = c("Juez", "Juez", "Juez/abogados", "Perito contrario",
                "Perito contrario", "Juez/abogados", "Juez/perito",
                "Juez", "Técnico")
)
print(knitr::kable(estructura, format = "pipe", align = c("l", "l")))

cat("\n--- Ejemplo de tabla de resultados ---\n")
ejemplo <- tibble(
  Concepto = c("Sobreprecio estimado", "Período cartelizado",
               "Cantidad afectada", "Daño directo total",
               "Intereses compensatorios (4%)", "Daño total actualizado"),
  Valor = c("17.0%", "Ene 2021 – Dic 2022 (24 meses)",
            "15.200 unidades", "488.558 EUR",
            "52.340 EUR", "540.898 EUR")
)
print(knitr::kable(ejemplo, format = "pipe", align = c("l", "r")))

cat("\n--- 5 reglas de la comparecencia ---\n")
cat("  1. Explicar el método en 5 minutos, sin jerga\n")
cat("  2. Responder a objeciones con calma y datos\n")
cat("  3. Reconocer limitaciones sin socavar el resultado\n")
cat("  4. No improvisar; ofrecer respuesta escrita si es necesario\n")
cat("  5. Mantener la objetividad: el perito no es abogado\n")

cat("\n========================================\n")
cat("  Capítulo 19 completado\n")
cat("========================================\n")
