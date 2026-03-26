# =============================================================================
# Capítulo 17: Análisis de sensibilidad y robustez
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
cat("  Capítulo 17: Sensibilidad y robustez\n")
cat("  Escenarios y tornado chart\n")
cat("========================================\n\n")

# No genera CSV propio (reutiliza datasets previos)
# Genera un tornado chart y una tabla de escenarios como ejemplos

set.seed(1717)
central <- 500

variaciones <- tibble(
  supuesto = c("Margen de contribución", "Tasa de descuento", "Período de daño",
               "Grupo de control", "Forma funcional", "Inclusión persistencia"),
  bajo = c(420, 475, 380, 460, 490, 500),
  alto = c(580, 530, 620, 545, 515, 590)
) |> mutate(rango = alto - bajo, supuesto = fct_reorder(supuesto, rango))

cat("--- Tornado chart (valores) ---\n")
print(knitr::kable(variaciones |> select(supuesto, bajo, alto, rango),
                   format = "pipe", align = c("l", "r", "r", "r")))

g1 <- ggplot(variaciones) +
  geom_segment(aes(x = bajo, xend = alto, y = supuesto, yend = supuesto),
               linewidth = 8, color = "#3498db", alpha = 0.7) +
  geom_vline(xintercept = central, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
  geom_text(aes(x = bajo - 5, y = supuesto, label = bajo), hjust = 1, size = 3.2) +
  geom_text(aes(x = alto + 5, y = supuesto, label = alto), hjust = 0, size = 3.2) +
  labs(title = "Tornado chart: sensibilidad del daño", x = "Daño (miles EUR)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_17_tornado.png", g1, width = 9, height = 5.5, dpi = 150)
cat("\n[OK] output/fig_17_tornado.png\n")

# Escenarios
escenarios <- tibble(
  Escenario = c("Conservador", "Central", "Optimista"),
  `Margen` = c("35%", "42%", "50%"),
  `Período` = c("Solo infracción", "+ 6 meses", "+ persistencia"),
  `Tasa` = c("2%", "4%", "6%"),
  `Daño` = c(380, 500, 650)
)

cat("\n--- Escenarios ---\n")
print(knitr::kable(escenarios, format = "pipe", align = c("l", "l", "l", "l", "r")))

cat("\n========================================\n")
cat("  Capítulo 17 completado\n")
cat("========================================\n")
