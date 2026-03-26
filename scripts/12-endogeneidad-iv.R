# =============================================================================
# Capítulo 12: Endogeneidad y variables instrumentales
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
library(AER)

cat("\n========================================\n")
cat("  Capítulo 12: Variables instrumentales\n")
cat("  OLS vs. 2SLS\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN
# =============================================================================

set.seed(1212)
n <- 200
coste_transporte <- 15 + rnorm(n, 0, 3)
shock_demanda <- rnorm(n, 0, 4)
cartel <- c(rep(0, 120), rep(1, 80))

precio <- 30 + 0.8 * coste_transporte + 0.5 * shock_demanda + 12 * cartel + rnorm(n, 0, 2)
cantidad <- 200 - 1.5 * precio + 2 * shock_demanda + rnorm(n, 0, 5)

ds_iv <- tibble(obs = 1:n, precio = round(precio, 2), cantidad = round(cantidad, 2),
                coste_transporte = round(coste_transporte, 2), cartel = cartel)

write_csv(ds_iv, "data/ds_iv_demanda.csv")
cat("[OK] Dataset: data/ds_iv_demanda.csv (n =", n, ")\n\n")

# =============================================================================
# 2. OLS vs 2SLS
# =============================================================================

fit_ols <- lm(cantidad ~ precio + cartel, data = ds_iv)
fit_iv <- ivreg(cantidad ~ precio + cartel | coste_transporte + cartel, data = ds_iv)

cat("--- OLS vs. 2SLS ---\n")
print(knitr::kable(tibble(
  Método = c("OLS", "2SLS"),
  `Coef. precio` = c(round(coef(fit_ols)["precio"], 3), round(coef(fit_iv)["precio"], 3)),
  `Coef. cártel` = c(round(coef(fit_ols)["cartel"], 3), round(coef(fit_iv)["cartel"], 3))
), format = "pipe", align = c("l", "r", "r")))

# Primera etapa
primera_etapa <- lm(precio ~ coste_transporte + cartel, data = ds_iv)
cat("\n--- Primera etapa ---\n")
cat("  F-stat:", round(summary(primera_etapa)$fstatistic[1], 2), "(>10 requerido)\n")
cat("  R²:    ", round(summary(primera_etapa)$r.squared, 4), "\n\n")

# =============================================================================
# 3. GRÁFICO
# =============================================================================

g1 <- ggplot(ds_iv, aes(x = precio, y = cantidad, color = factor(cartel))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("0" = "#3498db", "1" = "#e74c3c"),
                     labels = c("Sin cártel", "Con cártel")) +
  labs(title = "Simultaneidad precio-cantidad", x = "Precio", y = "Cantidad", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_12_iv_scatter.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] output/fig_12_iv_scatter.png\n")

cat("\n========================================\n")
cat("  Capítulo 12 completado\n")
cat("========================================\n")
