# =============================================================================
# Capítulo 15: Regresión cuantílica y efectos heterogéneos
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
library(quantreg)

cat("\n========================================\n")
cat("  Capítulo 15: Regresión cuantílica\n")
cat("  Efectos heterogéneos del cártel\n")
cat("========================================\n\n")

set.seed(1515)
n <- 400
tamano <- runif(n, 10, 500)
cartel <- c(rep(0, 200), rep(1, 200))
efecto_cartel <- cartel * (18 - 0.02 * tamano + rnorm(n, 0, 2))
precio <- 50 + 0.05 * tamano + efecto_cartel + rnorm(n, 0, 3)

ds_qr <- tibble(obs = 1:n, precio = round(precio, 2), tamano = round(tamano, 2), cartel = cartel)

write_csv(ds_qr, "data/ds_quantreg_precios.csv")
cat("[OK] Dataset: data/ds_quantreg_precios.csv (n =", n, ")\n\n")

# OLS
fit_ols <- lm(precio ~ cartel + tamano, data = ds_qr)
cat("--- OLS (efecto medio) ---\n")
cat("  Sobreprecio medio:", round(coef(fit_ols)["cartel"], 2), "\n\n")

# Regresión cuantílica
taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)
resultados <- map_dfr(taus, function(tau) {
  fit <- rq(precio ~ cartel + tamano, data = ds_qr, tau = tau)
  tibble(Cuantil = tau, Sobreprecio = round(coef(fit)["cartel"], 2))
})

cat("--- Sobreprecio por cuantiles ---\n")
print(knitr::kable(resultados, format = "pipe", align = c("r", "r")))
cat("\n")

# Gráfico scatter
g1 <- ggplot(ds_qr, aes(x = tamano, y = precio, color = factor(cartel))) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("0" = "#3498db", "1" = "#e74c3c"),
                     labels = c("Sin cártel", "Con cártel")) +
  labs(title = "Efectos heterogéneos del cártel", x = "Tamaño transacción", y = "Precio", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_15_quantreg_scatter.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] output/fig_15_quantreg_scatter.png\n")

# Gráfico de coeficientes por cuantiles
taus_fine <- seq(0.05, 0.95, by = 0.05)
coefs_qr <- map_dfr(taus_fine, function(tau) {
  fit <- rq(precio ~ cartel + tamano, data = ds_qr, tau = tau)
  se <- tryCatch(summary(fit, se = "nid")$coefficients["cartel", "Std. Error"],
                 error = function(e) NA_real_)
  tibble(tau = tau, coef = coef(fit)["cartel"], se = se)
})

g2 <- ggplot(coefs_qr, aes(x = tau, y = coef)) +
  geom_ribbon(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), fill = "#3498db", alpha = 0.2) +
  geom_line(color = "#2c3e50", linewidth = 1) +
  geom_point(color = "#2c3e50", size = 2) +
  geom_hline(yintercept = coef(fit_ols)["cartel"], color = "#e74c3c", linetype = "dashed") +
  labs(title = "Proceso cuantílico del sobreprecio", x = "Cuantil", y = "Sobreprecio") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_15_quantreg_process.png", g2, width = 8, height = 5, dpi = 150)
cat("[OK] output/fig_15_quantreg_process.png\n")

cat("\n========================================\n")
cat("  Capítulo 15 completado\n")
cat("========================================\n")
