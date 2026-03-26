# =============================================================================
# Capítulo 9: El passing-on y la cadena de distribución
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
cat("  Capítulo 9: Passing-on\n")
cat("  Pass-through y reparto del daño\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(909)

n <- 60
fecha <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = n)
t <- 1:n
inicio_cartel <- as.Date("2021-07-01")

coste_base <- 40 + 0.3 * t + 2 * sin(2 * pi * t / 12) + rnorm(n, 0, 1)
sobreprecio_upstream <- if_else(fecha >= inicio_cartel, 10 + 0.05 * (t - 30), 0)
coste_total <- coste_base + sobreprecio_upstream

pass_through_real <- 0.62
precio_downstream <- 80 + 0.55 * coste_total + pass_through_real * sobreprecio_upstream +
  3 * sin(2 * pi * t / 12) + rnorm(n, 0, 1.5)

cantidad <- round(pmax(30, 200 - 1.2 * precio_downstream + rnorm(n, 0, 5)), 0)

ds_passing_on <- tibble(
  fecha = fecha,
  coste_base = round(coste_base, 2),
  sobreprecio_upstream = round(sobreprecio_upstream, 2),
  coste_total = round(coste_total, 2),
  precio_downstream = round(precio_downstream, 2),
  cantidad = cantidad,
  cartel = if_else(fecha >= inicio_cartel, 1, 0)
)

write_csv(ds_passing_on, "data/ds_passing_on.csv")
cat("[OK] Dataset guardado: data/ds_passing_on.csv\n")
cat("     Obs:", n, "| Inicio cártel:", format(inicio_cartel), "\n")
cat("     Pass-through real (DGP):", pass_through_real, "\n\n")

# =============================================================================
# 2. MODELO DE PASS-THROUGH
# =============================================================================

modelo_po <- lm(precio_downstream ~ coste_total + factor(format(fecha, "%m")),
                data = ds_passing_on)

rho <- unname(coef(modelo_po)["coste_total"])
se_rho <- sqrt(vcov(modelo_po)["coste_total", "coste_total"])

cat("--- Modelo de pass-through ---\n")
cat("  R² ajustado:  ", round(summary(modelo_po)$adj.r.squared, 4), "\n")
cat("  Pass-through:  ", round(rho, 4), "\n")
cat("  Error estándar:", round(se_rho, 4), "\n")
cat("  IC 95%:        [", round(rho - 1.96 * se_rho, 4), ",",
    round(rho + 1.96 * se_rho, 4), "]\n\n")

# =============================================================================
# 3. REPARTO DEL DAÑO
# =============================================================================

ds_passing_on <- ds_passing_on |>
  mutate(
    dano_directo   = if_else(cartel == 1, (1 - rho) * sobreprecio_upstream * cantidad, 0),
    dano_indirecto = if_else(cartel == 1, rho * sobreprecio_upstream * cantidad, 0),
    dano_total     = dano_directo + dano_indirecto
  )

resumen_po <- ds_passing_on |>
  filter(cartel == 1) |>
  summarise(
    `Meses cartelizados`         = n(),
    `Sobreprecio medio upstream` = round(mean(sobreprecio_upstream), 2),
    `Pass-through estimado`      = round(rho, 4),
    `Daño comprador directo`     = round(sum(dano_directo), 2),
    `Daño comprador indirecto`   = round(sum(dano_indirecto), 2),
    `Daño total`                 = round(sum(dano_total), 2)
  )

cat("--- Reparto del daño ---\n")
print(knitr::kable(
  resumen_po |> pivot_longer(everything(), names_to = "Concepto", values_to = "Valor"),
  format = "pipe", align = c("l", "r")
))
cat("\n")

# =============================================================================
# 4. GRÁFICO: precio observado vs. contrafactual aguas abajo
# =============================================================================

precio_cf <- 80 + 0.55 * coste_base + 3 * sin(2 * pi * t / 12) + rnorm(n, 0, 1.5)

g1 <- ggplot(ds_passing_on, aes(x = fecha)) +
  annotate("rect", xmin = inicio_cartel, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(aes(y = precio_downstream, color = "Observado"), linewidth = 0.95) +
  geom_line(aes(y = precio_cf, color = "Contrafactual"), linewidth = 0.95, linetype = "dashed",
            data = tibble(fecha = fecha, precio_cf = precio_cf)) +
  geom_vline(xintercept = inicio_cartel, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("Observado" = "#e74c3c", "Contrafactual" = "#2c3e50")) +
  labs(title = "Passing-on: precio aguas abajo",
       subtitle = "La brecha refleja el sobreprecio repercutido al comprador indirecto",
       x = NULL, y = "Precio aguas abajo", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_09_passing_on.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_09_passing_on.png\n")

# =============================================================================
# 5. GRÁFICO: reparto mensual del daño
# =============================================================================

df_dano_po <- ds_passing_on |>
  filter(cartel == 1) |>
  select(fecha, dano_directo, dano_indirecto) |>
  pivot_longer(-fecha, names_to = "componente", values_to = "dano")

g2 <- ggplot(df_dano_po, aes(x = fecha, y = dano, fill = componente)) +
  geom_col(position = "stack", alpha = 0.85) +
  scale_fill_manual(values = c("dano_directo" = "#2c3e50", "dano_indirecto" = "#e74c3c"),
                    labels = c("Comprador directo", "Comprador indirecto")) +
  labs(title = "Reparto mensual del daño", x = NULL, y = "Daño mensual", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_09_reparto_dano.png", g2, width = 9, height = 5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_09_reparto_dano.png\n")

# =============================================================================
# 6. ANÁLISIS DE SENSIBILIDAD
# =============================================================================

sens_po <- bind_rows(
  tibble(`Pass-through` = round(unname(coef(lm(precio_downstream ~ coste_total,
         data = ds_passing_on))["coste_total"]), 4),
         Especificacion = "Solo coste total"),
  tibble(`Pass-through` = round(rho, 4),
         Especificacion = "Con estacionalidad"),
  tibble(`Pass-through` = round(unname(coef(lm(precio_downstream ~ coste_base + sobreprecio_upstream +
         factor(format(fecha, "%m")), data = ds_passing_on))["sobreprecio_upstream"]), 4),
         Especificacion = "Sobreprecio separado")
) |> select(Especificacion, `Pass-through`)

cat("\n--- Análisis de sensibilidad ---\n")
print(knitr::kable(sens_po, format = "pipe", align = c("l", "r")))

cat("\n========================================\n")
cat("  Capítulo 9 completado\n")
cat("========================================\n")
