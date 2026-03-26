# =============================================================================
# Capítulo 8: Estimación del sobreprecio (Overcharge)
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
cat("  Capítulo 8: Sobreprecio (Overcharge)\n")
cat("  Modelo log-lineal y monetización\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(808)

fechas <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = 54)
firmas <- LETTERS[1:6]
inicio_cartel <- as.Date("2021-01-01")
fin_cartel    <- as.Date("2022-12-01")

df_over <- expand_grid(fecha = fechas, firma = firmas) |>
  mutate(
    t = match(fecha, unique(fecha)),
    mes = factor(format(fecha, "%m")),
    firma_id = match(firma, firmas),
    coste = 70 + 0.45 * t + 4 * sin(2 * pi * t / 12) + 0.6 * firma_id + rnorm(n(), 0, 1.5),
    demanda = 180 + 0.35 * t + 6 * cos(2 * pi * t / 12) + rnorm(n(), 0, 3),
    cartel = if_else(fecha >= inicio_cartel & fecha <= fin_cartel, 1, 0),
    firma_fe = c(-4, -2, 0, 1, 3, 5)[firma_id],
    precio_competitivo = 25 + 0.62 * coste + 0.08 * demanda + firma_fe + rnorm(n(), 0, 1.3),
    precio_observado = precio_competitivo * (1 + 0.17 * cartel) + rnorm(n(), 0, 0.9),
    cantidad = round(pmax(40, 320 - 1.6 * precio_observado + 0.35 * demanda + rnorm(n(), 0, 8)), 0)
  )

ds_sobreprecio <- df_over |>
  select(fecha, firma, coste, demanda, cartel, precio_observado, cantidad) |>
  mutate(coste = round(coste, 2), demanda = round(demanda, 2), precio_observado = round(precio_observado, 2))

write_csv(ds_sobreprecio, "data/ds_sobreprecio.csv")
cat("[OK] Dataset guardado: data/ds_sobreprecio.csv\n")
cat("     Obs:", nrow(ds_sobreprecio), "| Firmas:", length(firmas), "| Meses:", length(fechas), "\n")
cat("     Período de cártel:", format(inicio_cartel), "a", format(fin_cartel), "\n\n")

# =============================================================================
# 2. MODELO LOG-LINEAL DE PRECIOS
# =============================================================================

modelo_log <- lm(
  log(precio_observado) ~ cartel + log(coste) + log(demanda) + factor(firma) + factor(mes),
  data = df_over
)

beta_cartel   <- unname(coef(modelo_log)["cartel"])
se_beta       <- sqrt(vcov(modelo_log)["cartel", "cartel"])
overcharge_pct <- exp(beta_cartel) - 1

cat("--- Modelo log-lineal ---\n")
cat("  R² ajustado:       ", round(summary(modelo_log)$adj.r.squared, 4), "\n")
cat("  Coef. cartel (log):", round(beta_cartel, 4), "\n")
cat("  Sobreprecio (%):   ", round(100 * overcharge_pct, 2), "%\n")
cat("  IC 95%:            [", round(100 * (exp(beta_cartel - 1.96 * se_beta) - 1), 2), "% ,",
    round(100 * (exp(beta_cartel + 1.96 * se_beta) - 1), 2), "%]\n\n")

# =============================================================================
# 3. PRECIO COMPETITIVO CONTRAFACTUAL Y DAÑO
# =============================================================================

df_over <- df_over |>
  mutate(
    precio_competitivo_hat = if_else(cartel == 1, precio_observado / (1 + overcharge_pct), precio_observado),
    sobreprecio_unitario   = if_else(cartel == 1, precio_observado - precio_competitivo_hat, 0),
    dano_directo           = sobreprecio_unitario * cantidad
  )

resumen_dano <- df_over |>
  filter(cartel == 1) |>
  summarise(
    `Meses cartelizados`         = n_distinct(fecha),
    `Precio observado medio`     = round(mean(precio_observado), 2),
    `Precio competitivo medio`   = round(mean(precio_competitivo_hat), 2),
    `Sobreprecio unitario medio` = round(mean(sobreprecio_unitario), 2),
    `Daño directo total`         = round(sum(dano_directo), 2)
  )

cat("--- Cuantificación del daño ---\n")
print(knitr::kable(
  resumen_dano |> pivot_longer(everything(), names_to = "Concepto", values_to = "Valor"),
  format = "pipe", align = c("l", "r")
))
cat("\n")

# =============================================================================
# 4. GRÁFICO: precio observado vs. competitivo
# =============================================================================

df_plot <- df_over |>
  group_by(fecha) |>
  summarise(precio_observado = mean(precio_observado),
            precio_competitivo = mean(precio_competitivo), .groups = "drop") |>
  pivot_longer(-fecha, names_to = "serie", values_to = "precio")

g1 <- ggplot(df_plot, aes(x = fecha, y = precio, color = serie)) +
  annotate("rect", xmin = inicio_cartel, xmax = fin_cartel,
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(linewidth = 0.95) +
  geom_vline(xintercept = inicio_cartel, color = "grey50", linetype = "dashed") +
  geom_vline(xintercept = fin_cartel, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("precio_observado" = "#e74c3c", "precio_competitivo" = "#2c3e50"),
                     labels = c("Precio observado", "Precio competitivo")) +
  labs(title = "Sobreprecio: precio observado vs. competitivo",
       subtitle = "La brecha durante el cártel representa el sobreprecio",
       x = NULL, y = "Precio medio", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_08_overcharge.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_08_overcharge.png\n")

# =============================================================================
# 5. GRÁFICO: daño monetario mensual
# =============================================================================

df_dano_mensual <- df_over |>
  filter(cartel == 1) |>
  group_by(fecha) |>
  summarise(dano = sum(dano_directo), .groups = "drop")

g2 <- ggplot(df_dano_mensual, aes(x = fecha, y = dano)) +
  geom_col(fill = "#c0392b", alpha = 0.85) +
  labs(title = "Daño monetario mensual por sobreprecio",
       x = NULL, y = "Daño directo mensual") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_08_dano_mensual.png", g2, width = 9, height = 4.8, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_08_dano_mensual.png\n")

# =============================================================================
# 6. ANÁLISIS DE SENSIBILIDAD
# =============================================================================

estimar_overcharge <- function(formula, data) {
  fit <- lm(formula, data = data)
  beta <- unname(coef(fit)["cartel"])
  tibble(`Sobreprecio (%)` = round(100 * (exp(beta) - 1), 2))
}

df_alt <- df_over |>
  mutate(cartel_alt = if_else(fecha >= as.Date("2021-03-01") & fecha <= as.Date("2022-10-01"), 1, 0))

fit_alt <- lm(log(precio_observado) ~ cartel_alt + log(coste) + log(demanda) + factor(firma) + factor(mes),
              data = df_alt)

sens_over <- bind_rows(
  estimar_overcharge(log(precio_observado) ~ cartel, df_over) |>
    mutate(Especificacion = "Solo indicador de cartel"),
  estimar_overcharge(log(precio_observado) ~ cartel + log(coste) + log(demanda), df_over) |>
    mutate(Especificacion = "Con controles coste y demanda"),
  estimar_overcharge(log(precio_observado) ~ cartel + log(coste) + log(demanda) + factor(firma) + factor(mes), df_over) |>
    mutate(Especificacion = "Con controles y efectos fijos"),
  tibble(`Sobreprecio (%)` = round(100 * (exp(unname(coef(fit_alt)["cartel_alt"])) - 1), 2),
         Especificacion = "Ventana de cártel alternativa")
) |> select(Especificacion, `Sobreprecio (%)`)

cat("\n--- Análisis de sensibilidad ---\n")
print(knitr::kable(sens_over, format = "pipe", align = c("l", "r")))

cat("\n========================================\n")
cat("  Capítulo 8 completado\n")
cat("========================================\n")
