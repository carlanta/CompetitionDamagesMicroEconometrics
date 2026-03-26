# =============================================================================
# Capítulo 7: Diferencias en diferencias (DID)
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
cat("  Capítulo 7: Diferencias en diferencias\n")
cat("  Estimación causal del efecto\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(307)

n <- 48
fecha <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = n)
inicio_tratamiento <- as.Date("2021-01-01")

df_did <- expand_grid(fecha = fecha, grupo = c("Control", "Tratado")) |>
  mutate(
    t = match(fecha, unique(fecha)),
    mes = factor(format(fecha, "%m")),
    tratado = if_else(grupo == "Tratado", 1, 0),
    post = if_else(fecha >= inicio_tratamiento, 1, 0),
    tendencia = 130 + 0.75 * t,
    estacionalidad = 5 * sin(2 * pi * t / 12),
    shock_comun = if_else(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-08-01"), -7, 0),
    nivel_grupo = if_else(grupo == "Tratado", -6, 0),
    efecto_tratamiento = if_else(tratado == 1 & post == 1, -14 - 0.08 * (t - 24), 0),
    ruido = rnorm(n(), 0, 2.4),
    ventas = tendencia + estacionalidad + shock_comun + nivel_grupo + efecto_tratamiento + ruido
  )

ds_did <- df_did |> select(fecha, grupo, tratado, post, ventas) |> mutate(ventas = round(ventas, 2))
write_csv(ds_did, "data/ds_did.csv")
cat("[OK] Dataset guardado: data/ds_did.csv\n")
cat("     Obs:", nrow(ds_did), "| Inicio tratamiento:", format(inicio_tratamiento), "\n\n")

# =============================================================================
# 2. TABLA 2x2 CLÁSICA
# =============================================================================

resumen_did <- df_did |>
  mutate(periodo = if_else(post == 1, "Después", "Antes")) |>
  group_by(grupo, periodo) |>
  summarise(media = mean(ventas), .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = media) |>
  mutate(Cambio = round(Después - Antes, 2), Antes = round(Antes, 2), Después = round(Después, 2))

did_total <- resumen_did$Cambio[resumen_did$grupo == "Tratado"] -
  resumen_did$Cambio[resumen_did$grupo == "Control"]

cat("--- Tabla DID 2x2 ---\n")
print(knitr::kable(resumen_did, format = "pipe", align = c("l", "r", "r", "r")))
cat("\n  Estimación DID (diferencia de cambios):", round(did_total, 2), "\n\n")

# =============================================================================
# 3. REGRESIÓN DID
# =============================================================================

modelo_did <- lm(ventas ~ tratado * post, data = df_did)
beta_did <- unname(coef(modelo_did)["tratado:post"])
se_did   <- sqrt(vcov(modelo_did)["tratado:post", "tratado:post"])

tabla_reg <- tibble(
  Parametro       = "Tratado x Post",
  Estimacion      = round(beta_did, 2),
  `Error estandar` = round(se_did, 2),
  `IC 95% inf.`   = round(beta_did - 1.96 * se_did, 2),
  `IC 95% sup.`   = round(beta_did + 1.96 * se_did, 2)
)

cat("--- Regresión DID ---\n")
print(knitr::kable(tabla_reg, format = "pipe", align = c("l", "r", "r", "r", "r")))
cat("\n")

# =============================================================================
# 4. GRÁFICO: series tratado vs. control
# =============================================================================

df_plot <- df_did |>
  group_by(fecha, grupo) |>
  summarise(ventas = mean(ventas), .groups = "drop")

g1 <- ggplot(df_plot, aes(x = fecha, y = ventas, color = grupo)) +
  annotate("rect", xmin = inicio_tratamiento, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(linewidth = 0.95) +
  geom_vline(xintercept = inicio_tratamiento, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("Tratado" = "#e74c3c", "Control" = "#2c3e50")) +
  labs(title = "Diferencias en diferencias",
       subtitle = "El grupo tratado se desvía del control tras la conducta",
       x = NULL, y = "Ventas medias", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_07_did_series.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_07_did_series.png\n")

# =============================================================================
# 5. GRÁFICO: brecha tratado - control
# =============================================================================

df_gap <- df_did |>
  group_by(fecha, grupo) |>
  summarise(ventas = mean(ventas), .groups = "drop") |>
  pivot_wider(names_from = grupo, values_from = ventas) |>
  mutate(brecha = Tratado - Control)

g2 <- ggplot(df_gap, aes(x = fecha, y = brecha)) +
  annotate("rect", xmin = inicio_tratamiento, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(color = "#8e44ad", linewidth = 1) +
  geom_hline(yintercept = mean(df_gap$brecha[df_gap$fecha < inicio_tratamiento]),
             linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = inicio_tratamiento, color = "grey50", linetype = "dashed") +
  labs(title = "Brecha tratado - control",
       subtitle = "Estable antes de la conducta; se deteriora después",
       x = NULL, y = "Brecha (tratado - control)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_07_did_gap.png", g2, width = 9, height = 4.8, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_07_did_gap.png\n")

# =============================================================================
# 6. ANÁLISIS DE SENSIBILIDAD
# =============================================================================

estimar_did <- function(data, fecha_inicio = inicio_tratamiento,
                        ventana_pre = as.Date("2019-01-01"),
                        incluir_mes = FALSE, excluir_shock = FALSE) {
  d <- data |> filter(fecha >= ventana_pre) |>
    mutate(post_alt = if_else(fecha >= fecha_inicio, 1, 0))
  if (excluir_shock) {
    d <- d |> filter(!(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-08-01")))
  }
  fit <- if (incluir_mes) lm(ventas ~ tratado * post_alt + mes, data = d) else lm(ventas ~ tratado * post_alt, data = d)
  tibble(efecto = round(unname(coef(fit)["tratado:post_alt"]), 2))
}

sens_did <- bind_rows(
  estimar_did(df_did) |> mutate(Especificacion = "DID básico"),
  estimar_did(df_did, incluir_mes = TRUE) |> mutate(Especificacion = "DID + estacionalidad"),
  estimar_did(df_did, ventana_pre = as.Date("2020-01-01")) |> mutate(Especificacion = "Ventana pre más corta"),
  estimar_did(df_did, excluir_shock = TRUE) |> mutate(Especificacion = "Excluyendo shock 2020")
) |> select(Especificacion, `Efecto estimado` = efecto)

cat("\n--- Análisis de sensibilidad ---\n")
print(knitr::kable(sens_did, format = "pipe", align = c("l", "r")))

cat("\n========================================\n")
cat("  Capítulo 7 completado\n")
cat("========================================\n")
