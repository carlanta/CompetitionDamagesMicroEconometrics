# =============================================================================
# Capítulo 4: Datos y evidencia para la cuantificación
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
cat("  Capítulo 4: Datos y evidencia\n")
cat("  EDA orientado a cuantificación\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN: empresa afectada + 2 comparables
# =============================================================================

set.seed(303)
n <- 72
t <- 1:n
t_inicio <- 30
t_fin <- 54

base_afectada <- 100 + 0.8 * t + 6 * sin(2 * pi * t / 12)
efecto <- rep(0, n)
efecto[t_inicio:t_fin] <- -20
efecto[(t_fin + 1):n] <- -20 * exp(-0.12 * ((t_fin + 1):n - t_fin))
ventas_afectada <- base_afectada + efecto + rnorm(n, 0, 3)

ventas_comp1 <- 90 + 0.75 * t + 5 * sin(2 * pi * t / 12) + rnorm(n, 0, 3)
ventas_comp2 <- 110 + 0.85 * t + 4 * sin(2 * pi * (t + 2) / 12) + rnorm(n, 0, 3.5)

ds_eda_ventas <- tibble(
  mes             = t,
  ventas_afectada = round(ventas_afectada, 2),
  ventas_comp1    = round(ventas_comp1, 2),
  ventas_comp2    = round(ventas_comp2, 2),
  t_inicio        = t_inicio,
  t_fin           = t_fin
)

write_csv(ds_eda_ventas, "data/ds_eda_ventas.csv")
cat("[OK] Dataset guardado: data/ds_eda_ventas.csv\n")
cat("     Observaciones:", n, "meses (6 años)\n")
cat("     Período infractor: t =", t_inicio, "a t =", t_fin, "\n\n")

# =============================================================================
# 2. SIMULACIÓN: serie de precios con outliers
# =============================================================================

set.seed(404)
n_obs <- 60
precios <- 50 + 0.3 * (1:n_obs) + rnorm(n_obs, 0, 2)
precios[15] <- precios[15] + 15  # operación extraordinaria
precios[42] <- precios[42] - 12  # error de registro

ds_outliers <- tibble(
  obs    = 1:n_obs,
  precio = round(precios, 2)
)

write_csv(ds_outliers, "data/ds_outliers.csv")
cat("[OK] Dataset guardado: data/ds_outliers.csv\n")
cat("     Observaciones:", n_obs, "\n")
cat("     Outliers introducidos en obs 15 y 42\n\n")

# =============================================================================
# 3. EDA: estadísticas descriptivas por empresa
# =============================================================================

resumen_eda <- ds_eda_ventas |>
  select(ventas_afectada, ventas_comp1, ventas_comp2) |>
  pivot_longer(everything(), names_to = "empresa", values_to = "ventas") |>
  group_by(empresa) |>
  summarise(
    Media    = round(mean(ventas), 2),
    Mediana  = round(median(ventas), 2),
    `Desv. típ.` = round(sd(ventas), 2),
    Mínimo   = round(min(ventas), 2),
    Máximo   = round(max(ventas), 2),
    .groups  = "drop"
  ) |>
  mutate(empresa = case_when(
    empresa == "ventas_afectada" ~ "Empresa afectada",
    empresa == "ventas_comp1"    ~ "Comparable 1",
    empresa == "ventas_comp2"    ~ "Comparable 2"
  ))

cat("--- Estadísticas descriptivas ---\n")
print(knitr::kable(resumen_eda, format = "pipe", align = c("l", "r", "r", "r", "r", "r")))
cat("\n")

# =============================================================================
# 4. GRÁFICO: series temporales afectada vs. comparables
# =============================================================================

df_plot <- ds_eda_ventas |>
  select(mes, ventas_afectada, ventas_comp1, ventas_comp2) |>
  pivot_longer(-mes, names_to = "empresa", values_to = "ventas") |>
  mutate(empresa = case_when(
    empresa == "ventas_afectada" ~ "Afectada",
    empresa == "ventas_comp1"    ~ "Comparable 1",
    empresa == "ventas_comp2"    ~ "Comparable 2"
  ))

g1 <- ggplot(df_plot, aes(x = mes, y = ventas, color = empresa)) +
  annotate("rect", xmin = t_inicio, xmax = t_fin, ymin = -Inf, ymax = Inf,
           fill = "#e74c3c", alpha = 0.07) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = (t_inicio + t_fin) / 2, y = max(ds_eda_ventas$ventas_comp2) + 8,
           label = "Período infractor", size = 3.3, fontface = "bold", color = "#c0392b") +
  scale_color_manual(values = c("Afectada" = "#e74c3c",
                                "Comparable 1" = "#3498db",
                                "Comparable 2" = "#2ecc71")) +
  labs(
    title    = "EDA: empresa afectada vs. comparables",
    subtitle = "La ruptura en la empresa afectada es visible; los comparables mantienen su tendencia",
    x = "Mes", y = "Ventas (miles EUR)", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_04_eda_series.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_04_eda_series.png\n")

# =============================================================================
# 5. GRÁFICO: detección de outliers con media móvil
# =============================================================================

ds_outliers <- ds_outliers |>
  mutate(
    media_movil = zoo::rollmean(precio, k = 5, fill = NA, align = "center"),
    sd_movil    = zoo::rollapply(precio, width = 5, FUN = sd, fill = NA, align = "center"),
    desv        = abs(precio - media_movil),
    outlier     = !is.na(desv) & !is.na(sd_movil) & desv > 2.5 * sd_movil
  )

outliers_detectados <- ds_outliers |> filter(outlier)

cat("\n--- Outliers detectados ---\n")
if (nrow(outliers_detectados) > 0) {
  print(knitr::kable(
    outliers_detectados |> select(obs, precio, media_movil, desv) |>
      mutate(across(c(media_movil, desv), ~ round(.x, 2))),
    format = "pipe", align = c("r", "r", "r", "r")
  ))
} else {
  cat("  Ningún outlier detectado con umbral 2.5 sigma\n")
}

g2 <- ggplot(ds_outliers, aes(x = obs, y = precio)) +
  geom_line(color = "#2c3e50", linewidth = 0.6) +
  geom_line(aes(y = media_movil), color = "#3498db", linetype = "dashed", linewidth = 0.6) +
  geom_point(data = ds_outliers |> filter(outlier), color = "#e74c3c", size = 3) +
  labs(
    title    = "Detección de valores atípicos",
    subtitle = "Los puntos rojos se desvían más de 2.5 sigma de la media móvil",
    x = "Observación", y = "Precio unitario"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_04_outliers.png", g2, width = 8, height = 4.5, dpi = 150)
cat("\n[OK] Gráfico guardado: output/fig_04_outliers.png\n")

# =============================================================================
# 6. VERIFICACIÓN VISUAL DE TENDENCIAS PARALELAS (pre-infracción)
# =============================================================================

df_pre <- ds_eda_ventas |>
  filter(mes < t_inicio) |>
  select(mes, ventas_afectada, ventas_comp1, ventas_comp2) |>
  mutate(
    gap_comp1 = ventas_afectada - ventas_comp1,
    gap_comp2 = ventas_afectada - ventas_comp2
  )

cat("\n--- Brecha pre-infracción (afectada - comparables) ---\n")
cat("  vs. Comparable 1:  media =", round(mean(df_pre$gap_comp1), 2),
    ", sd =", round(sd(df_pre$gap_comp1), 2), "\n")
cat("  vs. Comparable 2:  media =", round(mean(df_pre$gap_comp2), 2),
    ", sd =", round(sd(df_pre$gap_comp2), 2), "\n")

cat("\n========================================\n")
cat("  Capítulo 4 completado\n")
cat("========================================\n")
