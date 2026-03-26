# =============================================================================
# Capítulo 10: Estimación del lucro cesante (Lost Profits)
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
cat("  Capítulo 10: Lucro cesante\n")
cat("  Proyección, márgenes y actualización\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(1010)

n <- 72
fecha <- seq.Date(as.Date("2018-01-01"), by = "month", length.out = n)
t <- 1:n
mes_num <- as.integer(format(fecha, "%m"))
inicio_exclusion <- as.Date("2021-01-01")
fin_exclusion    <- as.Date("2023-06-01")

# Ingresos contrafactuales y observados
ingresos_cf <- 500 + 4.5 * t + 30 * sin(2 * pi * t / 12) + rnorm(n, 0, 12)
efecto_exclusion <- if_else(fecha >= inicio_exclusion & fecha <= fin_exclusion,
                            -120 - 0.8 * (t - 36), 0)
efecto_post <- if_else(fecha > fin_exclusion,
                       (-120 - 0.8 * 30) * exp(-0.15 * (t - 66)), 0)
ingresos_obs <- ingresos_cf + efecto_exclusion + efecto_post + rnorm(n, 0, 5)

# Costes
ratio_cv <- 0.58
costes_variables_cf  <- ratio_cv * ingresos_cf + rnorm(n, 0, 4)
costes_variables_obs <- ratio_cv * ingresos_obs + rnorm(n, 0, 4)
costes_fijos <- 120 + 0.5 * t + rnorm(n, 0, 3)

# Beneficios
beneficio_cf  <- ingresos_cf - costes_variables_cf - costes_fijos
beneficio_obs <- ingresos_obs - costes_variables_obs - costes_fijos

ds_lucro_cesante <- tibble(
  fecha = fecha, t = t, mes = mes_num,
  ingresos_cf = round(ingresos_cf, 2),
  ingresos_obs = round(ingresos_obs, 2),
  costes_variables_cf = round(costes_variables_cf, 2),
  costes_variables_obs = round(costes_variables_obs, 2),
  costes_fijos = round(costes_fijos, 2),
  beneficio_cf = round(beneficio_cf, 2),
  beneficio_obs = round(beneficio_obs, 2),
  exclusion = if_else(fecha >= inicio_exclusion, 1, 0)
)

write_csv(ds_lucro_cesante, "data/ds_lucro_cesante.csv")
cat("[OK] Dataset guardado: data/ds_lucro_cesante.csv\n")
cat("     Obs:", n, "| Exclusión:", format(inicio_exclusion), "a", format(fin_exclusion), "\n")
cat("     Ratio costes variables:", ratio_cv, "\n\n")

# =============================================================================
# 2. CUANTIFICACIÓN DEL LUCRO CESANTE
# =============================================================================

margen_contribucion <- 1 - ratio_cv

ds_lucro_cesante <- ds_lucro_cesante |>
  mutate(
    ingresos_perdidos = if_else(exclusion == 1, ingresos_cf - ingresos_obs, 0),
    lucro_bruto       = ingresos_perdidos * margen_contribucion,
    costes_evitados   = if_else(exclusion == 1, costes_variables_cf - costes_variables_obs, 0),
    lucro_neto        = if_else(exclusion == 1, beneficio_cf - beneficio_obs, 0)
  )

resumen_lc <- ds_lucro_cesante |>
  filter(exclusion == 1) |>
  summarise(
    `Meses afectados`                    = n(),
    `Ingresos perdidos totales`          = round(sum(ingresos_perdidos), 2),
    `Margen de contribución`             = round(margen_contribucion, 4),
    `Lucro bruto (ingresos x margen)`    = round(sum(lucro_bruto), 2),
    `Costes evitados`                    = round(sum(costes_evitados), 2),
    `Lucro cesante neto`                 = round(sum(lucro_neto), 2)
  )

cat("--- Cuantificación del lucro cesante ---\n")
print(knitr::kable(
  resumen_lc |> pivot_longer(everything(), names_to = "Concepto", values_to = "Valor"),
  format = "pipe", align = c("l", "r")
))
cat("\n")

# =============================================================================
# 3. ACTUALIZACIÓN FINANCIERA
# =============================================================================

fecha_ref <- max(fecha)
T_ref <- which(fecha == fecha_ref)

actualizar <- function(data, tasa_anual) {
  r_mensual <- (1 + tasa_anual)^(1/12) - 1
  data |>
    filter(exclusion == 1) |>
    mutate(factor_act = (1 + r_mensual)^(T_ref - t)) |>
    summarise(LC_actualizado = round(sum(lucro_neto * factor_act), 2)) |>
    mutate(`Tasa anual` = paste0(round(tasa_anual * 100, 1), "%"))
}

sens_tasa <- bind_rows(
  actualizar(ds_lucro_cesante, 0.02),
  actualizar(ds_lucro_cesante, 0.04),
  actualizar(ds_lucro_cesante, 0.06),
  actualizar(ds_lucro_cesante, 0.08)
) |> select(`Tasa anual`, `LC actualizado` = LC_actualizado)

cat("--- Actualización financiera ---\n")
print(knitr::kable(sens_tasa, format = "pipe", align = c("l", "r")))
cat("\n")

# =============================================================================
# 4. GRÁFICO: beneficio contrafactual vs. observado
# =============================================================================

df_plot <- ds_lucro_cesante |>
  select(fecha, beneficio_cf, beneficio_obs) |>
  pivot_longer(-fecha, names_to = "escenario", values_to = "beneficio")

g1 <- ggplot(df_plot, aes(x = fecha, y = beneficio, color = escenario)) +
  annotate("rect", xmin = inicio_exclusion, xmax = fin_exclusion,
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(linewidth = 0.95) +
  geom_vline(xintercept = inicio_exclusion, color = "grey50", linetype = "dashed") +
  geom_vline(xintercept = fin_exclusion, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("beneficio_cf" = "#2c3e50", "beneficio_obs" = "#e74c3c"),
                     labels = c("Beneficio contrafactual", "Beneficio observado")) +
  labs(title = "Lucro cesante: beneficio contrafactual vs. observado",
       subtitle = "La brecha entre ambas curvas es el beneficio perdido",
       x = NULL, y = "Beneficio mensual (miles EUR)", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_10_lucro_cesante.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_10_lucro_cesante.png\n")

# =============================================================================
# 5. GRÁFICO: lucro cesante mensual
# =============================================================================

g2 <- ggplot(ds_lucro_cesante |> filter(exclusion == 1),
             aes(x = fecha, y = lucro_neto)) +
  geom_col(fill = "#c0392b", alpha = 0.85) +
  labs(title = "Lucro cesante mensual",
       x = NULL, y = "Lucro cesante (miles EUR)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_10_lucro_mensual.png", g2, width = 9, height = 4.8, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_10_lucro_mensual.png\n")

# =============================================================================
# 6. ANÁLISIS DE SENSIBILIDAD (margen)
# =============================================================================

ingresos_perdidos_total <- sum(ds_lucro_cesante$ingresos_perdidos[ds_lucro_cesante$exclusion == 1])

sens_margen <- tibble(
  Supuesto = c("Margen contribución 35%", "Margen contribución 42%", "Margen contribución 50%"),
  `Lucro cesante` = c(
    round(ingresos_perdidos_total * 0.35, 2),
    round(ingresos_perdidos_total * 0.42, 2),
    round(ingresos_perdidos_total * 0.50, 2)
  )
)

cat("\n--- Sensibilidad al margen ---\n")
print(knitr::kable(sens_margen, format = "pipe", align = c("l", "r")))

cat("\n========================================\n")
cat("  Capítulo 10 completado\n")
cat("========================================\n")
