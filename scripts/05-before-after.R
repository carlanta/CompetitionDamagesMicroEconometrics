# =============================================================================
# Capítulo 5: Método antes-y-después (Before-and-After)
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
cat("  Capítulo 5: Before-and-After\n")
cat("  Estimación del contrafactual\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(105)

n <- 72
fecha <- seq.Date(as.Date("2018-01-01"), by = "month", length.out = n)
t <- 1:n
mes_num <- as.integer(format(fecha, "%m"))
mes_lab <- factor(mes_num)
inicio_infraccion <- as.Date("2022-01-01")

efecto_mes <- c(8, 5, 3, 1, -2, -4, -5, -3, 0, 2, 6, 10)
tendencia <- 120 + 1.1 * t
estacionalidad <- efecto_mes[mes_num]
ruido <- rnorm(n, 0, 3)

ventas_cf <- tendencia + estacionalidad + ruido
dano_real <- ifelse(fecha >= inicio_infraccion, 18 + 0.25 * (t - 48), 0)
ventas_obs <- ventas_cf - dano_real

ds_before_after <- tibble(
  fecha  = fecha,
  t      = t,
  mes    = mes_num,
  ventas = round(ventas_obs, 2)
)

write_csv(ds_before_after, "data/ds_before_after.csv")
cat("[OK] Dataset guardado: data/ds_before_after.csv\n")
cat("     Observaciones:", n, "meses\n")
cat("     Inicio infracción:", format(inicio_infraccion), "\n\n")

# =============================================================================
# 2. MODELO: regresión con tendencia + estacionalidad sobre período previo
# =============================================================================

df_ba <- ds_before_after |>
  mutate(mes = factor(mes))

modelo_pre <- lm(ventas ~ t + mes, data = df_ba |> filter(fecha < inicio_infraccion))

cat("--- Modelo estimado (período pre-infracción) ---\n")
cat("  R² ajustado:", round(summary(modelo_pre)$adj.r.squared, 4), "\n")
cat("  Coef. tendencia:", round(coef(modelo_pre)["t"], 3), "\n\n")

# =============================================================================
# 3. CONTRAFACTUAL Y DAÑO
# =============================================================================

df_ba <- df_ba |>
  mutate(
    contrafactual = predict(modelo_pre, newdata = df_ba),
    dano_mensual  = if_else(fecha >= inicio_infraccion, contrafactual - ventas, 0)
  )

resumen_ba <- df_ba |>
  filter(fecha >= inicio_infraccion) |>
  summarise(
    `Meses post-infracción`              = n(),
    `Ventas observadas acumuladas`       = round(sum(ventas), 1),
    `Ventas contrafactuales acumuladas`  = round(sum(contrafactual), 1),
    `Daño total estimado`                = round(sum(dano_mensual), 1),
    `Daño medio mensual`                 = round(mean(dano_mensual), 1)
  )

cat("--- Cuantificación del daño ---\n")
print(knitr::kable(
  resumen_ba |> pivot_longer(everything(), names_to = "Concepto", values_to = "Valor"),
  format = "pipe", align = c("l", "r")
))
cat("\n")

# =============================================================================
# 4. GRÁFICO: contrafactual vs. observado
# =============================================================================

g1 <- ggplot(df_ba, aes(x = fecha)) +
  annotate("rect", xmin = inicio_infraccion, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(aes(y = ventas, color = "Observado"), linewidth = 0.9) +
  geom_line(aes(y = contrafactual, color = "Contrafactual"), linewidth = 0.9, linetype = "dashed") +
  geom_vline(xintercept = inicio_infraccion, color = "grey50", linetype = "dashed") +
  annotate("text", x = inicio_infraccion, y = max(df_ba$contrafactual) + 8,
           label = "Inicio de la conducta", hjust = 0, size = 3.1, color = "grey35") +
  scale_color_manual(values = c("Observado" = "#e74c3c", "Contrafactual" = "#2c3e50")) +
  labs(
    title    = "Método antes-y-después",
    subtitle = "El contrafactual se proyecta desde la tendencia y estacionalidad preinfracción",
    x = NULL, y = "Ventas mensuales (miles EUR)", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_05_before_after.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_05_before_after.png\n")

# =============================================================================
# 5. GRÁFICO: daño mensual estimado
# =============================================================================

g2 <- ggplot(df_ba |> filter(fecha >= inicio_infraccion),
             aes(x = fecha, y = dano_mensual)) +
  geom_col(fill = "#c0392b", alpha = 0.85) +
  geom_hline(yintercept = 0, color = "grey50") +
  labs(
    title    = "Daño mensual estimado",
    subtitle = "Brecha contrafactual - observado en cada mes posterior a la conducta",
    x = NULL, y = "Daño mensual (miles EUR)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_05_dano_mensual.png", g2, width = 9, height = 4.8, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_05_dano_mensual.png\n")

# =============================================================================
# 6. ANÁLISIS DE SENSIBILIDAD
# =============================================================================

estimar_dano <- function(data, fecha_inicio_pre, usar_estacionalidad = TRUE) {
  muestra_pre <- data |>
    filter(fecha >= fecha_inicio_pre, fecha < inicio_infraccion)
  if (usar_estacionalidad) {
    fit <- lm(ventas ~ t + mes, data = muestra_pre)
  } else {
    fit <- lm(ventas ~ t, data = muestra_pre)
  }
  pred <- predict(fit, newdata = data)
  tibble(
    `Daño total`        = round(sum(pred[data$fecha >= inicio_infraccion] -
                                      data$ventas[data$fecha >= inicio_infraccion]), 1),
    `Daño medio mensual` = round(mean(pred[data$fecha >= inicio_infraccion] -
                                        data$ventas[data$fecha >= inicio_infraccion]), 1)
  )
}

sens_ba <- bind_rows(
  estimar_dano(df_ba, as.Date("2018-01-01"), TRUE) |>
    mutate(Especificacion = "Pre 2018-2021 + estacionalidad"),
  estimar_dano(df_ba, as.Date("2019-01-01"), TRUE) |>
    mutate(Especificacion = "Pre 2019-2021 + estacionalidad"),
  estimar_dano(df_ba, as.Date("2020-01-01"), TRUE) |>
    mutate(Especificacion = "Pre 2020-2021 + estacionalidad"),
  estimar_dano(df_ba, as.Date("2018-01-01"), FALSE) |>
    mutate(Especificacion = "Pre 2018-2021 sin estacionalidad")
) |>
  select(Especificacion, `Daño total`, `Daño medio mensual`)

cat("\n--- Análisis de sensibilidad ---\n")
print(knitr::kable(sens_ba, format = "pipe", align = c("l", "r", "r")))

cat("\n========================================\n")
cat("  Capítulo 5 completado\n")
cat("========================================\n")
