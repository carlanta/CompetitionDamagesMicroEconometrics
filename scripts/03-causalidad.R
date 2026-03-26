# =============================================================================
# Capítulo 3: Causalidad, atribución y concurrencia de causas
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
cat("  Capítulo 3: Causalidad\n")
cat("  Atribución y concurrencia de causas\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS: correlación espuria (recesión + conducta)
# =============================================================================

set.seed(2024)
n <- 48
t <- 1:n
t_conducta <- 20
t_recesion <- 18

tendencia <- 150 + 1.2 * t
estacionalidad <- 8 * sin(2 * pi * t / 12)
ruido <- rnorm(n, 0, 3)

efecto_recesion <- rep(0, n)
efecto_recesion[t_recesion:n] <- -12 * (1 - exp(-0.08 * (t_recesion:n - t_recesion + 1)))

efecto_conducta <- rep(0, n)
efecto_conducta[t_conducta:n] <- -8

ventas_real         <- tendencia + estacionalidad + efecto_recesion + efecto_conducta + ruido
ventas_sin_nada     <- tendencia + estacionalidad + ruido
ventas_solo_recesion <- tendencia + estacionalidad + efecto_recesion + ruido

ds_causalidad <- tibble(
  mes                  = t,
  ventas_real          = round(ventas_real, 2),
  ventas_sin_nada      = round(ventas_sin_nada, 2),
  ventas_solo_recesion = round(ventas_solo_recesion, 2),
  t_conducta           = t_conducta,
  t_recesion           = t_recesion
)

write_csv(ds_causalidad, "data/ds_causalidad.csv")
cat("[OK] Dataset guardado: data/ds_causalidad.csv\n")
cat("     Observaciones:", nrow(ds_causalidad), "\n")
cat("     Inicio recesión: t =", t_recesion, "\n")
cat("     Inicio conducta: t =", t_conducta, "\n\n")

# =============================================================================
# 2. ANÁLISIS: cuantificación del error de atribución
# =============================================================================
# Daño erróneo: atribuir toda la caída a la conducta (ignorar recesión)
# Daño correcto: solo la brecha entre 'solo recesión' y 'observado'

ds_causalidad <- ds_causalidad |>
  mutate(
    dano_erroneo  = if_else(mes >= t_conducta, ventas_sin_nada - ventas_real, 0),
    dano_correcto = if_else(mes >= t_conducta, ventas_solo_recesion - ventas_real, 0)
  )

cat("--- Error de atribución ---\n")
cat("  Daño erróneo (toda la caída):       ",
    round(sum(ds_causalidad$dano_erroneo), 2), "miles EUR\n")
cat("  Daño correcto (solo conducta):      ",
    round(sum(ds_causalidad$dano_correcto), 2), "miles EUR\n")
cat("  Sobreestimación:                    ",
    round(sum(ds_causalidad$dano_erroneo) - sum(ds_causalidad$dano_correcto), 2), "miles EUR\n")
cat("  Sobreestimación relativa:           ",
    round((sum(ds_causalidad$dano_erroneo) / sum(ds_causalidad$dano_correcto) - 1) * 100, 1), "%\n\n")

# =============================================================================
# 3. GRÁFICO: correlación espuria (tres series)
# =============================================================================

df_plot <- ds_causalidad |>
  select(mes, ventas_real, ventas_sin_nada, ventas_solo_recesion) |>
  pivot_longer(-mes, names_to = "serie", values_to = "ventas")

g1 <- ggplot(df_plot, aes(x = mes, y = ventas, color = serie, linetype = serie)) +
  geom_vline(xintercept = t_conducta, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = t_recesion, linetype = "dotted", color = "grey60", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  annotate("text", x = t_conducta + 0.5, y = max(ventas_sin_nada) + 5,
           label = "Inicio conducta", hjust = 0, size = 3, color = "grey40") +
  annotate("text", x = t_recesion - 0.5, y = max(ventas_sin_nada) + 9,
           label = "Inicio recesión", hjust = 1, size = 3, color = "grey40") +
  annotate("segment", x = 38, xend = 38,
           y = ventas_solo_recesion[38], yend = ventas_real[38],
           color = "#e74c3c", linewidth = 1,
           arrow = arrow(ends = "both", length = unit(0.08, "inches"))) +
  annotate("text", x = 39, y = mean(c(ventas_solo_recesion[38], ventas_real[38])),
           label = "Daño real", fontface = "bold", size = 3.2, color = "#c0392b", hjust = 0) +
  scale_color_manual(values = c(
    "ventas_real" = "#e74c3c",
    "ventas_sin_nada" = "#2c3e50",
    "ventas_solo_recesion" = "#3498db"
  ), labels = c(
    "Observado (recesión + conducta)",
    "Contrafactual (sin nada)",
    "Solo efecto recesión"
  )) +
  scale_linetype_manual(values = c(
    "ventas_real" = "solid",
    "ventas_sin_nada" = "dashed",
    "ventas_solo_recesion" = "dotdash"
  ), labels = c(
    "Observado (recesión + conducta)",
    "Contrafactual (sin nada)",
    "Solo efecto recesión"
  )) +
  labs(
    title    = "Correlación espuria: recesión + conducta ilícita",
    subtitle = "El daño real es solo la brecha entre la línea azul y la roja",
    x = "Mes", y = "Ventas (miles EUR)", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_03_correlacion_espuria.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_03_correlacion_espuria.png\n")

# =============================================================================
# 4. TEST DE PLACEBO TEMPORAL
# =============================================================================

set.seed(77)
placebo_t      <- c(-8, -6, -4, -2, 0)
efecto_real    <- c(0, 0, 0, 0, -15)
efecto_estimado <- efecto_real + rnorm(5, 0, 2.5)
se             <- c(3.2, 2.8, 3.5, 3.0, 2.2)

df_placebo <- tibble(
  periodo  = placebo_t,
  efecto   = efecto_estimado,
  ci_low   = efecto_estimado - 1.96 * se,
  ci_high  = efecto_estimado + 1.96 * se,
  tipo     = c(rep("Placebo", 4), "Real")
)

cat("\n--- Test de placebo temporal ---\n")
print(knitr::kable(
  df_placebo |> mutate(across(c(efecto, ci_low, ci_high), ~ round(.x, 2))) |>
    select(periodo, tipo, efecto, ci_low, ci_high),
  format = "pipe", col.names = c("Período", "Tipo", "Efecto", "IC 95% inf.", "IC 95% sup."),
  align = c("r", "l", "r", "r", "r")
))

g2 <- ggplot(df_placebo, aes(x = periodo, y = efecto, color = tipo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("Placebo" = "#3498db", "Real" = "#e74c3c")) +
  scale_x_continuous(breaks = placebo_t,
                     labels = c("t-8\n(placebo)", "t-6\n(placebo)",
                                "t-4\n(placebo)", "t-2\n(placebo)",
                                "t=0\n(real)")) +
  labs(
    title = "Test de placebo temporal",
    subtitle = "Solo en el período real el efecto es significativo",
    x = NULL, y = "Efecto estimado sobre ventas", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_03_placebo.png", g2, width = 8, height = 5, dpi = 150)
cat("\n[OK] Gráfico guardado: output/fig_03_placebo.png\n")

# =============================================================================
# 5. DESCOMPOSICIÓN DE LA CAÍDA EN FACTORES CAUSALES
# =============================================================================

componentes <- tibble(
  factor = c("Conducta ilícita", "Recesión económica",
             "Entrada nuevo competidor", "Decisiones propias", "Residual"),
  efecto = c(-15, -10, -6, -3, -1),
  tipo   = c("Indemnizable", "No atribuible", "No atribuible", "No atribuible", "No atribuible")
)

cat("\n--- Descomposición de la caída total ---\n")
print(knitr::kable(componentes, format = "pipe", align = c("l", "r", "l")))
cat("\n  Caída total:       ", sum(componentes$efecto), "%\n")
cat("  Daño indemnizable: ", componentes$efecto[componentes$tipo == "Indemnizable"], "%\n\n")

componentes$factor <- fct_reorder(componentes$factor, componentes$efecto)

g3 <- ggplot(componentes, aes(x = factor, y = efecto, fill = tipo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(efecto, "%")),
            hjust = 1.2, color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_manual(values = c("Indemnizable" = "#e74c3c", "No atribuible" = "#95a5a6")) +
  labs(
    title    = "Descomposición causal de la caída de ventas",
    subtitle = "Solo la fracción atribuible a la conducta es indemnizable",
    x = NULL, y = "Efecto sobre ventas (%)", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_03_descomposicion.png", g3, width = 8, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_03_descomposicion.png\n")

cat("\n========================================\n")
cat("  Capítulo 3 completado\n")
cat("========================================\n")
