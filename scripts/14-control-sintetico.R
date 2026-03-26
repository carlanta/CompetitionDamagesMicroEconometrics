# =============================================================================
# Capítulo 14: Control sintético
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
cat("  Capítulo 14: Control sintético\n")
cat("  SCM manual con placebos\n")
cat("========================================\n\n")

set.seed(1414)
n_unidades <- 7; n_periodos <- 48; t_tratamiento <- 25

df_synth <- expand_grid(unidad = paste0("U", 1:n_unidades), periodo = 1:n_periodos) |>
  mutate(
    uid = match(unidad, unique(unidad)),
    fe = c(100, 95, 105, 98, 102, 110, 92)[uid],
    tendencia = 0.5 * periodo + 2 * sin(2 * pi * periodo / 12),
    tratada = if_else(unidad == "U1", 1, 0),
    efecto = if_else(tratada == 1 & periodo >= t_tratamiento, -15 - 0.3 * (periodo - t_tratamiento), 0),
    y = round(fe + tendencia + efecto + rnorm(n(), 0, 2.5), 2)
  )

ds_synth <- df_synth |> select(unidad, periodo, tratada, y)
write_csv(ds_synth, "data/ds_synth_exclusion.csv")
cat("[OK] Dataset: data/ds_synth_exclusion.csv\n")
cat("     Unidades:", n_unidades, "| Periodos:", n_periodos, "\n\n")

# Pesos del sintético
pre <- df_synth |> filter(periodo < t_tratamiento)
y_tratada <- pre |> filter(unidad == "U1") |> pull(y)
Y_don <- pre |> filter(unidad != "U1") |>
  pivot_wider(id_cols = periodo, names_from = unidad, values_from = y) |>
  select(-periodo) |> as.matrix()

w_raw <- coef(lm(y_tratada ~ Y_don - 1))
w_raw[w_raw < 0] <- 0
w <- w_raw / sum(w_raw)

full_mat <- df_synth |> filter(unidad != "U1") |>
  pivot_wider(id_cols = periodo, names_from = unidad, values_from = y) |>
  select(-periodo) |> as.matrix()
y_synth <- as.numeric(full_mat %*% w)
y_real <- df_synth |> filter(unidad == "U1") |> pull(y)

cat("--- Pesos del sintético ---\n")
print(knitr::kable(tibble(Donante = paste0("U", 2:n_unidades), Peso = round(w, 4)),
                   format = "pipe", align = c("l", "r")))

efecto_post <- y_real[t_tratamiento:n_periodos] - y_synth[t_tratamiento:n_periodos]
rmspe_pre <- sqrt(mean((y_real[1:(t_tratamiento-1)] - y_synth[1:(t_tratamiento-1)])^2))

cat("\n--- Efecto estimado ---\n")
cat("  Efecto medio:    ", round(mean(efecto_post), 2), "\n")
cat("  Efecto acumulado: ", round(sum(efecto_post), 2), "\n")
cat("  RMSPE pre:        ", round(rmspe_pre, 3), "\n\n")

# Gráfico
g1 <- tibble(periodo = 1:n_periodos, Tratada = y_real, Sintético = y_synth) |>
  pivot_longer(-periodo, names_to = "serie", values_to = "valor") |>
  ggplot(aes(x = periodo, y = valor, color = serie)) +
  annotate("rect", xmin = t_tratamiento, xmax = n_periodos, ymin = -Inf, ymax = Inf,
           fill = "#e74c3c", alpha = 0.06) +
  geom_line(linewidth = 0.95) +
  geom_vline(xintercept = t_tratamiento, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("Tratada" = "#e74c3c", "Sintético" = "#2c3e50")) +
  labs(title = "Control sintético", x = "Período", y = "Resultado", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_14_synth.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] output/fig_14_synth.png\n")

# Placebos
placebo_gaps <- map_dfr(paste0("U", 2:n_unidades), function(u) {
  tibble(periodo = 1:n_periodos,
         gap = df_synth |> filter(unidad == u) |> pull(y) - y_synth,
         unidad = u)
})
gap_real <- tibble(periodo = 1:n_periodos, gap = y_real - y_synth)

g2 <- ggplot() +
  geom_line(data = placebo_gaps, aes(x = periodo, y = gap, group = unidad),
            color = "grey70", alpha = 0.6) +
  geom_line(data = gap_real, aes(x = periodo, y = gap), color = "#e74c3c", linewidth = 1.1) +
  geom_vline(xintercept = t_tratamiento, color = "grey50", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey50") +
  labs(title = "Inferencia por placebos", x = "Período", y = "Brecha") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("output/fig_14_placebos.png", g2, width = 9, height = 5, dpi = 150)
cat("[OK] output/fig_14_placebos.png\n")

cat("\n========================================\n")
cat("  Capítulo 14 completado\n")
cat("========================================\n")
