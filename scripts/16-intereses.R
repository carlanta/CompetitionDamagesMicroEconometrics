# =============================================================================
# Capítulo 16: Actualización financiera y cálculo de intereses
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
cat("  Capítulo 16: Actualización financiera\n")
cat("  Intereses compensatorios y moratorios\n")
cat("========================================\n\n")

set.seed(1616)

n_dano <- 30
fecha_inicio <- as.Date("2020-01-01")
fecha_sentencia <- as.Date("2024-12-01")
fechas_dano <- seq.Date(fecha_inicio, by = "month", length.out = n_dano)
dano_mensual <- 50 + 0.8 * (1:n_dano) + rnorm(n_dano, 0, 5)

ds_intereses <- tibble(fecha = fechas_dano, t = 1:n_dano, dano = round(dano_mensual, 2))

write_csv(ds_intereses, "data/ds_intereses.csv")
cat("[OK] Dataset: data/ds_intereses.csv (n =", n_dano, ")\n\n")

# Actualización
tasa_anual <- 0.04
r_mensual <- (1 + tasa_anual)^(1/12) - 1
meses_a_sent <- as.numeric(difftime(fecha_sentencia, fechas_dano, units = "days")) / 30.44
factor_act <- (1 + r_mensual)^meses_a_sent

ds_intereses <- ds_intereses |>
  mutate(factor_actualizacion = round(factor_act, 4),
         dano_actualizado = round(dano * factor_act, 2))

cat("--- Resultado ---\n")
cat("  Daño nominal:      ", round(sum(ds_intereses$dano), 2), "\n")
cat("  Daño actualizado:  ", round(sum(ds_intereses$dano_actualizado), 2), "\n")
cat("  Intereses:         ", round(sum(ds_intereses$dano_actualizado) - sum(ds_intereses$dano), 2), "\n")
cat("  Tasa aplicada:     ", tasa_anual * 100, "%\n\n")

# Sensibilidad
tasas <- c(0.02, 0.03, 0.04, 0.05, 0.06, 0.08)
sens <- map_dfr(tasas, function(ta) {
  rm <- (1 + ta)^(1/12) - 1
  fa <- (1 + rm)^meses_a_sent
  tibble(`Tasa` = paste0(ta*100, "%"), `Actualizado` = round(sum(ds_intereses$dano * fa), 2))
})

cat("--- Sensibilidad a la tasa ---\n")
print(knitr::kable(sens, format = "pipe", align = c("l", "r")))

# Gráfico
df_plot <- ds_intereses |> select(fecha, dano, dano_actualizado) |>
  pivot_longer(-fecha, names_to = "tipo", values_to = "valor")

g1 <- ggplot(df_plot, aes(x = fecha, y = valor, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("dano" = "#2c3e50", "dano_actualizado" = "#e74c3c"),
                    labels = c("Nominal", "Actualizado")) +
  labs(title = "Daño nominal vs. actualizado", x = NULL, y = "Daño mensual (miles EUR)", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_16_actualizacion.png", g1, width = 9, height = 4.5, dpi = 150)
cat("\n[OK] output/fig_16_actualizacion.png\n")

cat("\n========================================\n")
cat("  Capítulo 16 completado\n")
cat("========================================\n")
