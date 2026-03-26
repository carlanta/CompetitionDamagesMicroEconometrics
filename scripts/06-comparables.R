# =============================================================================
# Capítulo 6: Método de comparables (Yardstick)
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
cat("  Capítulo 6: Comparables (Yardstick)\n")
cat("  Contrafactual con benchmark externo\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(206)

n <- 60
fecha <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = n)
t <- 1:n
inicio_infraccion <- as.Date("2022-01-01")

coste_comun   <- 50 + 0.35 * t + 3 * sin(2 * pi * t / 12) + rnorm(n, 0, 1.2)
demanda_comun <- 100 + 0.4 * t + 2 * cos(2 * pi * t / 12) + rnorm(n, 0, 1.5)

precio_B <- 12 + 0.6 * coste_comun + 0.03 * demanda_comun + rnorm(n, 0, 0.8)
precio_C <- 13 + 0.58 * coste_comun + 0.028 * demanda_comun + rnorm(n, 0, 0.8)

sobrecoste_cartel <- ifelse(fecha >= inicio_infraccion, 8 + 0.10 * (t - 36), 0)
precio_A <- 12.5 + 0.59 * coste_comun + 0.029 * demanda_comun + sobrecoste_cartel + rnorm(n, 0, 0.9)
cantidad_A <- round(180 - 1.2 * precio_A + rnorm(n, 0, 4), 0)

ds_comparables <- tibble(
  fecha = fecha, precio_A = round(precio_A, 2), precio_B = round(precio_B, 2),
  precio_C = round(precio_C, 2), coste_comun = round(coste_comun, 2),
  demanda_comun = round(demanda_comun, 2), cantidad_A = cantidad_A
)

write_csv(ds_comparables, "data/ds_comparables.csv")
cat("[OK] Dataset guardado: data/ds_comparables.csv\n")
cat("     Observaciones:", n, "| Inicio infracción:", format(inicio_infraccion), "\n\n")

# =============================================================================
# 2. CONTRAFACTUAL CON YARDSTICK PROMEDIO
# =============================================================================

df_comp <- ds_comparables |>
  mutate(
    comparable_promedio = (precio_B + precio_C) / 2,
    diferencial_pre    = mean((precio_A - comparable_promedio)[fecha < inicio_infraccion]),
    contrafactual_A    = comparable_promedio + diferencial_pre,
    sobreprecio        = if_else(fecha >= inicio_infraccion, precio_A - contrafactual_A, 0),
    dano_monetario     = if_else(fecha >= inicio_infraccion, sobreprecio * cantidad_A, 0)
  )

resumen_comp <- df_comp |>
  filter(fecha >= inicio_infraccion) |>
  summarise(
    `Meses afectados`     = n(),
    `Sobreprecio medio`   = round(mean(sobreprecio), 2),
    `Cantidad media`      = round(mean(cantidad_A), 1),
    `Daño monetario total` = round(sum(dano_monetario), 1)
  )

cat("--- Cuantificación del daño ---\n")
print(knitr::kable(
  resumen_comp |> pivot_longer(everything(), names_to = "Concepto", values_to = "Valor"),
  format = "pipe", align = c("l", "r")
))
cat("\n")

# =============================================================================
# 3. GRÁFICO: mercados afectado + comparables
# =============================================================================

df_plot <- ds_comparables |>
  select(fecha, precio_A, precio_B, precio_C) |>
  pivot_longer(-fecha, names_to = "serie", values_to = "precio") |>
  mutate(serie = recode(serie, precio_A = "Mercado afectado",
                        precio_B = "Comparable B", precio_C = "Comparable C"))

g1 <- ggplot(df_plot, aes(x = fecha, y = precio, color = serie)) +
  annotate("rect", xmin = inicio_infraccion, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = inicio_infraccion, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("Mercado afectado" = "#e74c3c",
                                "Comparable B" = "#3498db", "Comparable C" = "#2ecc71")) +
  labs(title = "Método de comparables", subtitle = "El mercado afectado se separa del benchmark tras la conducta",
       x = NULL, y = "Precio medio", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_06_comparables.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_06_comparables.png\n")

# =============================================================================
# 4. GRÁFICO: brecha afectado vs. contrafactual yardstick
# =============================================================================

g2 <- ggplot(df_comp, aes(x = fecha)) +
  annotate("rect", xmin = inicio_infraccion, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(aes(y = precio_A, color = "Mercado afectado"), linewidth = 0.95) +
  geom_line(aes(y = contrafactual_A, color = "Contrafactual yardstick"), linewidth = 0.95) +
  geom_vline(xintercept = inicio_infraccion, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("Mercado afectado" = "#e74c3c", "Contrafactual yardstick" = "#2c3e50")) +
  labs(title = "Precio observado vs. contrafactual", x = NULL, y = "Precio medio", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_06_yardstick_gap.png", g2, width = 9, height = 5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_06_yardstick_gap.png\n")

# =============================================================================
# 5. ANÁLISIS DE SENSIBILIDAD
# =============================================================================

calcular_dano_comp <- function(serie_control, nombre) {
  dif_pre <- mean((df_comp$precio_A - serie_control)[df_comp$fecha < inicio_infraccion])
  cf <- serie_control + dif_pre
  sp <- ifelse(df_comp$fecha >= inicio_infraccion, df_comp$precio_A - cf, 0)
  dt <- sum(sp[df_comp$fecha >= inicio_infraccion] * df_comp$cantidad_A[df_comp$fecha >= inicio_infraccion])
  tibble(Benchmark = nombre,
         `Sobreprecio medio` = round(mean(sp[df_comp$fecha >= inicio_infraccion]), 2),
         `Daño total` = round(dt, 1))
}

sens_comp <- bind_rows(
  calcular_dano_comp(df_comp$precio_B, "Solo comparable B"),
  calcular_dano_comp(df_comp$precio_C, "Solo comparable C"),
  calcular_dano_comp((df_comp$precio_B + df_comp$precio_C) / 2, "Promedio B + C")
)

cat("\n--- Análisis de sensibilidad ---\n")
print(knitr::kable(sens_comp, format = "pipe", align = c("l", "r", "r")))

cat("\n========================================\n")
cat("  Capítulo 6 completado\n")
cat("========================================\n")
