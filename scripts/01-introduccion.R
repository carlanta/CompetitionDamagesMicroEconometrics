# =============================================================================
# Capítulo 1: Introducción a la cuantificación de daños económicos
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
cat("  Capítulo 1: Introducción\n")
cat("  Escenario contrafactual y daño\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(42)
n <- 40
t <- 1:n
t_infraccion <- 20

# Escenario contrafactual (sin infracción): tendencia lineal + ruido
ventas_cf <- 100 + 2.5 * t + rnorm(n, 0, 3)

# Escenario factual (con infracción desde t=20): caída de ~18 unidades
ventas_f <- ventas_cf
ventas_f[t_infraccion:n] <- ventas_cf[t_infraccion:n] - 18 - rnorm(n - t_infraccion + 1, 0, 2)

ds_contrafactual <- tibble(
  periodo      = t,
  ventas_cf    = round(ventas_cf, 2),
  ventas_f     = round(ventas_f, 2),
  t_infraccion = t_infraccion
)

write_csv(ds_contrafactual, "data/ds_contrafactual.csv")
cat("[OK] Dataset guardado: data/ds_contrafactual.csv\n")
cat("     Observaciones:", nrow(ds_contrafactual), "\n")
cat("     Período de infracción: desde t =", t_infraccion, "\n\n")

# =============================================================================
# 2. CUANTIFICACIÓN DEL DAÑO
# =============================================================================
# El daño en cada período es la diferencia entre el contrafactual y lo observado.
# Solo se computa para el período posterior al inicio de la infracción.

ds_contrafactual <- ds_contrafactual |>
  mutate(
    dano = if_else(periodo >= t_infraccion, ventas_cf - ventas_f, 0)
  )

dano_total     <- sum(ds_contrafactual$dano)
dano_medio     <- mean(ds_contrafactual$dano[ds_contrafactual$periodo >= t_infraccion])
meses_afectados <- sum(ds_contrafactual$periodo >= t_infraccion)

cat("--- Cuantificación del daño ---\n")
cat("  Meses afectados:       ", meses_afectados, "\n")
cat("  Daño medio mensual:    ", round(dano_medio, 2), " miles EUR\n")
cat("  Daño total acumulado:  ", round(dano_total, 2), " miles EUR\n\n")

# =============================================================================
# 3. TABLA RESUMEN
# =============================================================================

resumen <- tibble(
  Concepto = c(
    "Períodos totales",
    "Inicio de la infracción (t)",
    "Meses afectados",
    "Ventas contrafactuales acumuladas (post)",
    "Ventas observadas acumuladas (post)",
    "Daño total estimado",
    "Daño medio mensual"
  ),
  Valor = c(
    n,
    t_infraccion,
    meses_afectados,
    round(sum(ds_contrafactual$ventas_cf[ds_contrafactual$periodo >= t_infraccion]), 2),
    round(sum(ds_contrafactual$ventas_f[ds_contrafactual$periodo >= t_infraccion]), 2),
    round(dano_total, 2),
    round(dano_medio, 2)
  )
)

cat("--- Tabla resumen ---\n")
print(knitr::kable(resumen, format = "pipe", align = c("l", "r")))
cat("\n")

# =============================================================================
# 4. GRÁFICO PRINCIPAL: contrafactual vs. factual
# =============================================================================

df_plot <- ds_contrafactual |>
  select(periodo, ventas_cf, ventas_f) |>
  pivot_longer(-periodo, names_to = "escenario", values_to = "ventas")

g1 <- ggplot() +
  geom_ribbon(
    data = ds_contrafactual |> filter(periodo >= t_infraccion),
    aes(x = periodo, ymin = ventas_f, ymax = ventas_cf),
    fill = "#e74c3c", alpha = 0.2
  ) +
  geom_line(
    data = df_plot,
    aes(x = periodo, y = ventas, color = escenario, linetype = escenario),
    linewidth = 1
  ) +
  geom_vline(xintercept = t_infraccion, linetype = "dashed", color = "grey40") +
  annotate("text", x = t_infraccion + 0.5, y = max(ventas_cf) + 3,
           label = "Inicio de la\ninfracción", hjust = 0, size = 3.5, color = "grey30") +
  annotate("text", x = 32, y = mean(c(ventas_cf[32], ventas_f[32])),
           label = "DAÑO", fontface = "bold", size = 4, color = "#c0392b") +
  scale_color_manual(
    values = c("ventas_cf" = "#2c3e50", "ventas_f" = "#e74c3c"),
    labels = c("Escenario contrafactual\n(sin infracción)",
               "Escenario factual\n(observado)")
  ) +
  scale_linetype_manual(
    values = c("ventas_cf" = "dashed", "ventas_f" = "solid"),
    labels = c("Escenario contrafactual\n(sin infracción)",
               "Escenario factual\n(observado)")
  ) +
  labs(
    title    = "Lógica del escenario contrafactual",
    subtitle = "El daño como diferencia entre lo que habría ocurrido y lo que ocurrió",
    x = "Período", y = "Ventas (miles EUR)",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

ggsave("output/fig_01_contrafactual.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_01_contrafactual.png\n")

# =============================================================================
# 5. TABLA DE TIPOS DE DAÑO (referencia del capítulo)
# =============================================================================

tipos_dano <- tibble(
  `Tipo de daño` = c(
    "Sobreprecio (overcharge)",
    "Lucro cesante (lost profits)",
    "Daño emergente",
    "Efecto volumen"
  ),
  `Conducta típica` = c(
    "Cárteles de precios, abuso explotativo",
    "Exclusión de mercado, denigración, imitación desleal",
    "Cualquier conducta con impacto patrimonial directo",
    "Cárteles (componente adicional al sobreprecio)"
  ),
  `Métodos principales` = c(
    "Comparables, antes-y-después, DID, regresión de precios",
    "Proyección de márgenes, comparables de rentabilidad",
    "Análisis contable, valoración de activos",
    "Estimación de elasticidad-precio, modelos de demanda"
  )
)

cat("\n--- Tipos de daño económico ---\n")
print(knitr::kable(tipos_dano, format = "pipe"))

cat("\n========================================\n")
cat("  Capítulo 1 completado\n")
cat("========================================\n")
