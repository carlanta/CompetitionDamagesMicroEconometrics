# =============================================================================
# Capítulo 2: Conductas ilícitas y su impacto económico
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
cat("  Capítulo 2: Conductas ilícitas\n")
cat("  Horizonte temporal y descomposición\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS: horizonte temporal del daño con persistencia
# =============================================================================

set.seed(123)
n <- 60
t <- 1:n
t_inicio <- 15
t_fin <- 40

# Contrafactual: tendencia + ruido
ventas_cf <- 200 + 1.5 * t + rnorm(n, 0, 4)

# Factual: caída durante infracción + persistencia exponencial
ventas_f <- ventas_cf
ventas_f[t_inicio:t_fin] <- ventas_cf[t_inicio:t_fin] - 25 - rnorm(t_fin - t_inicio + 1, 0, 2)

if (t_fin < n) {
  periodos_post <- (t_fin + 1):n
  recuperacion <- 25 * exp(-0.15 * (periodos_post - t_fin))
  ventas_f[periodos_post] <- ventas_cf[periodos_post] - recuperacion + rnorm(length(periodos_post), 0, 2)
}

ds_horizonte_temporal <- tibble(
  periodo   = t,
  ventas_cf = round(ventas_cf, 2),
  ventas_f  = round(ventas_f, 2),
  t_inicio  = t_inicio,
  t_fin     = t_fin
)

write_csv(ds_horizonte_temporal, "data/ds_horizonte_temporal.csv")
cat("[OK] Dataset guardado: data/ds_horizonte_temporal.csv\n")
cat("     Observaciones:", nrow(ds_horizonte_temporal), "\n")
cat("     Período infractor: t =", t_inicio, "a t =", t_fin, "\n")
cat("     Persistencia: t =", t_fin + 1, "a t =", n, "\n\n")

# =============================================================================
# 2. CUANTIFICACIÓN DEL DAÑO POR FASES
# =============================================================================

ds_horizonte_temporal <- ds_horizonte_temporal |>
  mutate(
    fase = case_when(
      periodo < t_inicio                    ~ "Pre-infracción",
      periodo >= t_inicio & periodo <= t_fin ~ "Infracción",
      TRUE                                   ~ "Persistencia"
    ),
    dano = ventas_cf - ventas_f
  )

resumen_fases <- ds_horizonte_temporal |>
  filter(fase != "Pre-infracción") |>
  group_by(fase) |>
  summarise(
    Meses          = n(),
    `Daño medio`   = round(mean(dano), 2),
    `Daño total`   = round(sum(dano), 2),
    .groups = "drop"
  )

dano_total <- sum(ds_horizonte_temporal$dano[ds_horizonte_temporal$fase != "Pre-infracción"])

cat("--- Daño por fases ---\n")
print(knitr::kable(resumen_fases, format = "pipe", align = c("l", "r", "r", "r")))
cat("\n  Daño total (infracción + persistencia):", round(dano_total, 2), "miles EUR\n\n")

# =============================================================================
# 3. GRÁFICO: horizonte temporal con zonas de infracción y persistencia
# =============================================================================

g1 <- ggplot(ds_horizonte_temporal) +
  annotate("rect", xmin = t_inicio, xmax = t_fin, ymin = -Inf, ymax = Inf,
           fill = "#e74c3c", alpha = 0.08) +
  annotate("rect", xmin = t_fin, xmax = n, ymin = -Inf, ymax = Inf,
           fill = "#f39c12", alpha = 0.08) +
  geom_line(aes(x = periodo, y = ventas_cf, color = "Contrafactual"),
            linetype = "dashed", linewidth = 0.9) +
  geom_line(aes(x = periodo, y = ventas_f, color = "Factual"), linewidth = 0.9) +
  annotate("text", x = (t_inicio + t_fin) / 2, y = max(ventas_cf) + 6,
           label = "Período infractor", size = 3.3, fontface = "bold", color = "#c0392b") +
  annotate("text", x = (t_fin + n) / 2, y = max(ventas_cf) + 6,
           label = "Persistencia", size = 3.3, fontface = "bold", color = "#e67e22") +
  scale_color_manual(values = c("Contrafactual" = "#2c3e50", "Factual" = "#e74c3c"),
                     labels = c("Sin infracción", "Observado")) +
  labs(
    title    = "Horizonte temporal del daño",
    subtitle = "Los efectos pueden persistir más allá del cese de la conducta ilícita",
    x = "Período", y = "Ventas (miles EUR)", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fig_02_horizonte_temporal.png", g1, width = 9, height = 5.5, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_02_horizonte_temporal.png\n")

# =============================================================================
# 4. GRÁFICO: descomposición del daño (sobreprecio + efecto volumen)
# =============================================================================
# Curva de demanda lineal estilizada para ilustrar áreas A y B

p_comp    <- 60    # Precio competitivo
p_cartel  <- 78    # Precio cartelizado (~30% sobreprecio)
q_comp    <- 100   # Cantidad a precio competitivo
q_cartel  <- 72    # Cantidad a precio cartelizado

slope     <- (p_cartel - p_comp) / (q_cartel - q_comp)
intercept <- p_comp - slope * q_comp
q_range   <- seq(30, 130, length.out = 200)
p_demand  <- intercept + slope * q_range

df_demand <- tibble(q = q_range, p = p_demand)

g2 <- ggplot() +
  # Área A: sobreprecio
  annotate("rect", xmin = 0, xmax = q_cartel, ymin = p_comp, ymax = p_cartel,
           fill = "#e74c3c", alpha = 0.25) +
  # Área B: efecto volumen
  geom_polygon(
    data = tibble(q = c(q_cartel, q_comp, q_cartel),
                  p = c(p_comp, p_comp, p_cartel)),
    aes(x = q, y = p), fill = "#3498db", alpha = 0.25
  ) +
  geom_line(data = df_demand, aes(x = q, y = p), color = "#2c3e50", linewidth = 1.2) +
  geom_hline(yintercept = p_comp, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = p_cartel, linetype = "dashed", color = "#c0392b") +
  geom_segment(aes(x = q_cartel, y = 0, xend = q_cartel, yend = p_cartel),
               linetype = "dotted", color = "grey50") +
  geom_segment(aes(x = q_comp, y = 0, xend = q_comp, yend = p_comp),
               linetype = "dotted", color = "grey50") +
  annotate("text", x = q_cartel / 2, y = (p_comp + p_cartel) / 2,
           label = "A\n(Sobreprecio)", fontface = "bold", size = 4, color = "#c0392b") +
  annotate("text", x = (q_cartel + q_comp) / 2 + 2, y = p_comp + 5,
           label = "B\n(Efecto\nvolumen)", fontface = "bold", size = 3.5, color = "#2980b9") +
  annotate("text", x = 2, y = p_cartel + 2, label = "Precio cartel",
           hjust = 0, size = 3.2, color = "#c0392b") +
  annotate("text", x = 2, y = p_comp + 2, label = "Precio competitivo",
           hjust = 0, size = 3.2, color = "grey40") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 135),
                     breaks = c(q_cartel, q_comp),
                     labels = c(expression(Q[cartel]), expression(Q[comp]))) +
  scale_y_continuous(expand = c(0, 0), limits = c(30, 100),
                     breaks = c(p_comp, p_cartel),
                     labels = c(expression(P[comp]), expression(P[cartel]))) +
  labs(
    title    = "Descomposición del daño de un cártel",
    subtitle = "Sobreprecio (A) + efecto volumen (B) = daño total",
    x = "Cantidad", y = "Precio"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        plot.title = element_text(face = "bold"))

# Cuantificación numérica de las áreas
area_A <- (p_cartel - p_comp) * q_cartel
area_B <- 0.5 * (p_cartel - p_comp) * (q_comp - q_cartel)

cat("\n--- Descomposición numérica del daño ---\n")
cat("  Sobreprecio (A):    ", area_A, "\n")
cat("  Efecto volumen (B): ", area_B, "\n")
cat("  Daño total (A + B): ", area_A + area_B, "\n")
cat("  Ratio B/A:          ", round(area_B / area_A * 100, 1), "%\n\n")

ggsave("output/fig_02_sobreprecio_volumen.png", g2, width = 8, height = 6, dpi = 150)
cat("[OK] Gráfico guardado: output/fig_02_sobreprecio_volumen.png\n")

# =============================================================================
# 5. TABLA: mapa conducta-daño-método (referencia del capítulo)
# =============================================================================

mapa <- tibble(
  Conducta = c(
    "Cártel de fijación de precios",
    "Cártel de reparto de mercados",
    "Precios predatorios",
    "Margin squeeze",
    "Negativa de suministro",
    "Confusión / imitación desleal",
    "Denigración",
    "Violación de secretos",
    "Inducción a infracción contractual"
  ),
  `Tipo de daño principal` = c(
    "Sobreprecio + efecto volumen",
    "Sobreprecio + restricción de oferta",
    "Lucro cesante (exclusión)",
    "Lucro cesante (compresión de márgenes)",
    "Lucro cesante (exclusión)",
    "Lucro cesante (desviación clientela)",
    "Lucro cesante + daño reputacional",
    "Lucro cesante + pérdida ventaja competitiva",
    "Lucro cesante + daño emergente"
  ),
  `Métodos de referencia` = c(
    "Comparables, antes-y-después, DID, reg. precios (Caps. 5-8)",
    "Comparables geográficos, antes-y-después (Caps. 5-6)",
    "Proyección contrafactual, comparables rentabilidad (Caps. 5, 10)",
    "Análisis financiero-contable, test competidor eficiente (Cap. 11)",
    "Proyección contrafactual, control sintético (Caps. 5, 14)",
    "Antes-y-después, proyección de ventas (Caps. 5, 10)",
    "Antes-y-después, event study, comparables (Caps. 5-6)",
    "Regalías razonables, valoración intangibles (Cap. 10)",
    "Lucro cesante, coste de reemplazo (Cap. 10)"
  )
)

cat("\n--- Mapa conducta-daño-método ---\n")
print(knitr::kable(mapa, format = "pipe"))

cat("\n========================================\n")
cat("  Capítulo 2 completado\n")
cat("========================================\n")
