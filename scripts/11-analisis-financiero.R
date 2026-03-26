# =============================================================================
# Capítulo 11: Análisis financiero-contable del daño
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
cat("  Capítulo 11: Análisis financiero\n")
cat("  Margin squeeze y test AEC\n")
cat("========================================\n\n")

# =============================================================================
# 1. SIMULACIÓN DE DATOS
# =============================================================================

set.seed(1111)

n <- 60
fecha <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = n)
t <- 1:n
inicio_conducta <- as.Date("2021-07-01")

ingresos_victima <- 1000 + 8 * t + 40 * sin(2 * pi * t / 12) + rnorm(n, 0, 15)
coste_insumo <- 350 + 4 * t + 15 * sin(2 * pi * t / 12) + rnorm(n, 0, 8)
squeeze <- if_else(fecha >= inicio_conducta, 80 + 0.6 * (t - 30), 0)
coste_insumo_obs <- coste_insumo + squeeze
otros_costes <- 400 + 2 * t + rnorm(n, 0, 10)

margen_victima <- (ingresos_victima - coste_insumo_obs - otros_costes) / ingresos_victima
margen_cf <- (ingresos_victima - coste_insumo - otros_costes) / ingresos_victima
margen_sector <- 0.22 + 0.001 * t / n + rnorm(n, 0, 0.012)

ds_margin_squeeze <- tibble(
  fecha = fecha, ingresos = round(ingresos_victima, 2),
  coste_insumo = round(coste_insumo, 2), coste_insumo_obs = round(coste_insumo_obs, 2),
  squeeze = round(squeeze, 2), otros_costes = round(otros_costes, 2),
  margen_victima = round(margen_victima, 4), margen_cf = round(margen_cf, 4),
  margen_sector = round(margen_sector, 4),
  conducta = if_else(fecha >= inicio_conducta, 1, 0)
)

write_csv(ds_margin_squeeze, "data/ds_margin_squeeze.csv")
cat("[OK] Dataset: data/ds_margin_squeeze.csv\n")
cat("     Obs:", n, "| Inicio conducta:", format(inicio_conducta), "\n\n")

# =============================================================================
# 2. REGRESIÓN DE MÁRGENES
# =============================================================================

modelo_margen <- lm(margen_victima ~ conducta + t + factor(format(fecha, "%m")),
                    data = ds_margin_squeeze)
beta_cond <- unname(coef(modelo_margen)["conducta"])
se_cond <- sqrt(vcov(modelo_margen)["conducta", "conducta"])

cat("--- Impacto sobre el margen ---\n")
cat("  Coef. conducta: ", round(beta_cond, 4), "\n")
cat("  Impacto (p.p.): ", round(beta_cond * 100, 2), "\n")
cat("  IC 95%:         [", round((beta_cond - 1.96 * se_cond) * 100, 2), ",",
    round((beta_cond + 1.96 * se_cond) * 100, 2), "] p.p.\n\n")

# =============================================================================
# 3. CUANTIFICACIÓN DEL DAÑO
# =============================================================================

ds_margin_squeeze <- ds_margin_squeeze |>
  mutate(
    beneficio_obs = ingresos * margen_victima,
    beneficio_cf  = ingresos * margen_cf,
    dano = if_else(conducta == 1, beneficio_cf - beneficio_obs, 0)
  )

resumen <- ds_margin_squeeze |> filter(conducta == 1) |>
  summarise(`Meses` = n(), `Margen obs.` = paste0(round(mean(margen_victima)*100,2),"%"),
            `Margen cf.` = paste0(round(mean(margen_cf)*100,2),"%"),
            `Daño total` = round(sum(dano),2))

cat("--- Daño por margin squeeze ---\n")
print(knitr::kable(resumen, format = "pipe"))
cat("\n")

# =============================================================================
# 4. GRÁFICOS
# =============================================================================

df_plot <- ds_margin_squeeze |>
  select(fecha, margen_victima, margen_cf, margen_sector) |>
  pivot_longer(-fecha, names_to = "serie", values_to = "margen")

g1 <- ggplot(df_plot, aes(x = fecha, y = margen, color = serie)) +
  annotate("rect", xmin = inicio_conducta, xmax = max(fecha),
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.06) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = inicio_conducta, color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("margen_victima"="#e74c3c","margen_cf"="#2c3e50","margen_sector"="#3498db"),
                     labels = c("Víctima (obs.)","Víctima (cf.)","Sector")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title="Margin squeeze: evolución de márgenes", x=NULL, y="Margen operativo", color=NULL) +
  theme_minimal(base_size=13) +
  theme(legend.position="bottom", panel.grid.minor=element_blank(), plot.title=element_text(face="bold"))

ggsave("output/fig_11_margenes.png", g1, width=9, height=5.5, dpi=150)
cat("[OK] output/fig_11_margenes.png\n")

g2 <- ggplot(ds_margin_squeeze |> filter(conducta==1), aes(x=fecha, y=dano)) +
  geom_col(fill="#c0392b", alpha=0.85) +
  labs(title="Daño mensual por compresión de márgenes", x=NULL, y="Daño (miles EUR)") +
  theme_minimal(base_size=13) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold"))

ggsave("output/fig_11_dano_mensual.png", g2, width=9, height=4.8, dpi=150)
cat("[OK] output/fig_11_dano_mensual.png\n")

cat("\n========================================\n")
cat("  Capítulo 11 completado\n")
cat("========================================\n")
