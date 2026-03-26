# =============================================================================
# Capítulo 13: Datos de panel y efectos fijos
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
library(fixest)

cat("\n========================================\n")
cat("  Capítulo 13: Panel y efectos fijos\n")
cat("  OLS vs. TWFE\n")
cat("========================================\n\n")

set.seed(1313)
n_mercados <- 8; n_periodos <- 48
tratados <- c("M1","M2","M3","M4")

df_panel <- expand_grid(mercado = paste0("M", 1:n_mercados), periodo = 1:n_periodos) |>
  mutate(
    fecha = as.Date("2019-01-01") %m+% months(periodo - 1),
    mercado_id = match(mercado, unique(mercado)),
    fe_mercado = c(-8,-3,0,4,-5,2,6,-2)[mercado_id],
    tratado = if_else(mercado %in% tratados, 1, 0),
    post = if_else(periodo > 24, 1, 0),
    efecto = if_else(tratado == 1 & post == 1, -10 - 0.15 * (periodo - 24), 0),
    precio = round(50 + fe_mercado + 0.4*periodo + 3*sin(2*pi*periodo/12) + efecto + rnorm(n(), 0, 2), 2)
  )

ds_panel <- df_panel |> select(fecha, mercado, tratado, post, precio)
write_csv(ds_panel, "data/ds_panel_mercados.csv")
cat("[OK] Dataset: data/ds_panel_mercados.csv\n")
cat("     Mercados:", n_mercados, "| Periodos:", n_periodos, "| Obs:", nrow(ds_panel), "\n\n")

fit_ols <- lm(precio ~ tratado * post, data = df_panel)
fit_fe <- feols(precio ~ tratado:post | mercado + periodo, data = df_panel)

cat("--- OLS vs. TWFE ---\n")
print(knitr::kable(tibble(
  Modelo = c("OLS agrupado", "TWFE"),
  Efecto = c(round(coef(fit_ols)["tratado:post"], 2), round(coef(fit_fe)["tratado:post"], 2))
), format = "pipe", align = c("l", "r")))

g1 <- df_panel |>
  group_by(fecha, grupo = if_else(tratado==1,"Tratados","Control")) |>
  summarise(precio = mean(precio), .groups = "drop") |>
  ggplot(aes(x = fecha, y = precio, color = grupo)) +
  annotate("rect", xmin=as.Date("2021-01-01"), xmax=max(df_panel$fecha), ymin=-Inf, ymax=Inf, fill="#e74c3c", alpha=0.06) +
  geom_line(linewidth=0.95) +
  geom_vline(xintercept=as.Date("2021-01-01"), color="grey50", linetype="dashed") +
  scale_color_manual(values=c("Tratados"="#e74c3c","Control"="#2c3e50")) +
  labs(title="Panel: tratados vs. control", x=NULL, y="Precio medio", color=NULL) +
  theme_minimal(base_size=13) + theme(legend.position="bottom", panel.grid.minor=element_blank(), plot.title=element_text(face="bold"))

ggsave("output/fig_13_panel.png", g1, width=9, height=5.5, dpi=150)
cat("\n[OK] output/fig_13_panel.png\n")

cat("\n========================================\n")
cat("  Capítulo 13 completado\n")
cat("========================================\n")
