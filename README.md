# Cuantificación de Daños Antitrust

## Microeconometría Aplicada a la Cuantificación de Daños por Competencia Desleal y Prácticas Anticompetitivas

**Autor:** Carlos de Anta Puig  
Economista · Perito Financiero  
Miembro del Colegio de Economistas de Madrid  
Miembro del Instituto Español de Analistas Financieros (IEAF)  
Profesor de Econometría y Microeconometría  
carlos@cwconsultores.com

---

### Acceso al manual

- **Web (HTML):** [carlanta.github.io/CompetitionDamagesMicroEconometrics](https://carlanta.github.io/CompetitionDamagesMicroEconometrics/)
- **PDF:** disponible en la carpeta `docs/`

---

### Contenido

Manual universitario profesional de métodos microeconométricos para la cuantificación de daños derivados de conductas anticompetitivas y de competencia desleal.

**Parte I — Fundamentos (Caps. 1-4):** Contrafactual, conductas ilícitas, causalidad, datos y evidencia.

**Parte II — Métodos de estimación (Caps. 5-10):** Before-and-after, comparables (yardstick), diferencias en diferencias (DID), sobreprecio (overcharge), passing-on, lucro cesante (lost profits).

**Parte III — Métodos avanzados (Caps. 11-15):** Análisis financiero-contable, variables instrumentales (2SLS), datos de panel y efectos fijos, control sintético, regresión cuantílica.

**Parte IV — Del modelo al informe (Caps. 16-19):** Actualización financiera, análisis de sensibilidad, errores frecuentes, redacción y defensa del informe pericial.

---

### Estructura del repositorio

- `data/` — Datasets simulados (CSV), reproducibles con semilla fija
- `scripts/` — Scripts R autocontenidos por capítulo (generan datos, análisis, tablas y gráficos)
- `docs/` — Manual compilado (HTML para GitHub Pages + PDF)
- `*.qmd` — Código fuente de los capítulos (Quarto)
- `_quarto.yml` — Configuración del proyecto Quarto
- `references.bib` — Bibliografía

### Scripts

Cada script en `scripts/` genera el dataset, ejecuta el análisis completo del capítulo y produce tablas y gráficos. Para ejecutar:

```r
setwd("ruta/al/proyecto")
source("scripts/08-sobreprecio.R")
```

### Marco normativo de referencia

- Directiva 2014/104/UE (daños por infracciones del Derecho de la competencia)
- Guía Práctica de la Comisión Europea para la cuantificación del daño (2013)
- Jurisprudencia TJUE: Courage, Manfredi, Kone, Skanska

### Requisitos

- R >= 4.0
- Paquetes: `tidyverse`, `kableExtra`, `fixest`, `plm`, `AER`, `ivreg`, `quantreg`
- Quarto >= 1.3 (para compilar el libro)

---

*Carlos de Anta Puig, 2026. Todos los derechos reservados.*
