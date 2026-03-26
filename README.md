# Cuantificación de Daños Antitrust

## Microeconometría Aplicada a la Cuantificación de Daños por Competencia Desleal y Prácticas Anticompetitivas

**Autor:** Carlos de Anta Puig
Economista · Perito Financiero
Miembro del Colegio de Economistas de Madrid
Miembro del Instituto Español de Analistas Financieros (IEAF)
Profesor de Econometría y Microeconometría
carlos@cwconsultores.com

---

### Publicación

Este manual está publicado por **[Digital Reasons](https://www.digitalreasons.es)** (www.digitalreasons.es).

---

### Prólogo

Este manual nace de la práctica profesional. Tras años emitiendo informes periciales en procedimientos judiciales y arbitrales sobre competencia desleal y prácticas anticompetitivas, he constatado una carencia persistente: no existe, en lengua española, un texto que explique de forma rigurosa pero accesible cómo se cuantifican los daños económicos derivados de estas conductas.

Los manuales de referencia internacional y las guías institucionales de la Comisión Europea proporcionan un marco valioso, pero suelen estar escritos para economistas especializados. Los abogados que litigan estos casos necesitan entender qué hace el perito, por qué lo hace y qué significa el resultado sin necesidad de dominar toda la econometría subyacente. Los jueces y árbitros, por su parte, necesitan evaluar la solidez de un informe pericial sin tener que convertirse en estadísticos.

Este libro intenta cubrir esa brecha. Cada capítulo metodológico se estructura en tres capas: primero, una explicación intuitiva sin fórmulas; segundo, la formulación econométrica completa; tercero, la implementación en R con datos simulados, para que el lector pueda reproducir los análisis paso a paso.

No se trata solo de un manual de técnicas. Un buen informe pericial de daños exige construir un escenario contrafactual creíble, demostrar la causalidad entre la conducta y el perjuicio, aislar el efecto de la infracción de otros factores concurrentes y someter el resultado a análisis de sensibilidad rigurosos. Cada una de estas exigencias recorre el libro entero.

Si este manual consigue que un abogado entienda mejor lo que hay detrás de una cifra de daño, que un juez pueda valorar con más criterio la solidez de un informe pericial, o que un economista joven disponga de una guía útil para iniciarse en este campo, habrá cumplido su propósito.

*Carlos de Anta Puig — Madrid, marzo de 2026*

---

### Índice

**Parte I — Fundamentos**

1. Introducción a la cuantificación de daños económicos
2. Conductas ilícitas y su impacto económico
3. Causalidad, atribución y concurrencia de causas
4. Datos y evidencia para la cuantificación

**Parte II — Métodos de estimación del daño**

5. Método antes-y-después (*Before-and-After*)
6. Método de comparables (*Yardstick*)
7. Diferencias en diferencias (DID)
8. Estimación del sobreprecio (*Overcharge*)
9. El *passing-on* y la cadena de distribución
10. Estimación del lucro cesante (*Lost Profits*)

**Parte III — Métodos avanzados**

11. Análisis financiero-contable del daño
12. Endogeneidad y variables instrumentales
13. Datos de panel y efectos fijos
14. Control sintético
15. Regresión cuantílica y efectos heterogéneos

**Parte IV — Del modelo al informe pericial**

16. Actualización financiera y cálculo de intereses
17. Análisis de sensibilidad y robustez
18. Errores frecuentes en informes periciales de daños
19. Redacción y defensa del informe pericial

---

### Estructura del repositorio

- `data/` — Datasets simulados (CSV), reproducibles con semilla fija
- `scripts/` — Scripts R autocontenidos por capítulo (generan datos, análisis, tablas y gráficos)

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

---

*© Carlos de Anta Puig, 2026. Publicado por [Digital Reasons](https://www.digitalreasons.es). Todos los derechos reservados.*
