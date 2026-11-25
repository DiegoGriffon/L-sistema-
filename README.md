
# L-sistema (sistema de Lindenmayer)

Script en **R** con `ggplot2` que simula arquitecturas vegetales 2D mediante **L-sistemas entre corchetes** (reescritura paralela de cadenas e intérprete de tortuga con pila de estados). Incluye comentarios didácticos paso a paso y bibliografía fundamental para entender cómo reglas simples generan patrones de ramificación.

## ¿Qué hace este script?

1. **Reescritura paralela de cadenas (`expandir_lsistema`):** Parte de una cadena inicial (**axioma**) y aplica reglas de sustitución de forma simultánea a todos los símbolos en cada iteración.
2. **Intérprete de tortuga con pila (`interpretar_tortuga`):** Traduce la cadena simbólica a segmentos geométricos en el plano $(x, y)$ mediante comandos de avance (`F`, `f`), giros (`+`, `-`) y ramificación (`[` guarda estado en pila, `]` restaura estado).
3. **Normalización de paneles (`construir_panel`):** Ajusta la altura de cada estructura a `1.0` y centra su eje horizontal en `0` para comparar distintos hábitos de crecimiento lado a lado.
4. **Comparación de tres arquitecturas:** Genera una lámina multipanel con tres modelos clásicos (`1. Porte esbelto (ramas alternas)`, `2. Ramificacion abierta` y `3. Copa densa / arbustiva`) variando axioma, reglas, iteraciones y ángulo de giro, con grosor decreciente según la profundidad de ramificación.
5. **Renderizado y exportación (`ggplot2`):** Dibuja los tres paneles con `coord_fixed(ratio = 1)` para conservar los ángulos reales e incluye una línea comentada para exportar en formato vectorial SVG sin fondo.

## Requisitos y uso

* **Lenguaje y paquete:** R y `ggplot2`.
* **Ejecución:** Abre el script en R o RStudio y ejecútalo completo (`source("...")` o línea por línea). Para guardar el gráfico en SVG, descomenta la línea con `ggsave(..., bg = "transparent")`.

## Bibliografía fundamental

* **Lindenmayer, A. (1968).** Mathematical models for cellular interactions in development. I & II. *Journal of Theoretical Biology*, 18(3), 280–315.
* **Prusinkiewicz, P., & Lindenmayer, A. (1990).** *The Algorithmic Beauty of Plants*. Springer-Verlag, New York. [https://algorithmicbotany.org/papers/abop/abop.pdf](https://algorithmicbotany.org/papers/abop/abop.pdf)
* **Rozenberg, G., & Salomaa, A. (1980).** *The Mathematical Theory of L Systems*. Academic Press.
* **Hanan, J. S. (1992).** Parametric L-systems and their application to the modelling and visualization of plants. *PhD Thesis*, University of Regina.
* **Vos, J., Marcelis, L. F. M., de Visser, P. H. B., Struik, P. C., & Evers, J. B. (Eds.). (2007).** *Functional-Structural Plant Modelling in Crop Production*. Springer.
