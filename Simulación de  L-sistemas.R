# ==============================================================================
# L-sistema (sistema de Lindenmayer)
# Simulación de arquitectura vegetal mediante reescritura paralela de cadenas
# e interpretación geométrica de tortuga entre corchetes
# ==============================================================================

# Para simular el crecimiento de una planta por etapas. Empezamos con una
# cadena corta de texto llamada "axioma" (nuestro punto de partida) y, en
# cada ronda o iteración, revisamos todos sus símbolos al mismo tiempo para
# reemplazarlos según las "reglas" que hayamos definido. A ese proceso de ir
# transformando texto paso a paso se le llama "reescritura de cadenas", y que
# ocurra en todos los símbolos a la vez (reescritura en paralelo) es lo que hace
# que un L-sistema se parezca al desarrollo simultáneo de varias partes de una
# planta. Si un símbolo como '+' o '[' no tiene una regla propia, simplemente se
# queda tal como está en esa ronda.

# ==============================================================================

# PARTE 1: MOTOR DE REESCRITURA DE CADENAS EN PARALELO
# Qué hace esta parte: Toma una cadena inicial (axioma) y la transforma paso a
# paso aplicando reglas de sustitución de forma simultánea a todos los símbolos.
#
# Consiste en transformar una secuencia de símbolos (una cadena de caracteres)
# en otra nueva mediante reglas del tipo símbolo -> reemplazo. En lugar de
# calcular números con fórmulas, se manipulan símbolos que representan partes,
# estados o instrucciones.
#
# REESCRITURA PARALELA vs. SECUENCIAL:
# En las gramáticas formales clásicas la reescritura suele ser secuencial (se
# aplica o deriva por partes). En los L-sistemas es PARALELA: en cada iteración,
# TODOS los símbolos presentes en la cadena se reescriben a la vez, simulando
# el crecimiento o división simultánea de múltiples partes de un organismo.
# ==============================================================================

expandir_lsistema <- function(axioma, reglas, num_iteraciones, max_caracteres = 350000) {
  # Punto de partida del desarrollo: cadena inicial o axioma
  cadena <- axioma
  
  for (iteracion in seq_len(num_iteraciones)) {
    # Divide la cadena actual en un vector de caracteres individuales
    caracteres <- strsplit(cadena, "", fixed = TRUE)[[1]]
    
    # Sustituye en paralelo cada símbolo según 'reglas'. Si el símbolo no tiene
    # regla definida (por ejemplo '+', '-', '[' o ']'), se conserva tal cual.
    siguientes_caracteres <- vapply(
      caracteres,
      function(simbolo) if (!is.null(reglas[[simbolo]])) reglas[[simbolo]] else simbolo,
      character(1),
      USE.NAMES = FALSE
    )
    
    # Ensambla la nueva cadena resultante de este paso de reescritura
    cadena <- paste0(siguientes_caracteres, collapse = "")
    
    # Control de seguridad: la longitud de la cadena puede crecer de forma
    # exponencial en iteraciones altas y consumir mucha memoria
    if (nchar(cadena) > max_caracteres) {
      warning(sprintf(
        "Cadena truncada en iteración %d por superar %d caracteres.",
        iteracion,
        nchar(cadena)
      ))
      break
    }
  }
  
  cadena
}

# ==============================================================================
# PARTE 2: INTÉRPRETE DE TORTUGA 2D CON PILA DE RAMIFICACIÓN ('[' y ']')
# Qué hace esta parte: Traduce la cadena simbólica resultante de la
# reescritura en segmentos geométricos (coordenadas x, y) y registra la
# profundidad de ramificación de cada segmento.
#
# ¿QUÉ ES UN INTÉRPRETE DE TORTUGA?
# La cadena generada es solo texto simbólico; no es un dibujo por sí misma.
# Un intérprete de tortuga imagina un cursor o "tortuga" con posición (x, y)
# y ángulo de rumbo que lee la cadena símbolo por símbolo:
#   'F': avanza un paso dibujando un segmento visible.
#   'f': avanza un paso sin dibujar.
#   '+' / '-': gira el rumbo a la izquierda o a la derecha un ángulo fijo.
#   '[': guarda (apila) la posición y el ángulo actuales para iniciar una rama.
#   ']': recupera (desapila) el último estado guardado para volver al nudo
#     de ramificación y explorar otra rama.
# ==============================================================================

interpretar_tortuga <- function(cadena, angulo_grados, longitud_paso = 1.0, angulo_inicial_grados = 90) {
  caracteres <- strsplit(cadena, "", fixed = TRUE)[[1]]
  grados_a_radianes <- pi / 180
  
  # Estado inicial: base en (0, 0) y orientación vertical hacia arriba (90 grados)
  x_actual <- 0.0
  y_actual <- 0.0
  angulo_actual <- angulo_inicial_grados * grados_a_radianes
  giro_radianes <- angulo_grados * grados_a_radianes
  
  # Pila LIFO (último en entrar, primero en salir) para ramificaciones anidadas
  pila_estados <- vector("list", length = length(caracteres) + 1L)
  tope_pila <- 0L
  
  # Vectores preasignados para almacenar segmentos de forma eficiente en R
  max_segmentos <- length(caracteres)
  x_inicio <- numeric(max_segmentos)
  y_inicio <- numeric(max_segmentos)
  x_fin <- numeric(max_segmentos)
  y_fin <- numeric(max_segmentos)
  profundidad_rama <- integer(max_segmentos)
  es_dibujable <- logical(max_segmentos)
  indice_seg <- 0L
  
  for (simbolo in caracteres) {
    if (simbolo == "F" || simbolo == "f") {
      # Calcula el extremo final del paso según la posición y el ángulo vigentes
      x_nuevo <- x_actual + longitud_paso * cos(angulo_actual)
      y_nuevo <- y_actual + longitud_paso * sin(angulo_actual)
      
      indice_seg <- indice_seg + 1L
      x_inicio[indice_seg] <- x_actual
      y_inicio[indice_seg] <- y_actual
      x_fin[indice_seg] <- x_nuevo
      y_fin[indice_seg] <- y_nuevo
      
      # El nivel de corchetes abiertos ('tope_pila') refleja la profundidad de rama
      profundidad_rama[indice_seg] <- tope_pila
      es_dibujable[indice_seg] <- (simbolo == "F")
      
      x_actual <- x_nuevo
      y_actual <- y_nuevo
    } else if (simbolo == "+") {
      # Rota en sentido antihorario
      angulo_actual <- angulo_actual + giro_radianes
    } else if (simbolo == "-") {
      # Rota en sentido horario
      angulo_actual <- angulo_actual - giro_radianes
    } else if (simbolo == "[") {
      # Guarda posición y orientación actuales para abrir una rama lateral
      tope_pila <- tope_pila + 1L
      pila_estados[[tope_pila]] <- c(x_actual, y_actual, angulo_actual)
    } else if (simbolo == "]") {
      # Restaura el último estado guardado para regresar al punto de bifurcación
      if (tope_pila > 0L) {
        estado_recuperado <- pila_estados[[tope_pila]]
        tope_pila <- tope_pila - 1L
        x_actual <- estado_recuperado[1]
        y_actual <- estado_recuperado[2]
        angulo_actual <- estado_recuperado[3]
      }
    }
  }
  
  # Devuelve una tabla vacía con la estructura correcta si no hubo segmentos
  if (indice_seg == 0L) {
    return(data.frame(
      x_inicio = numeric(0),
      y_inicio = numeric(0),
      x_fin = numeric(0),
      y_fin = numeric(0),
      profundidad_rama = integer(0)
    ))
  }
  
  # Conserva únicamente los trazos visibles generados por 'F'
  indices_validos <- seq_len(indice_seg)[es_dibujable[seq_len(indice_seg)]]
  
  data.frame(
    x_inicio = x_inicio[indices_validos],
    y_inicio = y_inicio[indices_validos],
    x_fin = x_fin[indices_validos],
    y_fin = y_fin[indices_validos],
    profundidad_rama = profundidad_rama[indices_validos]
  )
}

# ==============================================================================
# PARTE 3: NORMALIZACIÓN GEOMÉTRICA Y ARMADO DE CADA PANEL
# Qué hace esta parte: Ejecuta la reescritura y la tortuga para un modelo,
# ajusta su altura a 1.0, centra su eje horizontal en 0 y le asigna su
# etiqueta de panel.
#
# ¿POR QUÉ NORMALIZAR ESCALA Y CENTRO?
# Cada regla e iteración produce estructuras de distinto tamaño absoluto y
# desplazamiento lateral. Normalizar permite comparar con claridad la forma
# arquitectónica de cada copa sin que unas tapen o aplasten a otras.
# ==============================================================================

construir_panel <- function(etiqueta_panel, axioma, reglas, num_iteraciones, angulo_grados, desplazamiento_x = 0) {
  # 1) Reescritura paralela de la cadena
  cadena_expandida <- expandir_lsistema(
    axioma = axioma,
    reglas = reglas,
    num_iteraciones = num_iteraciones
  )
  
  # 2) Traducción geométrica mediante tortuga
  segmentos <- interpretar_tortuga(
    cadena = cadena_expandida,
    angulo_grados = angulo_grados,
    longitud_paso = 1.0,
    angulo_inicial_grados = 90
  )
  
  # Calcula altura total y centro horizontal de la estructura generada
  y_minimo <- min(c(segmentos$y_inicio, segmentos$y_fin))
  y_maximo <- max(c(segmentos$y_inicio, segmentos$y_fin))
  escala_altura <- max(y_maximo - y_minimo, 1e-9)
  x_centro <- mean(c(segmentos$x_inicio, segmentos$x_fin))
  
  # Normaliza coordenadas para que la altura sea 1.0 y quede centrada en x = 0
  segmentos$x_inicio <- ((segmentos$x_inicio - x_centro) / escala_altura) + desplazamiento_x
  segmentos$x_fin <- ((segmentos$x_fin - x_centro) / escala_altura) + desplazamiento_x
  segmentos$y_inicio <- (segmentos$y_inicio - y_minimo) / escala_altura
  segmentos$y_fin <- (segmentos$y_fin - y_minimo) / escala_altura
  
  # Fija el orden de aparición de los paneles en la gráfica
  segmentos$panel <- factor(
    etiqueta_panel,
    levels = c(
      "1. Porte esbelto (ramas alternas)",
      "2. Ramificacion abierta",
      "3. Copa densa / arbustiva"
    )
  )
  
  segmentos
}

# ==============================================================================
# PARTE 4: CONFIGURACIÓN DE TRES L-SISTEMAS Y CONSOLIDACIÓN DE DATOS
# Qué hace esta parte: Define tres L-sistemas con distintos axiomas,
# reglas, iteraciones y ángulos, une sus segmentos en una tabla y asigna un
# grosor de línea según la profundidad de ramificación.
#
# SÍMBOLOS DE DIBUJO ('F') vs. SÍMBOLOS DE CONTROL ('X'):
# En los paneles 2 y 3, 'X' actúa como símbolo auxiliar o variable de estado
# que organiza cómo se ramifica la cadena en pasos sucesivos, mientras que 'F'
# es el que genera los segmentos dibujables.
#
# GROSOR SEGÚN PROFUNDIDAD DE RAMA:
# A mayor 'profundidad_rama', más anidada está la bifurcación. Reducir
# gradualmente 'grosor_linea' ayuda a distinguir visualmente los ejes más
# internos de las ramificaciones más finas.
# ==============================================================================
panel_1 <- construir_panel(
  etiqueta_panel = "1. Porte esbelto (ramas alternas)",
  axioma = "F",
  reglas = list("F" = "FF-[-F+F+F]+[+F-F-F]"),
  num_iteraciones = 4,
  angulo_grados = 22.5,
  desplazamiento_x = 0
)

panel_2 <- construir_panel(
  etiqueta_panel = "2. Ramificacion abierta",
  axioma = "X",
  reglas = list(
    "X" = "F[+X][-X]FX",
    "F" = "FF"
  ),
  num_iteraciones = 5,
  angulo_grados = 25.7,
  desplazamiento_x = 0
)

panel_3 <- construir_panel(
  etiqueta_panel = "3. Copa densa / arbustiva",
  axioma = "X",
  reglas = list(
    "X" = "F-[[X]+X]+F[+FX]-X",
    "F" = "FF"
  ),
  num_iteraciones = 5,
  angulo_grados = 22.5,
  desplazamiento_x = 0
)

# Reúne los tres paneles en un único data.frame listo para graficar
datos_plantas <- rbind(panel_1, panel_2, panel_3)

# Calcula el grosor de cada segmento en función de su profundidad de ramificación
datos_plantas$grosor_linea <- pmax(0.22, 0.85 * (0.82 ^ datos_plantas$profundidad_rama))

# ==============================================================================
# PARTE 5: RENDERIZADO Y EXPORTACIÓN GRÁFICA CON GGPLOT2
# Qué hace esta parte: Construye la figura multipanel con proporción
# geométrica real, muestra el gráfico en pantalla y deja comentada la
# exportación vectorial en SVG.
#
# ¿POR QUÉ USAR coord_fixed(ratio = 1) SIN scales = "free_x"?
# 'coord_fixed(ratio = 1)' mantiene la misma escala física en los ejes x e y
# para que los ángulos de ramificación no se deformen. Combinar
# 'facet_wrap(..., scales = "free_x")' con una relación de aspecto fija puede
# provocar error en 'ggplot2', por lo que aquí se omite 'scales = "free_x"'.
# ==============================================================================

library(ggplot2)

grafico_lsistemas <- ggplot(datos_plantas) +
  geom_segment(
    aes(
      x = x_inicio,
      y = y_inicio,
      xend = x_fin,
      yend = y_fin,
      linewidth = grosor_linea
    ),
    color = "#3b4236",
    lineend = "round",
    show.legend = FALSE
  ) +
  scale_linewidth_identity() +
  facet_wrap(~ panel, nrow = 1) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Estructuras vegetales 2D generadas por L-sistemas",
    subtitle = "Reescritura paralela e interpretacion de tortuga",
    caption = "Basado en el formalismo de Lindenmayer y Prusinkiewicz"
  ) +
  theme_void(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10, color = "#2b2e28", margin = margin(b = 8)),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13, color = "#1f241d"),
    plot.subtitle = element_text(hjust = 0.5, size = 9.5, color = "#4d5449", margin = margin(b = 12)),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "#6b7367", margin = margin(t = 10)),
    plot.margin = margin(15, 15, 15, 15)
  )

# Muestra la figura en la ventana gráfica de R
print(grafico_lsistemas)

# Exportación opcional en formato vectorial SVG sin fondo:
# ggsave("l_sistemas_plantas.svg", plot = grafico_lsistemas, width = 10, height = 4.8, bg = "transparent")

# ==============================================================================
# BIBLIOGRAFÍA FUNDAMENTAL
# - Lindenmayer, A. (1968). Mathematical models for cellular interactions in
#   development. I & II. Journal of Theoretical Biology, 18(3), 280-315.
# - Prusinkiewicz, P., & Lindenmayer, A. (1990). The Algorithmic Beauty of
#   Plants. Springer-Verlag, New York. https://algorithmicbotany.org/papers/abop/abop.pdf
# - Rozenberg, G., & Salomaa, A. (1980). The Mathematical Theory of L Systems.
#   Academic Press.
# - Hanan, J. S. (1992). Parametric L-systems and their application to the
#   modelling and visualization of plants. PhD Thesis, University of Regina.
# - Vos, J., Marcelis, L. F. M., de Visser, P. H. B., Struik, P
