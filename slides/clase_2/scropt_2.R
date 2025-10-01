############################################################
# Clase 2 · dplyr + primer informe con R Markdown
# Autor: Valentina González Madariaga
# Fecha: 1/10/2025
# Objetivo: practicar verbos básicos de dplyr y preparar un
# mini-informe reproducible en R Markdown.
############################################################

# 0) Paquetes ---------------------------------------------------------------
# Si falta alguno: ejecutar en consola -> install.packages("nombre")
library(readr)
library(dplyr)
library(ggplot2)
library(here)

# 1) Diagnóstico de proyecto y carpetas ------------------------------------
getwd()                 # directorio actual
here::here()            # raíz del proyecto (debe apuntar al .Rproj)
dir.create(here("outputs"), showWarnings = FALSE)  # carpeta para exportar

# 2) Datos: UNpop -----------------------------------------------------------
# Lectura desde URL (alternativa: guardar CSV en data/)
UNpop <- readr::read_csv(
  "https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/UNpop.csv"
)

# Vista rápida
dplyr::glimpse(UNpop)
head(UNpop)

# 3) dplyr básico -----------------------------------------------------------
# select() + filter()
unpop_sel <- UNpop %>%
  dplyr::select(year, world.pop) %>%
  dplyr::filter(year >= 1960)

head(unpop_sel)

# mutate() + arrange()
unpop_mut <- unpop_sel %>%
  dplyr::mutate(pop_mill = world.pop / 1000) %>%
  dplyr::arrange(year)

head(unpop_mut)

# summarise()
unpop_sum <- unpop_sel %>%
  dplyr::summarise(
    media = mean(world.pop, na.rm = TRUE),
    minimo = min(world.pop, na.rm = TRUE),
    maximo = max(world.pop, na.rm = TRUE)
  )

unpop_sum

# 4) ggplot2 rápido ---------------------------------------------------------
p <- ggplot(UNpop, aes(x = year, y = world.pop)) +
  geom_line() +
  labs(
    title = "Población mundial (ONU)",
    x = "Año", y = "Población (millones)"
  ) +
  theme_minimal()

p

# Guardar en outputs/
ggsave(
  filename = here("outputs", "unpop_plot.png"),
  plot = p, width = 8, height = 4, dpi = 150
)

# 5) Mini-taller R Markdown -------------------------------------------------
# Instrucciones (para ejecutar fuera del script):
# - File -> New File -> R Markdown...
# - Título/autor -> OK, guardar como Clase2_apellido_nombre.Rmd
# - Dentro del Rmd, incluir:
#   * YAML mínimo con output: html_document
#   * Chunk setup con library(dplyr), library(ggplot2), library(readr)
#   * Un resumen de UNpop o swiss
#   * Un gráfico simple
#   * Un inline R (por ejemplo, promedio redondeado)

# 6) Datos: swiss (para el mini-informe) -----------------------------------
data(swiss)

# inspección
str(swiss)
summary(swiss)

# tabla de promedios (todas las columnas)
swiss_means <- swiss %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

swiss_means

# gráfico simple (dispersión)
g <- ggplot(swiss, aes(x = Education, y = Infant.Mortality)) +
  geom_point() +
  labs(
    title = "Educación y mortalidad infantil (swiss)",
    x = "Educación", y = "Mortalidad infantil"
  ) +
  theme_minimal()

g

# Exportar si se desea
ggsave(
  filename = here("outputs", "swiss_scatter.png"),
  plot = g, width = 6, height = 4, dpi = 150
)

# 7) Sugerencia de estructura para el Rmd ----------------------------------
# Secciones:
# - Título / autor / fecha (YAML)
# - Introducción breve (texto)
# - Datos y variables (2–3 líneas)
# - Resultados:
#     · Tabla (summary() o swiss_means)
#     · Figura (histograma o scatter)
#     · Inline R (ej: promedio redondeado con round())
# - Comentario final (1–2 oraciones)
#
# Checklist:
# - ¿Compila a HTML?
# - ¿Incluye 1 tabla + 1 figura + 1 inline?
# - ¿Ejes y títulos claros?
# - ¿Guardaste archivos en outputs/?

# 8) Desafíos (opcionales) --------------------------------------------------
# A) Añadir group_by() + summarise() en algún dataset
# B) Probar un histograma: ggplot(swiss, aes(Fertility)) + geom_histogram()
# C) Repetir análisis con UNpop filtrando por décadas

# Fin del script ------------------------------------------------------------
