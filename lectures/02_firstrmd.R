########################################################################
# Clase 2 — Análisis de datos con R
# Profesora: Valentina González Madariaga
# Fecha: 2025-10-08
# Objetivo: Dplyr básico e Introducción R Markdown
########################################################################

# Cómo usar este script:
# 1) Abre SIEMPRE el .Rproj de tu proyecto del curso antes de correr el código.
# 2) Ejecuta cada bloque con Ctrl+Enter (o el botón "Run").

###############################################################################


##############################
# 0. Proyecto y rutas (lámina: Proyecto y rutas)
##############################

# Ver directorio de trabajo actual
getwd()

# Instalar 'here' solo si no lo tienes (quita el # y corre una vez)
# install.packages("here")

library(here)

# Muestra la ruta "raíz" del proyecto (donde está tu .Rproj)
here::here()

# Carpeta para resultados/exportaciones (si no existe, la crea)
dir.create(here::here("outputs"), showWarnings = FALSE)

#############################

#Opcion 1: No tengo un proyecto creado en mi pc  

# Crear un proyecto nuevo File > New Project... > New Directory > New Project.
# Guardar este script en el proyecto (File > New File > R Script > Guardar como...)
# Abrir el proyecto (.Rproj) en RStudio (doble click en el archivo .Rproj)
# Para corroborar aplicar paso 0

#############################

# Opción 2: Ya tengo un proyecto existente

#Abrir el proyecto primero
# Doble clic al archivo NOMBRE.Rproj (o en RStudio: File > Open Project... y seleccionan .Rproj).
# Traer el script al proyecto
#Si el script está en Descargas:

# Opción rápida: File > Open File... (abrirlo) y luego File > Save As... dentro de la carpeta del proyecto (p. ej., scripts/clase_1.R).
# O bien, arrastrar y soltar desde el explorador de archivos a la carpeta del proyecto (y luego en RStudio hacer More > Refresh en el panel Files).
# Para corroborar aplicar paso 0 


##############################
# 1. Operadores (lámina: Operadores)
##############################

# NUMÉRICOS
5 + 2    # suma
5 - 2    # resta
5 * 2    # multiplicación
5 / 2    # división
5 ^ 2    # potencia

# RELACIONALES (devuelven TRUE/FALSE)
3 == 3  # igualdad
3 != 2   # diferente
3 > 2; 3 >= 3 # mayor/mayor o igual
2 < 5; 2 <= 2  # menor/menor o igual

# LÓGICOS
TRUE & TRUE    # AND devuelve TRUE si ambos son TRUE 
TRUE | FALSE   # OR  TRUE si al menos uno es TRUE
!TRUE          # NOT: invierte TRUE/FALSE

# Son vectoriales analizan cada elemento 
c(TRUE, FALSE, TRUE) & c(FALSE, FALSE, TRUE)  # elemento a elemento
c(TRUE, FALSE, TRUE) | c(FALSE, FALSE, TRUE)  # elemento a elemento

# ÚTILES CON DATOS
c("A","B") %in% c("A","C")   # pertenencia
is.na(c(1, NA, 3))           # detectar NA

#ejemplo práctico:
asignatura <- c("R", "Python", "R", "Stata") # vector de asignaturas
asignatura %in% c("R","Python")  # pertenece a R o Python?

edad <- c(17, 18, 21, 30)
edad %in% 18:21   # pertenece a rango 18–21?
# [1] TRUE TRUE TRUE FALSE


# ÚTILES CON DATOS any y all
# ¿Alguna nota fuera de rango?

notas <- c(5.5, 6.2, 3.9, NA, 7.1)

any(notas < 1 | notas > 7, na.rm = TRUE)  # TRUE (hay un 7.1)
all(notas >= 1 & notas <= 7, na.rm = TRUE) # FALSE (por el 7.1)


##############################
# 2. Tidyverse y dplyr (lámina: Tidyverse y dplyr)
##############################

# Instalar paquetes solo una vez
# install.packages(c("readr","dplyr","ggplot2"))

library(readr)
library(dplyr)
library(ggplot2)
library(here)



##############################
# 3. Cargar datos 
##############################

UNpop <- readr::read_csv(
  "https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/UNpop.csv"
)

glimpse(UNpop)   # mirada rápida a columnas y tipos



##############################
# 4. Data Wrangling con dplyr 
#   Columnas: select()
#   Filas:    filter()
#   Ordenar:  arrange()
#   Crear:    mutate()
#   Resumir:  summarise()
#   Contar:   count()
#   Agrupar:  group_by()
#   Unir:     joins (left_join, inner_join, …)  [solo idea hoy]
#   Flujo:    pipes %>% (o |> )
##############################

# Usaremos SIEMPRE %>% en esta clase
`%>%` <- dplyr::`%>%`

  
  

##############################
# 5. select() — elegir columnas (lámina select)
##############################

UNpop %>%
  select(year, world.pop) %>%
  head()   # primeras 6 filas
# Nota: si pones select(world.pop, year) el orden cambia}
# Nota: si pones select(-year) excluye year (todas menos year)
# Nota: si pones select(starts_with("w")) selecciona columnas que empiezan con "w"
# Nota: si pones select(ends_with("pop")) selecciona columnas que terminan con "pop"


##############################
# 6. filter() — elegir filas (lámina filter)
##############################

UNpop %>%
  filter(year >= 1960) %>%
  head()
# Nota: si hay NA en year, no se incluyen (NA >= 1960 es NA, no TRUE)
# Nota: puedes combinar condiciones con & (AND) y | (OR)
# Ejemplo: filter(year >= 1960 & world.pop > 5000)
# Ejemplo: filter(year >= 1960 | world.pop > 5000) (más filas)
# Nota: para negar una condición, usa ! (NOT)



##############################
# 7. arrange() — ordenar filas (lámina arrange)
##############################

UNpop %>%
  arrange(desc(world.pop)) %>%
  head()
# Nota: por defecto ordena ascendente (menor a mayor)
# Nota: usa desc() para ordenar descendente (mayor a menor)
# Nota: puedes ordenar por varias columnas: arrange(col1, desc(col2), col3)
# Ejemplo: arrange(year, desc(world.pop)) (año asc, población desc)

UNpop %>%
  arrange(world.pop) %>%
  head()
##############################
# 8. mutate() — crear/transformar (lámina mutate)
##############################

UNpop %>%
  mutate(pop_billones = world.pop / 1000) %>%
  select(year, pop_billones) %>%
  head()
# Nota: puedes crear varias columnas a la vez
# Ejemplo: mutate(pop_billones = world.pop / 1000, pop_miles
# = world.pop / 1e6)
# Nota: puedes usar funciones como  sqrt(), .
# Ejemplo: mutate(raiz_pop = sqrt(world.pop))


##############################
# 9. summarise() — resúmenes (lámina summarise)
##############################

UNpop %>%
  summarise(
    min   = min(world.pop, na.rm = TRUE),
    max   = max(world.pop, na.rm = TRUE),
    media = mean(world.pop, na.rm = TRUE)
  )

# Nota: na.rm = TRUE es para ignorar NA en los cálculos
# Nota: puedes usar otras funciones como median(), sd(), var(), IQR(), etc.
# Ejemplo: summarise(mediana = median(world.pop, na.rm = TRUE), sd = sd(world.pop, na.rm = TRUE))
# Nota: si quieres el conteo total de filas, usa n()

##############################
# 10. count() — conteo rápido (lámina count)
##############################

# Ejemplo general (cuando haya variables categóricas):
# df %>% count(region)

# Con UNpop no aplica directamente (no hay "región").
#

##############################
# 11. group_by() + summarise() (lámina group_by + summarise)
##############################

# Ejemplo general:
# df %>% group_by(region) %>% summarise(n = n(), prom = mean(y, na.rm=TRUE))
# Con UNpop no aplica directamente (no hay "región").



##############################
# 12. joins — uniones (lámina joins - idea)
##############################

# left_join(x, y, by="id")   # conserva x, agrega columnas de y
# inner_join(x, y, by="id")  # solo coincidencias
# full_join(x, y, by="id")   # todo (x ∪ y)
# => Se verá en detalle en otra clase.



##############################
# 13. Pipes %>% — lectura “y luego” 
##############################

UNpop %>%
  filter(year >= 1960) %>%
  mutate(pop_billones = world.pop/1000) %>%
  arrange(year) %>%
  head()



##############################
# 14. Mini-ejercicio dplyr 
#   - años ≥ 1960
#   - crear 'crec' = world.pop - lag(world.pop)
#   - ordenar por 'crec' descendente y mostrar top 5 (year, crec)
##############################

# Hacer ejercicio 


##############################
# 15. ggplot2 básico (láminas: gramática mínima, aes/geom/theme)
##############################

# Gramática mínima:
# ggplot(datos, aes(...)) + geom_*() + labs() + theme_*()

# Serie temporal (línea) con UNpop
p_line <- ggplot(UNpop, aes(x = year, y = world.pop)) +  # datos + mapeo estético
  geom_line(linewidth = 0.9) +                         # grosor de línea
  labs(                                                 # etiquetas 
    title = "Población mundial (ONU)",                  # título
    x = "Año",                                         # eje x
    y = "Millones"                                     # eje y
  ) +
  theme_minimal()                                      # tema minimalista

p_line  # mostrar gráfico


#+ scale_y_continuous(
# labels = label_number(big.mark = ".", decimal.mark = ",")


##############################
# 16. Guardar imagen con ggsave() (lámina ggsave)
##############################

ggsave(
  filename = here::here("outputs", "unpop_line.png"),
  plot     = p_line,
  width    = 7,    # ancho en pulgadas
  height   = 4,    # alto en pulgadas
  dpi      = 150   # resolución: puntos por pulgada (96–150 pantalla, 300 impresión)
)



##############################
# 17. Variaciones rápidas (lámina: variantes geom/theme)
##############################

# Puntos
ggplot(UNpop, aes(year, world.pop)) +
  geom_point(alpha = .6, size = 2) +
  theme_minimal()

# Línea con otro tema y unidades en "billones"
ggplot(UNpop, aes(year, world.pop/1000)) +
  geom_line() +
  labs(y = "Billones") +
  theme_classic()



##############################
# 18. R Markdown — ¿qué es? (láminas: intro, crear Rmd)
##############################

# Pasos (en RStudio):
# 1) File > New File > R Markdown…
# 2) Elegir HTML > OK
# 3) Guardar como: my_first_rmd.Rmd
# 4) Botón "Knit"
# 5) Ver el HTML (Open in Browser si quieres)


##############################
# 19. YAML (lámina YAML) + crear plantilla Rmd 
##############################


---
title: "Mini-informe Clase 2"
author: "Nombre Apellido"
date: "`r format(Sys.Date(), \'%d-%m-%Y\')`"
output: html_document
# Opciones útiles:
# theme: cosmo
# toc: true
# number_sections: true
---
