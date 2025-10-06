########################################################################
# Clase 1 — Análisis de datos con R
# Profesora: Valentina González Madariaga
# Fecha: 2025-09-24
# Objetivo: primeros pasos en RStudio
########################################################################

# INSTRUCCIONES:
# - Ejecuta el script línea por línea (Ctrl+Enter en RStudio).
# - Si una línea instala un paquete, espera a que termine antes de seguir.
# - Abre el proyecto .Rproj  antes de empezar (recomendado).

# -----------------------
# 1) PAQUETES 
# -----------------------
# Vamos instalando/cargando paquete por paquete para que sea claro.

# 1.1 readr (para leer csv de forma fácil)
install.packages("readr")
library(readr)

# 1.2 dplyr (para manipular datos)
install.packages("dplyr")
library(dplyr)

# 1.3 ggplot2 (para gráficos)
install.packages("ggplot2")
library(ggplot2)

# 1.4 here (para rutas reproducibles dentro del proyecto)
 install.packages("here")
library(here)

# 1.5 haven (opcional: leer Stata/Sav)
  install.packages("haven")
library(haven)

# Nota: si no quieren instalar pueden comentar las líneas
# install.packages(...) y solo ejecutar library(...) si ya están instalados.

# -----------------------
# 2) OPCIONES Y SEMILLA
# -----------------------
# Ajustes básicos; no es obligatorio pero ayuda a reproducir resultados simples
set.seed(2025)

# -----------------------
# 3) DIRECTORIO / RUTA
# -----------------------
# Recomendado: abrir el .Rproj en RStudio.
# Aquí comprobamos la ruta actual y la ruta base de here()
print(getwd())       # muestra el directorio actual de la sesión
print(here::here())  # muestra la raíz del proyecto según here()

# Si por algún motivo necesitan cambiar (temporalmente) la ruta:
# setwd("C:/ruta/a/tu/proyecto")  # evítalo 

# -----------------------
# 4) CARGAR DATOS (OPCIÓN RECOMENDADA: URL)
# -----------------------
# Vamos a leer el archivo UNpop desde el repositorio del libro QSS.
UNpop_URL <- "https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/UNpop.csv"

# Leer el CSV (readr devuelve un tibble, que es fácil de usar)
UNpop <- readr::read_csv(UNpop_URL)

# Mirar el tipo del objeto y las primeras filas
print(class(UNpop))    # suele decir "tbl_df" "tbl" "data.frame"
print(head(UNpop))     # primeras 6 filas
dplyr::glimpse(UNpop)  # visión compacta de las columnas

# -----------------------
# 5) EJEMPLOS BÁSICOS DE OBJETOS
# -----------------------
# Números y enteros
a <- 3.14
b <- 5L
print(class(a))  # "numeric"
print(class(b))  # "integer"

# Vectores 
v_num <- c(1, 2, 3)
v_chr <- c("a", "b", "c")
v_mix <- c(1, "2", TRUE)  # observa cómo todo pasa a character
str(v_num)
str(v_chr)
str(v_mix)

# Factor (categoría)
f <- factor(c("bajo", "alto", "medio"))
print(f)
print(levels(f))

# Lógico (logical)
k <- 4 > 3
print(k)
print(class(k))

# -----------------------
# 6) SELECCIÓN DE FILAS/COLUMNAS (BASE VS DPLYR)
# -----------------------
# Base R: seleccionar primeras 3 filas
print(UNpop[1:3, ]) # todas las columnas

# Base R: seleccionar una columna
print(UNpop[, "world.pop"]) # por nombre

# dplyr (más legible): slice + select
UNpop %>%
  slice(1:3) %>%  # primeras 3 filas
  select(year, world.pop) %>% # solo columnas year y world.pop
  print() # mostrar resultado

# -----------------------
# 7) TRANSFORMAR Y RESUMIR (PIPE %>%)
# -----------------------
# 7.1 Crear una nueva variable simple
UNpop2 <- UNpop %>%
  mutate(world_millions = world.pop / 1000)  # dividir para ver en miles

# 7.2 Filtrar años 
UNpop2 <- UNpop2 %>%
  filter(year >= 1970)

# 7.3 Ver resultado
print(head(UNpop2))

# 7.4 Hacer un resumen simple (media)
resumen <- UNpop2 %>%
summarise(mean_world_millions = mean(world_millions, na.rm = TRUE))
print(resumen)

# -----------------------
# 8) SELECCIONES CON INDICES 
# -----------------------
# Cada otra fila (usar seq)
UNpop %>% slice(seq(1, n(), by = 2)) %>% print() # n() es número total de filas 

# Usar row_number() para filtrar filas impares
UNpop %>% filter(row_number() %% 2 == 1) %>% print() # filas impares

# -----------------------
# 9) GUARDAR RESULTADOS (outputs/)
# -----------------------
# Crear carpeta outputs si no existe
dir.create(here::here("outputs"), showWarnings = FALSE)

# Guardar CSV (fácil de subir o revisar luego)
readr::write_csv(UNpop2, here::here("outputs", "UNpop_clean.csv"))

# Guardar RDS (objeto R)
saveRDS(UNpop2, here::here("outputs", "UNpop_clean.rds"))



# -----------------------
# 10) GRÁFICO BÁSICO (ggplot2)
# -----------------------
# Hacemos un gráfico simple de year vs world.pop
p <- ggplot(UNpop, aes(x = year, y = world.pop)) +
  geom_line() +
  labs(title = "Población mundial (UNpop)", x = "Año", y = "World population") +
  theme_minimal()

print(p)  # muestra el gráfico en el panel Plots

# Guardar la imagen
ggsave(filename = here::here("outputs", "UNpop_plot.png"), plot = p, width = 7, height = 3.5)



# -----------------------
# 12) EJERCICIOS PARA HACER AHORA (PASO A PASO)
# -----------------------
# 1) Ejecuta: print(class(UNpop)) y dplyr::glimpse(UNpop)
# 2) Crea UNpop2 con world_millions = world.pop / 1000 y filtra year >= 1970
#    (ya está hecho en el script; revisar UNpop2)
# 3) Guarda UNpop2 en outputs/ (ya hecho). Verifica que el archivo exista.
# 4) Crea un vector mixto v_mix y usa str(v_mix) para explicar por qué cambió de tipo.
# 5) Genera el gráfico (ya hecho) y revisa outputs/UNpop_plot.png


