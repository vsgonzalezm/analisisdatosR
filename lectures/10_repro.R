########################################################################
# Clase 10 — Resultados
# Profesora: Valentina González Madariaga
# Fecha: 2025-12-10
# Objetivo: Reproducibles y comunicación 
########################################################################
library(rmarkdown)

# -------------------------------------------------------------------------
#  INSTALACIÓN Y CARGA DE LIBRERÍAS
# -------------------------------------------------------------------------

#install.packages(c("xaringan", "gapminder", "palmerpenguins", "rmarkdown", "tidyverse", "quarto"))
library(tidyverse)
library(palmerpenguins)
library(knitr)
library(xaringanthemer)


# -------------------------------------------------------------------------
# PRESENTACION XARINGAN 
# -------------------------------------------------------------------------

# EJERCICIO:
# 1. Ve a File > New File > R Markdown...
# 2. Elige "From Template" -> "Ninja Presentation"
# 3. Borra el contenido y prueba crear tus slides.

# Tip: Para poner imagenes desde internet en tu presentacion:
# knitr::include_graphics("https://url_de_la_imagen.jpg")


# -------------------------------------------------------------------------
# PARTE 2: REPORTES PARAMETRIZADOS (Gapminder)
# -------------------------------------------------------------------------

# PASO 1: CREAR LA PLANTILLA

# Crea un archivo nuevo: File > New File > R Markdown.
# Borra TODO su contenido y pega el siguiente bloque borrando # del principio 
# ponle de nombre plantilla_pais.Rmd:

# --- INICIO COPIA PLANTILLA ---

#---
#  title: "Reporte de Indicadores de Desarrollo"
#author: "Análisis Automatizado"
#date: "`r Sys.Date()`"
#output: 
#  html_document:
#  theme: flatly
#toc: true
#params:
#  pais: "Argentina"
#---
#  
#  ```{r setup, include=FALSE}
## Configuración global: Ocultamos el código para que el reporte final sea #limpio
#knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
#
## Cargamos las librerías necesarias
#library(tidyverse)
#library(gapminder)
#library(knitr)
#library(scales) # Para formatos bonitos de números ($ y comas)
#```
#
#
#```{r}
## 1. FILTRADO: Esta es la parte clave que conecta con el parámetro
#datos_pais <- gapminder %>% 
#  filter(country == params$pais)
#```
#
#
#```{r}
## 2. CÁLCULOS: Preparamos datos para el texto dinámico
## Obtenemos los datos del año más reciente disponible en gapminder
#datos_recientes <- datos_pais %>% 
#  filter(year == max(year))
#```
#
#
#```{r}
## Variables individuales para usar en el texto
#esperanza_vida_actual <- datos_recientes$lifeExp
#pib_actual <- datos_recientes$gdpPercap
#poblacion_actual <- datos_recientes$pop
#anio_dato <- datos_recientes$year
#```
#
#
### Introducción
#
#Este informe presenta la evolución histórica de los indicadores #socioeconómicos para R params$pais.
#
#Según los registros más recientes (año r anio_dato), el país cuenta con #una población de r comma(poblacion_actual) habitantes.
#
#El PIB per cápita registrado es de r dollar(pib_actual), mientras que la #esperanza de vida ha alcanzado los r round(esperanza_vida_actual, 1) años
#
#
### Evolución de la Salud (Esperanza de Vida)
#El siguiente gráfico muestra cómo ha aumentado la longevidad en r #params$pais desde 1952.
#
#```{r}
#ggplot(datos_pais, aes(x = year, y = lifeExp)) +
#  # Área sombreada para darle estilo
#  geom_area(fill = "#69b3a2", alpha = 0.4) +
#  # Línea principal
#  geom_line(color = "#69b3a2", size = 1.2) +
#  # Puntos para marcar cada medición
#  geom_point(color = "#404080", size = 2) +
#  # Etiquetas y tema
#  labs(
#    title = paste("Esperanza de Vida en", params$pais),
#    subtitle = "Evolución histórica (1952 - 2007)",
#    y = "Años de Vida",
#    x = "Año",
#    caption = "Fuente: Proyecto Gapminder"
#  ) +
#  theme_minimal() +
#  scale_y_continuous(limits = c(min(datos_pais$lifeExp) - 5, max#(datos_pais$lifeExp) + 5))
#```
#
#
### Desarrollo Económico
#
#A continuación observamos la tendencia del Producto Interno Bruto (PIB) #per cápita.
#```{r}
#ggplot(datos_pais, aes(x = year, y = gdpPercap)) +
#  geom_col(fill = "#404080", alpha = 0.8) +
#  geom_text(aes(label = dollar(round(gdpPercap))), vjust = -0.5, size = 3#) +
#  labs(
#    title = paste("Evolución del PIB per Cápita en", params$pais),
#    y = "PIB per Cápita (USD)",
#    x = "Año"
#  ) +
#  theme_minimal() +
#  scale_y_continuous(labels = dollar)
#```
#
### Datos Históricos
#Detalle completo de los registros utilizados para este análisis.
#
#
#```{r}
#datos_pais %>% 
#  select(Año = year, `Esperanza de Vida` = lifeExp, `Población` = pop, #`PIB per Cápita` = gdpPercap) %>% 
#  arrange(desc(Año)) %>% 
#  kable(format.args = list(big.mark = ","))
#```

# --- FIN COPIA PLANTILLA ---



# PASO 2: AUTOMATIZAR LA CREACIÓN

# Revisar tu directorio de trabajo para evitar errores 
print(paste("Directorio de trabajo actual:", getwd()))

# recuerda que la plantilla anterior debe estar guardada ahi 

# Prueba con un solo país
render(
  input = "plantilla_pais.Rmd",
  output_file = "Reporte_Prueba_Chile.html",
  params = list(pais = "Chile"),
  quiet = FALSE  # Lo dejamos en FALSE para ver si hay errores al tejer
)

# Revisa si se generó tu documento en la carpeta de trabajo. 

#---------------------------------------------------------------------
# Lista de países (Solo América )
lista_paises <- gapminder %>%
  filter(continent == "Americas") %>%
  pull(country) %>%
  unique() %>%
  as.character()

#---------------------------------------------------------------------
# Función Generadora
generar_un_reporte <- function(nombre_pais) {
  tryCatch({  
    render(
      input = "plantilla_pais.Rmd",
      output_file = paste0("Reporte_", make.names(nombre_pais), ".html"),
      params = list(pais = nombre_pais),
      quiet = TRUE
    )
    message(paste("OK:", nombre_pais))
  }, error = function(e) {
    message(paste("FALLÓ:", nombre_pais, "-", e$message))
  })
}

# Ejecutar
purrr::walk(lista_paises, generar_un_reporte)
# Revisa tu carpeta de trabajo para ver los reportes generados.

# ==============================================================================
# PARTE 3: QUARTO
# ==============================================================================

# 1. Ve a File > New File > Quarto Document.
# 2. Prueba el modo "Visual" (botón arriba a la izquierda del script).
# 3. Diseña laminas
# 4. Dale al botón "Render" para ver el resultado.

