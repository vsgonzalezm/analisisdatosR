########################################################################
# Clase 7 — Script
# Profesora: Valentina González Madariaga
# Fecha: 2025-11-19
# Objetivo: Correlaciones, Regresión Lineal

########################################################################

# ==============================================================
# 0) Cargar librerías y datos
# =============================================================

library(tidyverse)	 # dplyr, ggplot2, readr, tidyr (El "paquete de paquetes")
library(janitor)	 # clean_names()
library(here)	 	 # rutas reproducibles
library(ggrepel)	 # etiquetas que no se superponen
library(scales)	 	 # formateo de ejes y reescalado
library(broom)	 	 # tidy/augment de modelos (útil para extraer resultados limpios)
library(modelsummary) # tablas de modelos (útil para resultados de varias regresiones)
library(sjlabelled) # para ver etiquetas de variables
library(sjPlot)	 	 # para gráficos de modelos
library(gapminder)

# Cargamos el dataset de Latinobarómetro 2023
# NOTA: Cambiamos la ruta para que lea directamente el archivo en la carpeta principal.
lb <- read.csv("lb_2023.csv") # Indicar ruta correspondiente al csv si no está en la carpeta principal

# ==============================================================
# Seleccionar variables clave y Limpieza
# ==============================================================
lb_subset <- lb %>%
  select(
    # ---------- IDENTIFICACIÓN Y SOCIODEMOGRÁFICAS ----------
    idenpa,		 # Identificación del país (códigos ISO)
    pais_nombre,	 # Nombre del país
    sexo,		 # Sexo
    edad,		 # Edad (en años)
    reeeduc_1,	 # Educación
    s18_a,		 # Situación ocupacional
    s2,		 # Clase social percibida
    
    # ---------- VARIABLES POLÍTICAS Y ECONÓMICAS ----------
    p16st,		 # Auto-ubicación ideológica (0 = izquierda ... 10 = derecha)
    p5stgbs,	 # Evaluación de la situación económica del país
    p6stgbs,	 # Evaluación del cambio económico en 12 meses
    p40stgbs,	 # Interés en política
    
    # ---------- CONFIANZA EN INSTITUCIONES ----------
    p13stgbs_a,	 # a. Fuerzas Armadas
    p13stgbs_b,	 # b. Carabineros/Policía
    p13st_c,	 # c. Iglesia
    p13st_d,	 # d. Congreso
    p13st_f,	 # f. Poder Judicial
    
    # ---------- ACTITUDES Y VALORES ----------
    p22st,		 # Justificación de evasión de impuestos (1=nada justificable – 10=totalmente)
    p61st		 # Aceptación de desigualdad (1=inaceptable – 10=aceptable)
  )

# Creamos un vector con los códigos de no respuesta más comunes:
na_codes <- c(-3, -2, -1)

# Aplicamos la limpieza de NAs sobre todas las columnas numéricas:
lb_subset <- lb_subset %>%
  mutate(across(where(is.numeric), ~ ifelse(.x %in% na_codes, NA, .x))) 

# Caso de educación (0 = sin dato)
lb_subset$reeeduc_1 <- ifelse(lb_subset$reeeduc_1 == 0, NA, lb_subset$reeeduc_1) 

# Caso autoid (97 = no sabe/no contesta)
lb_subset$p16st <- ifelse(lb_subset$p16st == 97, NA, lb_subset$p16st)	

# ==============================================================
# 1) Exploración y Estadísticos Descriptivos
# ==============================================================

# Calculo de estadísticos descriptivos para p22st (justificación evasión impuestos)
# Esto sirve para entender la Media, Mediana y Desviación Estándar de la variable clave
lb_subset %>%
  summarise(
    media = mean(p22st, na.rm = TRUE),
    mediana = median(p22st, na.rm = TRUE),
    d_e = sd(p22st, na.rm = TRUE), # desviación estándar
  )


## Varianza 
## Ejemplo en R

set.seed(123)   # para reproducibilidad

# Simular 30 notas entre 1.0 y 7.0
notas <- round(runif(30, min = 1, max = 7), 1)

# Estadísticos descriptivos
media_n   <- mean(notas)
mediana_n <- median(notas)
sd_n      <- sd(notas)
var_n     <- var(notas)

notas
media_n
mediana_n
sd_n
var_n

# ==============================================================
# 2) Correlación y Gráficos (Latinobarómetro: Ejemplos de CORRELACIÓN DÉBIL)
# ==============================================================

# Filtrar solo Chile 
lb_chile <- lb_subset %>%
  filter(pais_nombre == "Chile")

# --- EJEMPLO 1: Edad y evasión ---

# Calculamos el coeficiente 'r' de Pearson. use="complete.obs" ignora los NAs
cor_edad <- cor(lb_chile$edad, lb_chile$p22st,
                use = "complete.obs")




# --- EJEMPLO 2: Ideología y evasión ---
cor_ideologia <- cor(lb_chile$p16st, lb_chile$p22st,
                     use = "complete.obs")




# --- GRÁFICO 1: CORRELACIÓN DÉBIL (IDEOLOGÍA vs. EVASIÓN) ---
# Usamos un gráfico de dispersión para visualizar la correlación nula.
g_lb1 <- lb_chile %>%
  ggplot(aes(x = p16st, y = p22st)) +
  geom_point(alpha = 0.5, size = 1.5) + # alpha suaviza la superposición de puntos
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Añade la línea de regresión (casi plana)
  labs(
    title = "Ideología y Justificación de Evasión de Impuestos (Chile)",
    subtitle = paste("Correlación 'r' =", round(cor_ideologia, 4), "- Relación Nula"),
    x = "Ideología (0 = Izquierda, 10 = Derecha)",
    y = "Justificación Evasión (1 = Nada, 10 = Totalmente)"
  ) +
  theme_minimal()
print(g_lb1)

# INTERPRETACIÓN :
# 1. El coeficiente r = 0.01 está muy cerca de 0: la relación es nula.
# 2. La línea de regresión (roja) es casi horizontal: X no explica Y.



# ==============================================================
# 3) EJERCICIO: Regresión Lineal Simple 
# ==============================================================

# Queremos predecir EVASIÓN (Y) en función de IDEOLOGÍA (X)
m_ch1 <- lm(p22st ~ p16st, data = lb_chile)
summary(m_ch1)

# --- INTERPRETACIÓN DETALLADA DE LA SALIDA DE M.CH1 ---
# (Recordatorio: Este modelo es MUY malo, sirve para mostrar qué NO debe salir)

# 1. Coeficiente p16st (Estimate): 0.007752
#    * INTERPRETACIÓN: Por cada aumento de 1 unidad en Ideología (hacia la derecha), la Evasión aumenta solo 0.007752 unidades. **(Efecto prácticamente NULO)**.

# 2. P-value de p16st: 0.78
#    * INTERPRETACIÓN: Es **mayor que 0.05**. El coeficiente es **NO SIGNIFICATIVO**. No podemos afirmar que la ideología afecte realmente a la evasión.

# 3. Multiple R-squared: 9.624e-05
#    * INTERPRETACIÓN: El modelo explica solo el **0.0096%** de la variación en la Evasión. **(Modelo inservible)**.


# ==============================================================

# El dataset 'women' nos permite ver una CORRELACIÓN CASI PERFECTA.
data("women")
# head(women) # Dos variables: height (altura) y weight (peso)

# --- EXPLORACIÓN ---
cor_w <- cor(women$height, women$weight)
print(paste("Correlación Altura vs Peso (women):", round(cor_w, 4)))
# Resultado: 0.9955 

# --- GRÁFICO 2: CORRELACIÓN FUERTE ---
g_w1 <- women %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") + # Añade la línea de regresión con el error estándar
  labs(
    title = "Relación Lineal Simple: Altura y Peso (Dataset women)",
    subtitle = paste("Correlación 'r' =", round(cor_w, 4), "- Relación muy fuerte"),
    x = "Altura (pulgadas)",
    y = "Peso (libras)"
  ) +
  theme_minimal()
print(g_w1)

# INTERPRETACIÓN DEL GRÁFICO:
# 1. Los puntos están casi perfectamente alineados (alta correlación).
# 2. La línea de regresión resume la tendencia de los datos.


# --- MODELO DE REGRESIÓN SIMPLE (M.W1) ---

m_w1 <- lm(weight ~ height, data = women) # Queremos predecir el PESO (Y) en función de la ALTURA (X)
summary(m_w1)



# ==============================================================
# 4)  Correlación y Regresión Múltiple 
# ==============================================================
# Gapminder es ideal para ver cómo el PIB afecta la Esperanza de Vida.

# PREPARACIÓN Y EXPLORACIÓN
data("gapminder") 

# Filtramos solo el año más reciente para el análisis de corte transversal
gm_2007 <- gapminder %>%
  filter(year == 2007)

# Exploramos la distribución de las variables clave
# Histograma del PIB per cápita 

# Realiza el histograma


# Transformación Logarítmica (para normalizar la variable 'gdpPercap')
gm_2007 <- gm_2007 %>%
  mutate(log_gdp = log(gdpPercap))


#  EJERCICIOS DE CORRELACIÓN 

# --- CORRELACIÓN 1: EXP. DE VIDA vs. PIB (logarítmico) ---

# Esperamos una correlación POSITIVA y FUERTE: a más riqueza, más longevidad.

# Hacer el ejercicio



# --- CORRELACIÓN 2: EXP. DE VIDA vs. POBLACIÓN ---
# Esperamos una correlación MÁS DÉBIL: el tamaño de la población no debería afectar tanto.

# Hacer el ejercicio


# --- GRÁFICO 1: CORRELACIÓN FUERTE (lifeExp vs. log_gdp) ---



# INTERPRETACIÓN:



# REGRESIÓN SIMPLE (GM.S1): UN SOLO PREDICTOR
# Modelo: Queremos predecir la Esperanza de Vida (Y) con el Logaritmo del PIB (X)


#  REGRESIÓN MÚLTIPLE (GM.M2): MÁS DE UN PREDICTOR


# --- GRÁFICO 2: VISUALIZACIÓN DEL MODELO MÚLTIPLE ---
