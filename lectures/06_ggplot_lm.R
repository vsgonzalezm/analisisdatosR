########################################################################
# Clase 6 — Script detallado con comentarios
# Profesora: Valentina González Madariaga
# Fecha: 2025-11-12
# Objetivo: Visualización (ggplot2) + regresión lineal (Latinobarómetro 2024)
########################################################################

# ==============================================================
# 0) Cargar librerías y datos
# =============================================================


library(tidyverse)  # dplyr, ggplot2, readr, tidyr
library(janitor)    # clean_names()
library(here)       # rutas reproducibles
library(ggrepel)    # etiquetas que no se superponen
library(scales)     # formateo de ejes y reescalado
library(broom)      # tidy/augment de modelos
library(modelsummary) # tablas de modelos
# paletas
library(wesanderson)
library(viridisLite)
library(RColorBrewer)
library(ggthemes)

# Cargamos el dataset de Latinobarómetro 2023
lb<-read.csv(here::here("data","lb_2023.csv"))# Indicar ruta correspondiente al csv


# ==============================================================
# Seleccionar variables clave 
# ==============================================================
lb_subset <- lb %>%
  select(
    # ---------- IDENTIFICACIÓN Y SOCIODEMOGRÁFICAS ----------
    idenpa,       # Identificación del país (códigos ISO)
    pais_nombre,  # Nombre del país
    sexo,         # Sexo (1. Hombre 2. Mujer)
    edad,         # Edad (en años)
    reeeduc_1,    # Educación (1. Analfabeto 2. Básica incompleta 3. Básica completa 4. Secundaria, media, técnica incompleta 5. Secundaria, media, técnica completa 6. Superior Incompleta 7. Superior completa 0. Sin dato)
    s18_a,         # Situación ocupacional (1. Independiente/cuenta propia 2. Asalariado en emp. pública 3. Asalariado en emp. privada 4. Temporalmente no trabaja 5. Retirado/pensionado 6. No trabaja/ responsable de las compras y el cuidado de la casa 7. Estudiante)
    s2,           # Clase social percibida (1. Alta, 2. Media Alta, 3. Media,4. Media Baja, 5. Baja,-2 y -1 borrar)
    
    # ---------- VARIABLES POLÍTICAS Y ECONÓMICAS ----------
    p16st,        #  # Auto-ubicación ideológica (0 = izquierda ... 10 = derecha, borrar 97,-2, -1)
    p5stgbs,      # Evaluación de la situación económica del país (1 = muy buena ... 5 = muy mala, -2 y -1 borrar)
    p6stgbs,      # Evaluación del cambio económico en 12 meses (1 = mucho mejor ... 5 = mucho peor, -2 y -1 borrar)
    p40stgbs,       # Interés en política (1 = muy interesado ... 4 = nada interesado, -2 y -1 borrar)
    
    # ---------- CONFIANZA EN INSTITUCIONES (mucha, algo, poco, ninguna) ----------
    p13stgbs_a,  # a.Fuerzas Armadas (-3, -2 y -1 borrar)
    p13stgbs_b,   # b. Carabineros/Policía ( -2 y -1 borrar)
    p13st_c,       # c. Iglesia ( -2 y -1 borrar)
    p13st_d,      # d.Congreso  ( -2 y -1 borrar)
    p13st_f,      # f. Poder Judicial ( -2 y -1 borrar)
    
    # ---------- ACTITUDES Y VALORES ----------
    p22st,        # Justificación de evasión de impuestos (nada justificable1–10 totalmente,-2 y -1 borrar )
    p61st         # Aceptación de desigualdad (totalmente inaceptable 1–10 tot. aceptable  -2 y -1 borrar
  )


# ==============================================================
# 1) Explorar 
# ==============================================================

# Detectar valores fuera de rango (ej: -1, -2, -3, 97, 98, 99, 0

#lb_subset(lb_subset %in% na_codes) <- NA

# Creamos un vector con los códigos de no respuesta más comunes:
na_codes <- c(-3, -2, -1)

# Aplicamos sobre todas las columnas numéricas:
lb_subset <- lb_subset %>%
  mutate(across(where(is.numeric), ~ ifelse(.x %in% na_codes, NA, .x))) # Reemplaza códigos NA aplica lo que viene dentro de los paréntesis a todas las columnas donde is.numeric sea TRUE,  ~ indica aplica lo siguiente a cada columna. Después indica si el valor de la celda está en na_codes, reemplázalo por NA; si no, déjalo igual

# Caso de educación (0 = sin dato)
lb_subset$reeeduc_1 <- ifelse(lb_subset$reeeduc_1 == 0, NA, lb_subset$reeeduc_1) # Reemplaza 0 por NA en reeeduc_1

# Caso autoid
lb_subset$p16st <- ifelse(lb_subset$p16st == 97, NA, lb_subset$p16st) 



# ==============================================================
# Ejercicios 
# ==============================================================

# Revisar la distribución de la variable "edad" 
# Contamos cuántas personas se identifican en cada clase social percibida (S2)
# Calculamos el promedio de auto-ubicación ideológica (0=izquierda ... 10=derecha)
# Evaluamos la variabilidad en la justificación de evasión de impuestos (P22ST)

#summary()
#count()
#mean()
#sd()


table(lb_subset$edad)


# ==============================================================
# 2) Análisis descriptivo
# ==============================================================

# Univariado 
# Histograma 
lb_subset %>%
  ggplot(aes(x = edad)) +         # definimos el eje X con la variable continua "edad"
  geom_histogram() +              # construimos un histograma con bins por defecto
  labs(title = "Distribución de edad", 
       x = "Edad (años)", 
       y = "Frecuencia") +        # añadimos etiquetas
  theme_minimal()                 # aplicamos tema limpio y claro

# Histograma mejoras visuales
lb_subset %>%
  ggplot(aes(x = edad)) +
  geom_histogram(binwidth = 5, # <-- NUEVA línea: ajustamos ancho de barras a 5 años
                 fill = "steelblue", # <-- NUEVA línea: color de relleno
                 color = "white") + # borde blanco
  labs(title = "Distribución de edad",
       x = "Edad (años)",
       y = "Frecuencia") +
  theme_minimal(base_size = 13) 

# Gráfico de densidad 

# Variable ideología política (0 = izquierda, 10 = derecha)
lb_subset %>%
  ggplot(aes(x = p16st)) +       # eje X: variable continua (escala 0–10)
  geom_density() +               # curva de densidad suavizada
  labs(title = "Densidad — Auto-ubicación ideológica",
       x = "Ideología (0 = izq, 10 = der)",
       y = "Densidad") +
  coord_cartesian(xlim = c(0,10)) +   # ajusta rango correcto del eje X
  theme_minimal()

# Gráfico de densidad con mejoras
lb_subset %>%
  ggplot(aes(x = p16st)) +
  geom_density(fill = "lightblue", alpha = 0.5) + # <-- NUEVA línea: color y transparencia
  geom_vline(aes(xintercept = mean(p16st, na.rm=TRUE)),
             color = "red", linetype = "dashed", size = 1) + # <-- NUEVA línea: media en rojo
  labs(title = "Distribución ideológica — Latinobarómetro 2023",
       subtitle = "Curva de densidad (0–10)",
       x = "Auto-ubicación ideológica",
       y = "Densidad") +
  coord_cartesian(xlim = c(0,10)) +
  theme_minimal()

# Bivariado 
# Scatterplot

lb_subset %>%
  ggplot(aes(x = edad, y = p22st)) + # X: edad, Y: evasión
  geom_point(alpha = 0.2, color = "grey40") + # puntos semi transparentes
  labs(title = "Relación entre edad y justificación de evasión",
       x = "Edad",
       y = "Evasión de impuestos (1–10)") +
  theme_minimal()


# Scatterplot con mejoras

lb_subset %>%
  ggplot(aes(x = edad, y = p22st)) +
  geom_jitter(alpha = 0.3, width = 0.4, height = 0.3, color = "steelblue") + # <-- NUEVA línea: dispersa puntos
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") + # <-- NUEVA línea: línea de tendencia
  labs(title = "Tendencia lineal: edad → evasión",
       subtitle = "Cada punto = persona entrevistada",
       x = "Edad",
       y = "Justificación de evasión (1–10)") +
  theme_minimal()


# Gráfico de barras
lb_subset %>%
  ggplot(aes(x = factor(s2))) + # convertimos a factor
  geom_col(stat="count") + # barras por frecuencia
  labs(title = "Clase social percibida",
       x = "Clase social (1=Alta ... 5=Baja)",
       y = "Frecuencia") +
  theme_minimal()


#Gráfico de barras con mejoras 
lb_subset %>%
  ggplot(aes(x = factor(s2))) +
  geom_col(stat="count", fill="darkorange", color="black") + # <-- NUEVA línea: color y borde
  labs(title = "Clase social percibida — estilo personalizado",
       x = "Clase social", y = "N de casos") +
  theme_bw() + # <-- NUEVA línea: tema diferente
  theme(axis.text=element_text(size=12))



# **Gráfico de lineas**

#Promedio de evasión por edad
lb_subset %>%
  group_by(edad) %>%
  summarise(prom_evasion = mean(p22st, na.rm = TRUE)) %>%
  ggplot(aes(x = edad, y = prom_evasion)) +
  geom_line(color = "grey40") +
  labs(title = "Promedio de evasión según edad",
       x = "Edad",
       y = "Promedio de justificación de evasión") +
  theme_minimal()


# **Gráfico de lineas**

#agrupar por tramos de edad 
p5b <- lb_subset %>%
  mutate(
    tramo_edad = cut(
      edad,
      breaks = seq(15, 95, by = 10),                   # cortes de 10 años
      labels = c("15–24","25–34","35–44","45–54",
                 "55–64","65–74","75–84","85–94"),     # etiquetas claras
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  group_by(tramo_edad) %>%
  summarise(prom_evasion = mean(p22st, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = tramo_edad, y = prom_evasion, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkorange", size = 2) +
  labs(title = "Promedio de evasión por tramo de edad",
       x = "Tramo de edad (años)",
       y = "Promedio de justificación de evasión (1–10)") +
  theme_minimal(base_size = 13)


# Paleta 

# library(wesanderson) # <-- NUEVA línea: cargamos paleta Wes Anderson
lb_subset %>%
  ggplot(aes(x=factor(s2), fill=factor(s2))) +
  geom_bar() +
  scale_fill_manual(values = wes_palette("Zissou1", 5, type = "discrete")) + # <-- NUEVA línea
  labs(title="Paleta Wes Anderson (Zissou1)", fill="Clase social") +
  theme_minimal()

# ==============================================================
# Ejercicios 
# ==============================================================

# Hacer algun gráfico con facets  (elegir paises)


# ==============================================================
# 3) Regresión Lineal 
# ==============================================================

# Teórico 

# --------------------------------------------------------------
# 0) Preparación: crear factores con etiquetas y seleccionar columnas
# --------------------------------------------------------------

# Creamos versiones factor de las variables categóricas con etiquetas claras
dat <- lb_subset %>%
  mutate(
    # Sexo: 1=Hombre, 2=Mujer  → factor con etiquetas
    sexo_f = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # Evaluación económica del país (1=Muy buena … 5=Muy mala) → factor ordenado
    econ_f = factor(p5stgbs, levels = 1:5,
                    labels = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"), ordered = TRUE)
)

# --------------------------------------------------------------
# 1) MODELO LINEAL SIMPLE: p22st ~ p16st
# --------------------------------------------------------------

# Filtramos los casos que tienen ambos valores observados (Y y X)
dat_m1 <- dat %>%
  filter(!is.na(p22st), !is.na(p16st))

# Ajustamos el modelo OLS:
m1 <- lm(p22st ~ p16st, data = dat_m1)  
# - Intercepto (β0): valor esperado de p22st cuando p16st=0 (extremo izquierda).
# - Pendiente  (β1): cambio promedio en p22st por +1 punto en ideología (ceteris paribus, aquí no hay más X).

# Resumen del modelo (coeficientes, errores estándar, t, p-valores, R²)
summary(m1)

g_m1b <- dat_m1 %>%
  ggplot(aes(x = p16st, y = p22st)) +
  geom_jitter(alpha = 0.25, width = 0.15, height = 0.15,
              color = "steelblue", size = 1.2) +  # dispersión con leve ruido visual
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  labs(title = "Modelo simple: p22st ~ p16st",
       subtitle = "Tendencia: a mayor ideología de derecha, mayor justificación de evasión (débil pero positiva)",
       x = "Ideología (0 = izquierda, 10 = derecha)",
       y = "Justificación de evasión (1–10)") +
  theme_minimal(base_size = 13)

print(g_m1b)

# ==============================================================
# Ejercicios  
# ==============================================================

# Generar regresión

