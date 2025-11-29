########################################################################
# Clase 8 — Regresión Logística
# Profesora: Valentina González Madariaga
# Fecha: 2025-11-26
# Objetivo: 
#   1. Ajustar modelos logísticos en R.
#   2. Interpretar Log-Odds, Odds Ratios y Probabilidades.
########################################################################

# ==============================================================
# 0) CARGAR LIBRERÍAS Y CONFIGURACIÓN
# ==============================================================
library(tidyverse)    # Manipulación de datos y gráficos
library(janitor)      # Limpieza de nombres
library(broom)        # Resultados ordenados
library(modelsummary) # Tablas de regresión profesionales
library(sjPlot)       # Gráficos de efectos marginales (Fundamental)
library(scales)       # Formato de porcentajes

options(scipen = 999) # Evitar notación científica


# Cargamos el dataset de Latinobarómetro 2023
# NOTA: Cambiamos la ruta para que lea directamente el archivo en la carpeta principal.
lb <- read.csv("data/lb_2023.csv") # Indicar ruta correspondiente al csv si no está en la carpeta principal

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

# Caso clase social percibida (s2: 1=Alta ... 5=Baja)
lb_subset <- lb_subset %>%
  mutate(s2 = ifelse(s2 < 1 | s2 > 5, NA, s2))

# ==============================================================
# PARTE 1: REGRESIÓN LINEAL (SIMULACIÓN)
# ==============================================================

# 1. Creamos datos ficticios: Consumo de Chocolate vs Peso
set.seed(123)
datos_chocolate <- tibble(
  chocolate_gr = runif(50, 0, 100),            # X: Consumo 0 a 100gr
  peso_kg = 55 + 0.1 * chocolate_gr + rnorm(50, 0, 2) # Y: Peso base 55 + efecto
)

# 2. Ajustamos el modelo lineal (lm)
modelo_lineal <- lm(peso_kg ~ chocolate_gr, data = datos_chocolate)

# 3. Interpretamos
summary(modelo_lineal)
# PREGUNTA A LA CLASE: 
# "Si el coeficiente es 0.1, ¿cuánto sube el peso por cada gramo extra de chocolate?"

# 4. Visualizamos la LÍNEA RECTA
ggplot(datos_chocolate, aes(x = chocolate_gr, y = peso_kg)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "La Lógica Lineal",
       subtitle = "A más X, más Y (constantemente)",
       x = "Chocolate (gr)", y = "Peso (kg)")

# ==============================================================
# PARTE 2: Filtrar 
# ==============================================================

lb_chile <- lb_subset %>%
  filter(pais_nombre == "Chile") 


# ==============================================================
# PARTE 3: Necesitamos variable binaria dependiente
# ==============================================================



lb_model <- lb_chile %>%
  # 1. VARIABLE DEPENDIENTE (Y): CONFIANZA EN EL CONGRESO
  # Original p13st_d: 1=Mucha, 2=Algo, 3=Poca, 4=Ninguna
  # Transformación: 1 (Confía: 1 y 2) vs 0 (No Confía: 3 y 4)
  mutate(confia_congreso = ifelse(p13st_d <= 2, 1, 0)) %>%
  
  # 2. VARIABLE INDEPENDIENTE (X): PERCEPCIÓN ECONÓMICA
  # Original p5stgbs: 1=Muy Buena ... 5=Muy Mala
  # INVERTIR para que sea intuitivo: 1=Mala ... 5=Buena
  mutate(econ_buena = 6 - p5stgbs) %>%
  
  # 3. Otras variables para después
  mutate(mujer = ifelse(sexo == 2, 1, 0),        # Dummy Mujer
         # 4. INVERSIÓN CLASE SOCIAL (s2)
         # Original: 1=Alta, 5=Baja. 
         # Inversión (6-x): 1 pasa a 5 (Alta), 5 pasa a 1 (Baja).
         # Objetivo: Que un coeficiente positivo signifique "Mayor Estatus".
         clase_alta = 6 - s2)                    

# Chequeo rápido
table(lb_model$confia_congreso)

# ==============================================================
# PARTE 4: Ejemplo
# ==============================================================
# Pregunta: ¿Mayor clase autopercibida aumenta la probabilidad de confiar en el Congreso?

# 1. Ajustar Modelo Logístico (GLM)
modelo_simple <- glm(confia_congreso ~ econ_buena, 
                     data = lb_model, 
                     family = binomial) # <--- OBLIGATORIO: family = binomial

# 2. Salida en Log-Odds (Difícil de leer)
summary(modelo_simple)
# Coeficiente econ_buena positivo (ej: 0.5): "Hay relación directa".

# 3. Conversión a Odds Ratios (Interpretación Real)
OR_simple <- exp(coef(modelo_simple)["econ_buena"])
print(OR_simple)

# INTERPRETACIÓN:
# "Por cada escalón que se sube en la clase social (ej. de Media a Media-Alta), los ODDS de confiar en el Congreso aumentan en un 89%."
# (Cálculo: 1.89 - 1 = 0.89 -> 89%)


# 4. Visualización 
plot_model(modelo_simple, type = "pred", terms = "econ_buena") +
  labs(title = "Probabilidad de Confiar en el Congreso",
       y = "Probabilidad (0-1)", 
       x = "Clase autpercibida (1=Baja -> 5=Alta)") +
  theme_minimal()

# ==============================================================
# PARTE 5: MODELO MULTIVARIADO
# ==============================================================
# ¿Se mantiene el efecto de la clase si controlamos por economía e ideología?

# NOTA METODOLÓGICA:
# - Ideología (p16st) tiene 10 niveles. La tratamos como numérica para ver la tendencia lineal (izquierda a derecha). Si la convirtiéramos a factor, tendríamos 10 coeficientes distintos.
# - Edad: Se mantiene continua.

modelo_full <- glm(confia_congreso ~ econ_buena + p16st + edad + mujer,
                   data = lb_model,
                   family = binomial)

# Tabla con Odds Ratios
modelsummary(modelo_full, 
             exponentiate = TRUE, # <--- ¡Clave! Convierte Log-Odds a OR
             stars = TRUE,  # Indica significancia
             statistic = "conf.int", # Intervalos de confianza
             title = "Determinantes Confianza Congreso")



# 1. ECON_BUENA (Percepción Económica)
#    - OR Obtenido: 1.750 (***)
#    - Cálculo: (1.750 - 1) * 100 = 75%
#    - Interpretación: "Manteniendo constante la ideología, edad y género, por cada punto 
#      que mejora la percepción de la economía (ej. de Mala a Regular), los ODDS de confiar 
#      en el Congreso aumentan en un 75%."

# 2. P16ST (Ideología: 0=Izq -> 10=Der)
#    - OR Obtenido: 1.043 
#    - Cálculo: (1.043 - 1) * 100 = 4.3%
#    - Interpretación: "Controlando por economía y sociodemográficos, moverse un paso 
#      hacia la derecha política aumenta los odds de confianza en un 4.3%."


# 3. EDAD (Continua)
#    - OR Obtenido: 0.996 (No significativo o marginal)
#    - Interpretación: "El OR es prácticamente 1 (0.996). Esto indica que la edad 
#      NO tiene un efecto sustantivo en la probabilidad de confiar, una vez que ya 
#      controlamos por economía e ideología."

# 4. MUJER (Dummy: 1=Mujer)
#    - OR Obtenido: 1.714 (**)
#    - Cálculo: (1.714 - 1) * 100 = 71.4%
#    - Interpretación: "Ser mujer AUMENTA los odds de confiar en el 
#      Congreso en un 71.4%, en comparación con un hombre de la misma edad, ideología y 
#      situación económica."


# Gráfico Forest Plot (Comparación de efectos)
# Si el punto está a la derecha de la línea roja (1), aumenta la confianza.
plot_model(modelo_full, 
           type = "est", # Efectos
           sort.est = TRUE,  # Ordenar por tamaño de efecto
           show.values = TRUE, # Mostrar valores
           vline.color = "red") +
  labs(title = "¿Qué variables pesan más?")


# ==============================================================
#    SECCIÓN DE EJERCICIOS 
# ==============================================================


# --------------------------------------------------------------
# EJERCICIO 1: Confianza en la iglesia
# --------------------------------------------------------------
# Contexto: La Iglesia (p13st_c) es una institución tradicional.
# Confianza por edad

# --- INSTRUCCIONES ---
# 1. Cree la variable binaria 'confia_iglesia' basada en 'p13st_c'.
#    (Recuerde: 1 y 2 = Confía, 3 y 4 = No Confía).
# 2. Corra un modelo logístico: confia_iglesia ~ edad.
# 3. Muestre la tabla de Odds Ratios.
# 4. Genere gráfico




lb_ejer <- lb_model %>%
  mutate(confia_iglesia = ifelse(p13st_c <= 2, 1, 0))

# Paso 2: Modelo
modelo_iglesia <- glm(confia_iglesia ~ edad, 
                      data = lb_ejer, 
                      family = binomial)

# Paso 3: Tabla OR
modelsummary(modelo_iglesia, exponentiate = TRUE, stars = TRUE, title = "Ejercicio 1")

# Paso 4: Gráfico
plot_model(modelo_iglesia,
           type = "pred",
           terms = "edad") +
  labs(title = "Edad y Confianza en la Iglesia", y = "Probabilidad")
# --------------------------------------------------------------
# EJERCICIO 2: Evasión
# --------------------------------------------------------------
# Contexto: Variable 'p22st': ¿Se justifica evadir impuestos? (1=Nunca...10=Siempre).
# Queremos ver qué factores predicen una conducta.

# --- INSTRUCCIONES ---
# 1. Cree la variable binaria 'justifica_evasion'. 
#    Criterio: Si p22st > 3 (Justifica algo o mucho) = 1. Si es 1-3 = 0.
# 2. Corra el modelo: justifica_evasion ~ confia_congreso + edad + .
# 3. Genere un "Forest Plot" para comparar los efectos.

# --- DESARROLLO Y SOLUCIÓN ---

# Paso 1: Mutate
lb_ejer <- lb_ejer %>%
  mutate(justifica_evasion = ifelse(p22st > 3, 1, 0))

# Paso 2: Modelo
modelo_evasion <- glm(justifica_evasion ~ confia_congreso + edad + econ_buena, 
                      data = lb_ejer, 
                      family = binomial)


# Paso 3: Gráfico Forest Plot
plot_model(modelo_evasion, type = "est", sort.est = TRUE, vline.color = "red") +
  labs(title = "¿Qué factores aumentan la justificación de evadir?")



# --------------------------------------------------------------
# EJERCICIO 3:Elige tu variable
# --------------------------------------------------------------
# Instrucciones:
# 1. Elija UNA variable de la base que no hayamos usado 
# 2. Transfórmela a binaria (decida su punto de corte: 1 vs 0).
# 3. Elija dos variables independientes (ej. sexo, educación, ideología).
# 4. Corra el modelo, haga el gráfico y escriba una frase de conclusión.