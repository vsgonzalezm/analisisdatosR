########################################################################
# Clase 9 — Elección modelos 
# Profesora: Valentina González Madariaga
# Fecha: 2025-12-03
# Objetivo: Interpretación práctica de regresiones lineales y logísticas.
########################################################################

# -------------------------------------------------------------------------
#  INSTALACIÓN Y CARGA DE LIBRERÍAS
# -------------------------------------------------------------------------
# Usamos 'pacman' para gestionar paquetes de forma eficiente.
# Si no tienes pacman, se instala automáticamente.
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,       # Manipulación de datos y gráficos (ggplot2, dplyr)
  easystats,       # Conjunto de paquetes (report, performance, parameters)
  ggeffects,       # Para visualizar predicciones (Probabilidades Predichas)
  marginaleffects, # Para calcular efectos marginales 
  titanic,         # Datos del Titanic
  gapminder,       # Datos para ejercicio lineal
  carData,         # Datos para ejercicio logístico
  kableExtra,      # Tablas bonitas 
  qqplotr
)

# -------------------------------------------------------------------------
# PREPARACIÓN DE DATOS (TITANIC)
# -------------------------------------------------------------------------
data("titanic_train")

# Seleccionamos y limpiamos las variables de interés
df_titanic <- titanic_train %>%
  select(Survived, Pclass, Age, Fare, Sex) %>%
  mutate(
    Sex = as.factor(Sex),           # Convertir a factor (categoría)
    Pclass = as.factor(Pclass),     # Convertir a factor
    Survived = as.factor(Survived)  # Convertir a factor (0=No, 1=Sí)
  ) %>%
  drop_na() # Eliminamos filas con datos faltantes para evitar errores

glimpse(df_titanic) # Vistazo rápido a los datos

# =========================================================================
# REGRESIÓN LINEAL
# =========================================================================

# --- Modelo Simple ---
model_lin <- lm(Fare ~ Age, data = df_titanic)

# Interpretación automática
report(model_lin) # Resumen interpretativo del modelo

# Ver coeficientes 
model_parameters(model_lin)
# (Intercept): Precio base teórico cuando edad = 0.
# Age: Cuánto sube el precio por cada año extra de vida.

# Bondad de Ajuste (R2)
r2(model_lin)
# Nos dice qué % de la varianza del precio explica la edad.


# --- Visualización ---

ggplot(df_titanic, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.5, color = "grey") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1.5) + # El modelo
  labs(
    title = "Regresión Lineal Simple",
    subtitle = "Relación entre Edad y Precio del Boleto",
    x = "Edad (Años)",
    y = "Precio (Libras)"
  ) +
  theme_minimal()


# =========================================================================
#  ¿Y si controlamos por la Clase Social?
# =========================================================================

# Ajustamos modelo con Edad + Clase
model_multi <- lm(Fare ~ Age + Pclass, data = df_titanic)

# Comparación de modelos 
# Rank = TRUE nos ordena del mejor al peor
compare_performance(model_lin, model_multi, rank = TRUE)
# Nota: Mira el 'Adjusted R2' y el 'AIC'.

# Ver los nuevos coeficientes (Ceteris Paribus)
model_parameters(model_multi)
# Ahora 'Age' es el efecto controlado por clase.
# 'Pclass2' y 'Pclass3' son diferencias respecto a la Pclass1 (Base).


# Diagnóstico 
diagnostico <-check_model(model_multi)


# Diagnostico visual de supuestos  
plot(check_model(model_multi, check = c("linearity", "homogeneity"))) ## Linealidad y Homocedasticidad



# =========================================================================
# REGRESIÓN LOGÍSTICA (LOGIT)
# =========================================================================

# Ajustamos modelo (family = "binomial" es la clave)
model_logit <- glm(Survived ~ Sex + Pclass + Age, 
                   data = df_titanic, 
                   family = "binomial")
print(model_logit)

# Interpretación de Coeficientes 

# A) Log-Odds (Difíciles de interpretar, solo ver signo)
model_parameters(model_logit)

# B) Odds Ratios 
# exponentiate = TRUE convierte log-odds a OR
model_parameters(model_logit, exponentiate = TRUE)
# OR > 1: Aumenta chance (Riesgo/Éxito)
# OR < 1: Disminuye chance (Protector)

#  Probabilidades Predichas 
# ¿Cuál es la probabilidad exacta para diferentes perfiles?

# Gráfico de probabilidad por Edad y Sexo
plot(ggpredict(model_logit, terms = c("Age", "Sex"))) +
  labs(title = "Probabilidad Predicha de Sobrevivir", y = "Probabilidad (0-1)")

# Efectos Marginales 
# ¿En cuántos puntos porcentuales cambia la probabilidad?

# Average Marginal Effects (AME)
avg_slopes(model_logit)
# Interpretación columna 'estimate':
# Si dice 0.50 en Sexfemale, ser mujer aumenta la probabilidad en un 50%.
# Si dice -0.18 en Pclass3, ser de 3ra clase baja la probabilidad en un 18%.

# ---  Bondad de Ajuste Logística ---
r2(model_logit) # Mira Tjur's R2
performance(model_logit) # AIC, BIC




