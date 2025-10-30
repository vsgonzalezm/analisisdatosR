########################################################################
# Clase 4 — Análisis de datos con R
# Profesora: Valentina González Madariaga
# Fecha: 2025-10-23
# Objetivo: Joins
########################################################################


# Cargar librerías
library(tidyverse)
library(nycflights13)
library(here)

##############################
# 0) Proyecto y rutas 
##############################
# Verificar ubicación del proyecto
here::here()

# Ver datos nycflights13
data(package = "nycflights13")

# Para revisar en detalle que significa cada variable de cada dataset
#https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf


##############################
# 1) Cargar y armar objeto
##############################
# Cargamos cada tabla en su propio objeto para que quede claro que son distintas

vuelos <- flights
aerolineas <- airlines
aeropuertos <- airports
aviones <- planes
clima <- weather

# Entender que nycflights13 es un conjunto de tablas, no una sola.


##############################
# 2) Explorar 
##############################

glimpse(vuelos)

# Usa count() para ver cuántos vuelos hay de cada 'carrier'
vuelos %>% count(carrier, sort = TRUE)


##############################
# Ejercicios
##############################

# Ahora explora la tabla 'aerolineas'
# ¿Cuál es la traducción para "UA", "AA","DL"?
glimpse(aerolineas)
print(aerolineas)

# Explora 'aeropuertos' y 'aviones'
# ¿Qué columnas podrían servir como "claves" para conectarlas con 'vuelos'?
glimpse(aeropuertos)
glimpse(aviones)

##############################
# 2b) Filtrar 
##############################

# Filtrando vuelos 
vuelos_filtrado <- vuelos%>%
  select(year, month, day, hour, 
         dep_delay, arr_delay, carrier, 
         tailnum, origin, dest, distance)

# Filtrando aeropuertos
aerop_filtrado <- aeropuertos %>%
  select(faa, name)

# Filtrando aviones - solo variables esenciales
aviones_filtrado<- aviones %>%
  select(tailnum, year, manufacturer, model)

# Filtrando clima 
clima_filtrado <- clima %>%
  select(origin, year, month, day, hour, 
         temp, wind_speed, precip, humid)

##############################
# 3) Introducción Joins
##############################

# Una clave es una columna que nos permite identificar y conectar filas entre diferentes tablas (como carrier)

# Tipos de joins
#inner_join(): La intersección.
#left_join(): Todo de la tabla izquierda.
#right_join(): Todo de la tabla derecha.
#full_join(): La unión completa.



##############################
# 4) inner_join
##############################

##############################
# Ejemplo Guiado: inner_join()
##############################

#Vamos a unir la información de los vuelos con la información de los aviones. #La clave que los conecta es `tailnum` (el número de cola del avión).

# Unimos vuelos con aviones. Solo quedarán los vuelos cuyo avión exista en la tabla de aviones.

vuelos_aviones_inner <- vuelos_filtrado %>%
  inner_join(aviones_filtrado, by = "tailnum")

# ¿Cuántos vuelos quedan?
nrow(vuelos_filtrado)   # Vuelos originales
nrow(vuelos_aviones_inner) # Vuelos después del inner_join

# Perdimos todos los vuelos cuyo tailnum no estaba en la tabla aviones_filtrado.

##############################
# Ejercicio. 
##############################

# 1. Usa inner_join para unir la tabla vuelos_filtrado con la tabla aerolineas
# 2. La clave que las conecta es la columna carrier.
# 3. Guarda el resultado en un objeto llamado vuelos_aerolineas_inner.
# 4. Compara el número de filas de vuelos_filtrado con el de tu nuevo objeto. # 5. ¿Perdiste algún vuelo? ¿Por qué podría pasar?

#Pista:  
nombre_objeto <- vuelos_filtrado %>%
  inner_join(aerolineas, by = "carrier")

# Ejecuta tu código aquí

vuelos_aerolineas_inner <- vuelos_filtrado %>%
  inner_join(aerolineas, by = "carrier")


##############################
# 5) left_join
##############################

##############################
# Ejemplo Guiado: left_join()
##############################

# Unimos todos los vuelos con la info de aviones que tengamos
vuelos_aviones_left <- vuelos_filtrado %>%
  left_join(aviones_filtrado, by = "tailnum")

# ¿Cuántos vuelos quedan?
nrow(vuelos_filtrado)   # Vuelos originales
nrow(vuelos_aviones_left) # Vuelos después del left_join

##############################
# Ejercicio. 
##############################

# 1. Usa left_join() para unir la tabla vuelos_filtrado con la tabla aerop_filtrado.
# 2. La clave para unir es origin de vuelos y faa de aeropuertos. Debes especificarlo así: by = c("origin" = "faa")
# 3 Guarda el resultado en vuelos_aeropuertos_left.
# 4. Revisa las primeras filas con head(). 
# 5. ¿Qué pasa con los nombres de los aeropuertos?

#Pista:  
nombre_objeto <- vuelos_filtrado %>%
  left_join(aerop_filtrado, by = c("origin" = "faa"))

# Ejecuta tu código aquí

vuelos_aeropuertos_left <- vuelos_filtrado %>%
  left_join(aerop_filtrado, by = c("origin" = "faa"))


##############################
# 6) right_join
##############################

##############################
# Ejemplo Guiado: right_join()
##############################

# Unamos la tabla de aviones con la de vuelos. Queremos ver TODOS los aviones, aunque no hayan volado.

# Mantenemos TODOS los aviones de la tabla derecha
aviones_vuelos_right <- aviones_filtrado %>%
  right_join(vuelos_filtrado, by = "tailnum")

# Comparamos con el left_join que hicimos antes
identical(vuelos_aviones_left, aviones_vuelos_right)

##############################
# Ejercicio. 
##############################

# 1. Usa right_join() para unir la tabla aerolineas con vuelos_filtrado.
# 2. La clave es carrier.
# 3. Guarda el resultado en aerolineas_vuelos_right.
# 4. Ahora, haz lo mismo pero con left_join() en orden inverso: une vuelos_filtrado con aerolineas. Llama al resultado # vuelos_aerolineas_left.
# 5. Usa la función identical() para comprobar que ambos resultados son exactamente iguales.

#Pista:  
nombre_objeto <- aerolineas %>%
  right_join(vuelos_filtrado, by = "carrier")

# Ejecuta tu código aquí

aerolineas_vuelos_right <- aerolineas %>%
  right_join(vuelos_filtrado, by = "carrier")

vuelos_aerolineas_left <- vuelos_filtrado %>%
  left_join(aerolineas, by = "carrier")

##############################
# 7) full_join
##############################

##############################
# Ejemplo Guiado: full_join()
##############################

# Unamos la información de vuelos y aviones, pero esta vez queremos TODO: todos los vuelos y todos los aviones.

aviones_vuelos_full <- aviones_filtrado %>%
  full_join(vuelos_filtrado, by = "tailnum")
# Había que cambiar el orden era aviones primero 

# ¿Cuántas filas tenemos?
nrow(aviones_filtrado)      # Total aviones registrados
nrow(aviones_vuelos_full)   # Total tras la unión completa

# n de aviones 3322
# n total vuelos 336776

##############################
# Ejercicio.
##############################


# 1. Usa full_join() para unir vuelos_filtrado con aviones_filtrado por tailnum.
# 2. Filtra el resultado para encontrar los **vuelos que no tienen información del avión**.
#    Estos se reconocen porque tienen NA en las columnas que vienen de aviones_filtrado
#    (por ejemplo year.y o manufacturer).
# 3. ¿Cuántos vuelos no tienen información del avión?

# Ejecuta tu código aquí

vuelos_aviones_full <- vuelos_filtrado %>%
  full_join(aviones_filtrado, by = "tailnum")

# Filtramos los vuelos sin información del avión
vuelos_sin_info_avion <- vuelos_aviones_full %>%
  filter(is.na(year.y))

# Cuántos son
nrow(vuelos_sin_info_avion)
# alrededor de 57.000 vuelos nos falta informacion , esto nos sirve para entender que pueden haber coflictos entre bases de datos


##############################
# Armar una base  maestra 
##############################

# Paso 1: Unir vuelos con aerolíneas
base_maestra <- vuelos_filtrado %>%
  left_join(aerolineas, by = "carrier")

# Paso 2: Añadir información de aviones
base_maestra <- base_maestra %>%
  left_join(aviones_filtrado, by = "tailnum")

# Paso 3: Añadir nombre del aeropuerto de origen
base_maestra <- base_maestra %>%
  left_join(aerop_filtrado, by = c("origin" = "faa"))


# Al unir el paso 4 generaba error porque nos había ocurrido la duplicación de year, con .x y .y

# Por lo tanto primero asegurar nombres antes de unir 
base_maestra <- base_maestra %>%
  rename(
    year = year.x )

# Paso 4: Añadir información del clima
base_maestra <- base_maestra %>%
  left_join(clima_filtrado, by = c("origin", "year", "month", "day", "hour"))

#  Revisa el resultado
glimpse(base_maestra)



##############################
# Análisis y Visualización
##############################

# Antes normalizar nombres para que los análisis sean más fáciles
base_maestra <- base_maestra %>%
  rename(
    name = name.x,        # nombre de la aerolínea
    year_vuelo = year,  # año del vuelo
    year_avion = year.y   # año de fabricación del avión
  )


# PREGUNTA 1: ¿Qué aerolíneas tienen los peores retrasos?
# Sugerencia: Agrupa por 'name', calcula la media de 'dep_delay' y ordena de mayor a menor.
# na.rm = TRUE en mean()!

# Escribe tu código aquí:
retrasos_aerolineas <- base_maestra %>%
  group_by(name) %>%
  summarise(
    retraso_promedio = mean(dep_delay, na.rm = TRUE),
    n_vuelos = n()
  ) %>%
  arrange(desc(retraso_promedio))

head(retrasos_aerolineas)


# PREGUNTA 2: ¿Los aviones más antiguos se retrasan más?
# Sugerencia:
# 1. Filtra para quitar valores NA en 'year.y' (año de fabricación del avión).
# 2. Crea una nueva columna 'antiguedad' con mutate(). La fórmula es year.x - year.y.
# 3. Agrupa por 'antiguedad' y calcula la media de 'dep_delay'.



# Escribe tu código aquí:
# Calcular antigüedad del avión (año del vuelo - año de fabricación)
retrasos_antiguedad <- base_maestra %>%
  filter(!is.na(year_avion)) %>%
  mutate(antiguedad = year_vuelo - year_avion) %>%
  group_by(antiguedad) %>%
  summarise(
    retraso_promedio = mean(dep_delay, na.rm = TRUE),
    n_vuelos = n()
  ) %>%
  arrange(antiguedad)

head(retrasos_antiguedad)


# PREGUNTA 3: ¿El clima afecta los retrasos de salida?
# Sugerencia:
# 1. Filtra para quitar valores NA en 'dep_delay' y en las variables de clima ('temp', 'precip', etc.).
# 2. Agrupa por una variable de clima, por ejemplo, si llovía o no (precip > 0).
# 3. Calcula la media de 'dep_delay' para cada grupo.

# Escribe tu código aquí:

# Crear variable binaria: llovía (sí/no)
retraso_clima <- base_maestra %>%
  filter(!is.na(dep_delay), !is.na(precip)) %>%
  mutate(llovia = if_else(precip > 0, "Sí", "No")) %>%
  group_by(llovia) %>%
  summarise(
    retraso_promedio = mean(dep_delay, na.rm = TRUE),
    n_vuelos = n()
  )



# VISUALIZACIÓN:
# Para cualquiera de los análisis anteriores, crea un gráfico simple con ggplot2.
# Ejemplo para la pregunta 1:
# ggplot(nombre_de_tu_tabla, aes(x = reorder(name, retraso_promedio), y = retraso_promedio)) +
#   geom_col() +
#   coord_flip() +
#   theme_minimal()


