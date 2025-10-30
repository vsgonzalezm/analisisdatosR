
########################################################################
# Clase 5 
# Profesora: Valentina González Madariaga
# Fecha: 2025-10-30
# Objetivo: Repaso y Visualización con ggplot2 
########################################################################

# ---------------------------------------------------------------------
# 0) Cargar librerías y datos
# ---------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(gapminder)
library(here)
library(ggthemes)
library(ggrepel)

# Cargar datos 
data("gapminder")

head(gapminder)


########################################################################
# 1) Nombres con janitor
########################################################################

gapminder <- clean_names(gapminder) # ver los nombres de las columnas

########################################################################
# 1) Tablas con janitor
########################################################################

gapminder %>% tabyl(continent)  

gapminder %>% tabyl(continent) %>% 
  adorn_percentages("col") %>%     # porcentajes por columna
  adorn_totals()                  # totales

gapminder %>%
  filter(continent == "Americas") %>%
  tabyl(country) %>%
  adorn_totals()


# Filtramos un año para simplificar
gap_2007 <- gapminder %>% filter(year == 2007)

# Creamos una tabla cruzada
gap_2007 <- gap_2007 %>%
  mutate(
    r_vida = case_when(
      life_exp < 60 ~ "Baja (<60)",
      life_exp < 75 ~ "Media (60–75)",
      TRUE ~ "Alta (≥75)"
    ) )

# Tabla cruzada simple
tabyl(gap_2007, continent, r_vida)

########################################################################
# 2) Joins
########################################################################


# Creamos una tabla con promedios por continente
continent_info <- gapminder %>%
  group_by(continent) %>%
  summarise(
    mean_lifeExp = mean(life_exp),
    mean_gdp = mean(gdp_percap)
  )

# Unimos la tabla original con la info agregada por continente
gap_join <- gapminder %>%
  left_join(continent_info, by = "continent")

# Ver resultado
glimpse(gap_join)


# Ejercicio ----


# Crea una tabla con población total por continente y año,
# únela con gapminder usando left_join() y llámala gap2.

pop_info <- gapminder %>%
group_by(continent, year) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = "drop")

gap2 <- gapminder %>%
  left_join(pop_info, by = c("continent", "year"))

########################################################################
# 3) Pivot longer / wider
########################################################################

# Pasar de formato ancho (wide) a largo (long)
gap_long <- gapminder %>%
  pivot_longer(
    cols = c(life_exp, gdp_percap),   # columnas que se transforman en filas
    names_to = "indicador",           # nueva columna con los nombres originales
    values_to = "valor"               # nueva columna con los valores
  )

# Volver de largo a ancho
gap_wide <- gap_long %>%
  pivot_wider(
    names_from = indicador,           # qué variable usará para crear nuevas columnas
    values_from = valor               # qué valores se asignarán a esas columnas
  )

#  EJERCICIO DE PIVOT ----
# Crear un pivot que muestre la evolución de población por continente
# en formato long.
# las variables a seleccionar son  pop, continent y year.


#Pista: 

pop_long <- gapminder %>%
   select(continent,year,pop) %>%
   pivot_longer(
     cols = pop ,
     names_to = "indicador",
     values_to = "valor"
   )



########################################################################
# 4) Visualización paso a paso con ggplot2
########################################################################

# 4.1 Lienzo vacío
gapminder %>%
  ggplot()
# --> Crea el espacio del gráfico, aún sin ejes ni puntos.



# 4.2 Mapeos estéticos
gapminder %>%
  ggplot(aes(x = gdp_percap, y = life_exp))
# --> Define qué variables van en el eje X y en el eje Y.

#gapminder %>%
#  ggplot(aes(x = life_exp, y = gdp_percap))
## --> Define qué variables van en el eje X y en el eje Y.


# 4.3 Capa geométrica (tipo de gráfico)
gapminder %>%
  ggplot(aes(x = gdp_percap, y = life_exp)) +
  geom_point()
# --> geom_point() crea un gráfico de dispersión.



# 4.4 Línea temporal
gapminder %>%
  ggplot(aes(x = year, y = life_exp, group = country)) + # agrupa por país eje horizontal → los años y eje vertical → la esperanza de vida
  geom_line(alpha = 0.5) # transparencia
# --> geom_line() conecta los valores de cada país a lo largo del tiempo
# 142 países × 12 años ≈ 1700 líneas


# Agregar color por continente para facilitar

#gapminder %>%
#ggplot(aes(x = year, y = life_exp, group = country, color = continent)) +
#geom_line(alpha = 0.4) +
#labs(title = "Tendencias de esperanza de vida por continente")




# 4.5 Colores y tamaños
gapminder %>%
  ggplot(aes(x = gdp_percap, y = life_exp, 
             color = continent, size = pop / 1e6)) + # tamaño en millones
  geom_point(alpha = 0.7) 
# --> color agrupa por continente; size ajusta el tamaño según población (en millones).




# 4.6 Etiquetas
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdp_percap, y = life_exp,
             label = country, color = continent)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, nudge_x = 2000)
# --> geom_text() agrega nombres de países sobre los puntos.



# 4.7 Escalas (otras opciones además de logarítmica)
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdp_percap, y = life_exp, color = continent)) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::comma) +  # muestra números con comas (más legible)
  labs(x = "PIB per cápita (US$)", y = "Esperanza de vida (años)")
# --> Se puede usar scale_x_continuous() para personalizar ejes sin usar logaritmos.
# --> Otras opciones: scale_y_reverse(), scale_x_sqrt(), scale_color_manual().



# 4.8 Títulos y etiquetas
gapminder %>%
  filter(year == 1982) %>%
  ggplot(aes(x = gdp_percap, y = life_exp, color = continent)) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Relación entre PIB per cápita y esperanza de vida",
    subtitle = "Fuente: Gapminder",
    x = "PIB per cápita (US$)",
    y = "Esperanza de vida (años)",
    color = "Continente"
  )



# 4.9 Línea de referencia
mean_life <- mean(gapminder$life_exp)
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x =gdp_percap,  y =life_exp, color = continent)) +
  geom_point(size = 3) +
  geom_hline(yintercept = mean_life, linetype = 2, color = "black") +
  labs(title = "Esperanza de vida global vs PIB per cápita (2007)")
# --> geom_hline() agrega una línea horizontal (promedio global).



# 4.10 Estilos con ggthemes
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes( x=gdp_percap, y = life_exp, color = continent, label = country)) +
  geom_point(size = 3) +
  labs(title = "PIB y esperanza de vida (tema Economist)") +
  theme_economist() +
  scale_color_economist()
# --> Cambia el estilo visual del gráfico.



# 4.11 Etiquetas repelidas (ggrepel)
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(gdp_percap, life_exp, color = continent, label = country)) +
  geom_point(size = 3) +
  geom_text_repel() +
  scale_x_continuous(labels = scales::comma) +
  theme_economist()
# --> geom_text_repel() evita superposición entre nombres de países.



# 4.12 Facets (comparar países)
gapminder %>%
  filter(country %in% c("Chile", "Argentina")) %>%
  ggplot(aes(x = year, y = life_exp, color = country)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~country) +
  labs(title = "Evolución de la esperanza de vida: Chile vs Argentina",
       x = "Año", y = "Esperanza de vida (años)")
# --> facet_wrap() crea un panel por país (útil para comparar grupos o categorías).



# 4.13 Gráfico final combinado
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(gdp_percap, life_exp,
             color = continent, label = country)) +
  geom_point(size = 3) +
  geom_text_repel() +
  scale_x_continuous(labels = scales::comma) +
  xlab("PIB per cápita (US$)") +
  ylab("Esperanza de vida (años)") +
  ggtitle("Relación entre PIB per cápita y esperanza de vida (2007)") +
  geom_hline(yintercept = mean(gapminder$life_exp),
             lty = 2, color = "darkgrey") +
  theme_economist()


########################################################################
# 5) Ejercicios finales
########################################################################

# Ejercicio 1:
# Construye un gráfico que muestre la evolución del PIB per cápita
# de Chile, Argentina, Brasil y México entre 1952 y 2007.
# Pistas:
# - Filtra esos países
# - Usa aes(x = year, y = gdp_percap, color = country)
# - geom_line() para trazar líneas
# - labs() para títulos y ejes


# Ejercicio 2:
# Muestra la esperanza de vida promedio por continente en 2007.
# Usa geom_col() (gráfico de barras).



# Ejercicio 3:
# Crea un gráfico de dispersión entre PIB per cápita y esperanza de vida
# para los países de América en 2007. Ajusta un color y tema diferente.

