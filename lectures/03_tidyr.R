########################################################################
# Clase 3 — Análisis de datos con R
# Profesora: Valentina González Madariaga
# Fecha: 2025-10-08
# Objetivo: Tidyverse II
########################################################################

#Instalación y carga de paquetes

#library("devtools") # if not installed, run install.packages("devtools")
#install_github("kosukeimai/qss-package", build_vignettes = TRUE) # if not installed


##############################
# 0) Proyecto y rutas 
##############################
# Abre SIEMPRE el .Rproj del curso antes de correr.
# Crea carpetas base si no existen.

# install.packages("here")      # si alguien no lo tiene
library(here)

here::here()
dir.create(here::here("data"),    showWarnings = FALSE)
dir.create(here::here("outputs"), showWarnings = FALSE)

##############################
# 1) Paquetes 
##############################
# install.packages(c("readr","dplyr","tidyr","ggplot2","janitor","readxl","haven","writexl"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(readxl)
library(haven)
library(writexl)

`%>%` <- dplyr::`%>%`   # Usaremos SIEMPRE  %>%

##############################
# 2) Dataset: QSS · social 
##############################
url_social <- "https://raw.githubusercontent.com/kosukeimai/qss/master/CAUSALITY/social.csv"

social <- readr::read_csv(url_social) %>%
  janitor::clean_names()   # nombres a snake_case

glimpse(social)

##############################
# 3) Diccionario 
##############################

dict <- tibble::tibble(
  Variable     = c("hhsize","messages","sex","yearofbirth","primary2004","primary2006"),
  Descripcion  = c(
    "Tamaño del hogar (nº de personas).",
    "Tratamiento: Control / Civic Duty / Hawthorne / Neighbors.",
    "Sexo: female / male.",
    "Año de nacimiento.",
    "Votó (1) o se abstuvo (0) en la primaria 2004.",
    "Votó (1) o se abstuvo (0) en la primaria 2006."
  )
)
print(dict)

##############################
# 4) Guardar en distintos formatos 
##############################

# Guardamos copias para que prueben otras importaciones:
readr::write_csv(social, here::here("data","social.csv"))
writexl::write_xlsx(social, here::here("data","social.xlsx"))
haven::write_dta(social, here::here("data","social.dta"))
haven::write_sav(social, here::here("data","social.sav"))

##############################
# 5) Cargar de distintos formatos
##############################
social_xlsx <- read_excel(here::here("data","social.xlsx")) 
social_dta  <- read_dta(here::here("data","social.dta"))      
social_sav  <- haven::read_sav(here::here("data","social.sav"))     


# Chequeos rápidos:
glimpse(social_xlsx)
glimpse(social_dta)
glimpse(social_sav)

##############################
# 6) janitor
##############################
# clean_names() ya aplicado arriba.

social %>%
  tabyl(messages) %>%
  adorn_totals("row") %>% # total general
  adorn_pct_formatting(digits = 1) # formato %


# Nuestra base está en formato wide originalmente 1 persona por fila. 

# Añadir identificador único por fila (ESTO FALTABA EN LA CLASE)

social <- social %>% mutate(id =row_number())   # id único por observación

# Mover id  para verlo siempre  primera columna
social <- social %>% relocate(id, .before = 1)          # id como primera columna


##############################
# 8) tidyr · pivot_longer 
##############################

# Convertir columnas año en filas: primary2004 y primary2006 -> anio, turnout

social_long <- social %>%
  pivot_longer(
    cols      = c(primary2004, primary2006),   # columnas a cambiar
    names_to  = "anio",                        # nombre de la nueva columna que guarda el nombre original
    values_to = "turnout"                      # nueva columna con el valor 0/1
  ) %>%
  
  # transformar el texto "primary2004" -> 2004
  mutate(anio = as.integer(gsub("primary", "", anio)))  # quita "primary" y pasa a entero - gsub = global substitution

# revisa si funciono


##############################
# 9) tidyr · pivot_wider - volver a ancho
##############################
social_wide <- social_long %>%
  pivot_wider(
    names_from   = anio,                         # usar los valores 2004/2006 como nombres
    values_from  = turnout,                      # los valores a distribuir en columnas
    names_prefix = "primary",                    # crea columnas primary2004, primary2006
    values_fill  = list(turnout = 0)             # rellenar NA con 0  
  )

##############################
# 10) Repaso dplyr 
##############################
# Recordatorio de verbos: select(), rename(), filter(), mutate(), case_when(),
# arrange(), group_by() + summarise(), count()



##############################
# 11) Ejercicio 1 — select() + rename() 
##############################
# Planteamiento:
# - Quedarse con: sex, messages, yearofbirth, hhsize, primary2004, primary2006
# - Renombrar yearofbirth -> yob
# Idea base :
# social1 <- social %>%
#   select( ... ) %>%
#   rename( ... )

social1 <- social %>%
select( ... ) %>%
rename( ... )



##############################
# 12) Ejercicio 2 — filter()  
##############################
# Planteamiento:
# - Quedarse con hogares de 2+ personas y yob >= 1960.
# Idea base :
# social2 <- social1 %>%
#   filter( ... , ... )


##############################
# 13) Ejercicio 3 — mutate() + case_when() 
##############################
# Planteamiento:
# - Crear edad = 2006 - yob.
# - Recodificar messages -> mensaje (español: Vecinos, Deber cívico, Hawthorne, Control).
# Idea base :
# social3 <- social2 %>%
#   mutate(
#     edad = ...,
#     mensaje = case_when(
#       messages == "Neighbors"  ~ "Vecinos",
#       messages == "Civic Duty" ~ "Deber cívico",
#       messages == "Hawthorne"  ~ "Hawthorne",
#       TRUE                     ~ "Control"
#     )
#   )


##############################
# 14) Validación y limpieza 
##############################
# Ejemplos útiles:
social %>%
  mutate(edad = 2006 - yearofbirth) %>%
  summarise(
    na_edad      = sum(is.na(edad)),
    fuera_rango  = sum(!between(edad, 18, 100), na.rm = TRUE)
  )

