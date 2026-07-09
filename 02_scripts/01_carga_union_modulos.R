# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 01_carga_union_modulos.R
# Propósito: Importar los módulos 200, 700 y 130, realizar el
# merge a nivel hogar y guardar la base integrada
# Fuente: INEI - ENAHO 2025
# Autor: Antonella Ramos
# Fecha: 08/07/2026
# =========================================================

# 1. Cargar librerías ----
library(tidyverse)
library(readr)
library(arrow)
renv::snapshot()

# 2. Importar datos ----
# Módulo 200: Características de los miembros del hogar
m200 <- read_csv2(
  "01_datos/originales/enaho_m200_2025.csv",
  locale = locale(encoding = "Latin1")
)

# Módulo 700: Programas Sociales (participación en asistencia alimentaria)
m700 <- read_csv2(
  "01_datos/originales/enaho_m700_2025.csv",
  locale = locale(encoding = "Latin1")
)

# Módulo 130: Inseguridad Alimentaria
m130 <- read_csv2(
  "01_datos/originales/enaho_m130_2025.csv",
  locale = locale(encoding = "Latin1")
)

# 3. Unión de bases ----
# La llave keys_hogar identifica de forma única a cada hogar en la ENAHO.
keys_hogar <- c("CONGLOME", "VIVIENDA", "HOGAR")

# Se utiliza left_join tomando el Módulo 200 como base de referencia,
# para conservar todos los hogares aunque no tengan coincidencia en
# los módulos 700 o 130. Esto permite detectar casos sin registro.
enaho_2025 <- m200 %>%
  left_join(m700, by = keys_hogar) %>%
  left_join(m130, by = keys_hogar)

# Verificación: la N resultante debe ser igual a la del Módulo 200
nrow(enaho_2025) == nrow(m200)
