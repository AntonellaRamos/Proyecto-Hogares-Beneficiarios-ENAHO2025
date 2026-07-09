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
