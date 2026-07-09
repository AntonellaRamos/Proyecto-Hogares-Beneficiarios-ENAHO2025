# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 02_preparar_datos.R
# Propósito: Preparar la base de datos para el análisis mediante
# la selección y renombrado de variables, la inspección inicial
# y el diagnóstico de valores perdidos.
# Fuente: INEI - ENAHO 2025
# Autor: Antonella Ramos
# Fecha: 08/07/2026
# =========================================================

# 1. Cargar librerías ----
library(tidyverse)
library(arrow)
library(janitor)
library(naniar)
renv::snapshot()

# 2. Cargar llave y base integrada ----
keys_hogar <- c("CONGLOME", "VIVIENDA", "HOGAR")

enaho_2025 <- read_parquet("01_datos/procesados/enaho_2025_v1.parquet")

# 3. Filtrar jefe de hogar ----
# P203 == 1 identifica al jefe/jefa del hogar.
# Se trabaja a nivel hogar usando al jefe como representante
# sociodemográfico del hogar.
# Se usa P203.x porque tras el merge los módulos 200 y 130 comparten
# la variable P203 — el sufijo .x corresponde al Módulo 200,
# que es la fuente correcta para identificar el parentesco.
enaho_2025 <- enaho_2025 %>%
  filter(P203.x == 1)
