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
