# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 04_exploracion.R
# Propósito: Conversión de tipos, estadísticas descriptivas,
# visualizaciones exploratorias y exploración bivariada
# Fuente: INEI - ENAHO 2025
# Autor: Antonella Ramos
# Fecha: 10/07/2026
# =========================================================

# 1. Cargar librerías ----
library(tidyverse)
library(arrow)
library(gt)

# 2. Cargar base tratada ----
enaho_2025 <- read_parquet("01_datos/procesados/enaho_2025_v3.parquet")
