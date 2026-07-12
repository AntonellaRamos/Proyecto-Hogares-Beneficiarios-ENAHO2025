# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 05_clasificacion.R
# Propósito: Recodificación de variables, construcción de
# variable de beneficiario, índice FIES y tipologías
# Fuente: INEI - ENAHO 2025
# Autor: Antonella Ramos
# Fecha: 12/07/2026
# =========================================================

# 1. Cargar librerías ----
library(tidyverse)
library(arrow)
library(gt)

# 2. Cargar base explorada ----
enaho_2025 <- read_parquet("01_datos/procesados/enaho_2025_v5.parquet")
