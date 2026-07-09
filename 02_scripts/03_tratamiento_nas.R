# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 03_tratamiento_nas.R
# Propósito: Diagnóstico por tipo de ausencia y tratamiento
# de valores perdidos en variables de programas y FIES
# Fuente: INEI - ENAHO 2025
# Autor: Antonella Ramos
# Fecha: 09/07/2026
# =========================================================

# 1. Cargar librerías ----
library(tidyverse)
library(arrow)

# 2. Cargar llave y base acondicionada ----
keys_hogar <- c("conglome", "vivienda", "hogar")

enaho_2025 <- read_parquet("01_datos/procesados/enaho_2025_v2.parquet")