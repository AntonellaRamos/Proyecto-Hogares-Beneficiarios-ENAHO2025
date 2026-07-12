# ==============================================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 08_documentar.R
# Autor: Antonella Ramos
# Fecha: 12/07/2026
# Objetivo: Añadir metadatos a la base analítica y generar el codebook final.
# ==============================================================================

# 1. Configuración y paquetes ----
library(tidyverse)
library(arrow)
library(labelled)
library(codebook)
library(dataMaid)

# 2. Cargar base analítica final ----
enaho_2025 <- read_parquet("01_datos/procesados/enaho_2025_v6.parquet")
