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

# 3. Selección de variables para el codebook ----
# Se seleccionan las variables base exploradas, recodificadas,
# compuestas y tipologías construidas en las etapas anteriores.

enaho_codebook_2025 <- enaho_2025 %>%
  select(
    # Variables base exploradas
    sexo_jefe,
    edad_jefe,
    ecivil_jefe,
    dominio,
    factor07,
    # Variables recodificadas
    grupo_edad_jefe,
    ecivil_agrupado,
    # Variables compuestas
    beneficiario,
    fies_score,
    fies_nivel,
    # Tipologías
    tipologia_1,
    tipologia_2,
    tipologia_3,
    tipologia_4
  ) %>%
  mutate(across(where(is.character), as.factor))

# Exportamos como base codebook
write_parquet(enaho_codebook_2025, "01_datos/procesados/enaho_2025_v7_codebook.parquet")