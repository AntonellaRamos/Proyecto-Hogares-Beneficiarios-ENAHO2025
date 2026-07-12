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

# 4. Inyección de metadatos ----

# A. Variables base exploradas
var_label(enaho_codebook_2025$sexo_jefe)  <- "Sexo del jefe del hogar (Fuente: P207, Módulo 200)"
var_label(enaho_codebook_2025$edad_jefe)  <- "Edad del jefe del hogar en años cumplidos (Fuente: P208A, Módulo 200)"
var_label(enaho_codebook_2025$ecivil_jefe) <- "Estado civil del jefe del hogar (Fuente: P209, Módulo 200)"
var_label(enaho_codebook_2025$dominio)     <- "Dominio geográfico de la vivienda (Fuente: DOMINIO, Módulo 200)"
var_label(enaho_codebook_2025$factor07)    <- "Factor de expansión anual a nivel hogar (Fuente: FACTOR07, Módulo 700)"

# B. Variables recodificadas
var_label(enaho_codebook_2025$grupo_edad_jefe) <- "Grupo de edad del jefe del hogar (cortes estándar INEI)"
var_label(enaho_codebook_2025$ecivil_agrupado) <- "Estado civil agrupado del jefe del hogar"

# C. Variables compuestas
var_label(enaho_codebook_2025$beneficiario) <- "Condición de beneficiario de programas de asistencia alimentaria"
var_label(enaho_codebook_2025$fies_score)   <- "Puntaje FIES: suma de 8 ítems de inseguridad alimentaria (0-8)"
var_label(enaho_codebook_2025$fies_nivel)   <- "Nivel de inseguridad alimentaria según escala FIES (FAO)"

# D. Tipologías
var_label(enaho_codebook_2025$tipologia_1) <- "Tipología 1: Condición de beneficiario × nivel de inseguridad alimentaria"
var_label(enaho_codebook_2025$tipologia_2) <- "Tipología 2: Perfil sociodemográfico del jefe de hogar (sexo × grupo de edad)"
var_label(enaho_codebook_2025$tipologia_3) <- "Tipología 3: Dominio geográfico × condición de beneficiario"
var_label(enaho_codebook_2025$tipologia_4) <- "Tipología 4: Dominio geográfico × nivel de inseguridad alimentaria × condición de beneficiario"
