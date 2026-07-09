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

# 4. Selección y renombrado de variables ----
# Se seleccionan solo las variables relevantes para el proyecto y se
# renombran en el mismo paso para mayor eficiencia y legibilidad.
# Los sufijos .x corresponden al Módulo 200 en los casos donde
# variables compartidas entre módulos generaron duplicados tras el merge.
# FACTOR07.x proviene del Módulo 700 (factor de expansión a nivel hogar).
enaho_seleccion <- enaho_2025 %>%
  select(
    # Llaves de identificación del hogar
    conglome = CONGLOME,
    vivienda = VIVIENDA,
    hogar    = HOGAR,
    
    # Variables de estratificación geográfica
    ubigeo  = UBIGEO.x,
    dominio = DOMINIO.x,
    estrato = ESTRATO.x,
    
    # Módulo 200: perfil sociodemográfico del jefe de hogar
    sexo_jefe   = P207.x,
    edad_jefe   = P208A,
    ecivil_jefe = P209.x,
    
    # Factor de expansión (Módulo 700)
    factor07 = FACTOR07.x,
    
    # Módulo 700: programas de asistencia alimentaria
    prog_vaso_leche   = `P701$01`,
    prog_comedor      = `P701$02`,
    prog_desayuno_esc = `P701$03`,
    prog_almuerzo_esc = `P701$04`,
    prog_cuna_mas     = `P701$05`,
    prog_canasta      = `P701$10`,
    prog_otro1        = `P701$06`,
    prog_otro2        = `P701$07`,
    prog_otro3        = `P701$08`,
    prog_no_recibio   = `P701$09`,
    
    # Módulo 130: Escala FIES
    fies_1 = P130_1,
    fies_2 = P130_2,
    fies_3 = P130_3,
    fies_4 = P130_4,
    fies_5 = P130_5,
    fies_6 = P130_6,
    fies_7 = P130_7,
    fies_8 = P130_8
  )