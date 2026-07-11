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

# 3. Conversión de tipos de datos ----
glimpse(enaho_2025)

# Observación: Las variables categóricas llegan como <dbl> desde el parquet.
# Se convierten a factor con etiquetas legibles según el
# diccionario de variables de la ENAHO 2025.

enaho_2025 <- enaho_2025 %>%
  mutate(
    # Sexo del jefe de hogar
    # 1 = Hombre, 2 = Mujer (Diccionario ENAHO 2025, Módulo 200)
    sexo_jefe = factor(sexo_jefe,
                       levels = c(1, 2),
                       labels = c("Hombre", "Mujer")),
    
    # Estado civil del jefe de hogar
    # 1=Casado(a), 2=Conviviente, 3=Viudo(a),
    # 4=Divorciado(a), 5=Separado(a), 6=Soltero(a)
    ecivil_jefe = factor(ecivil_jefe,
                         levels = c(1, 2, 3, 4, 5, 6),
                         labels = c("Casado/a", "Conviviente",
                                    "Viudo/a", "Divorciado/a",
                                    "Separado/a", "Soltero/a")),
    
    # Dominio geográfico
    # 1=Costa norte, 2=Costa centro, 3=Costa sur,
    # 4=Sierra norte, 5=Sierra centro, 6=Sierra sur,
    # 7=Selva, 8=Lima Metropolitana
    dominio = factor(dominio,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("Costa norte", "Costa centro",
                                "Costa sur", "Sierra norte",
                                "Sierra centro", "Sierra sur",
                                "Selva", "Lima Metropolitana")),
    
    # Variables de programas sociales (0/1 → No/Sí) (Diccionario ENAHO 2025, Módulo 700)
    across(starts_with("prog_"),
           ~ factor(., levels = c(0, 1),
                    labels = c("No", "Sí"))),
    
    # Variables FIES (1=Sí, 2=No) (Diccionario ENAHO 2025, Módulo 130)
    across(starts_with("fies_"),
           ~ factor(., levels = c(1, 2),
                    labels = c("Sí", "No")))
  )

# Verificación de tipos
glimpse(enaho_2025)


