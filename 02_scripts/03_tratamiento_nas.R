# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre hogares
# beneficiarios de programas de asistencia alimentaria en Perú
# Script: 03_tratamiento_nas.R
# Propósito: Diagnóstico por tipo de ausencia y tratamiento
# de valores perdidos en variables de programas y FIES
# Fuente: INEI - ENAHO 2025
# Autor: Antonella Ramos
# Fecha: 09/07/2026
# Ultima modificación: 10/07/2026
# =========================================================

# 1. Cargar librerías ----
library(tidyverse)
library(arrow)

# 2. Cargar llave y base acondicionada ----
keys_hogar <- c("conglome", "vivienda", "hogar")

enaho_2025 <- read_parquet("01_datos/procesados/enaho_2025_v2.parquet")

# 3. Diagnóstico por tipo de ausencia ----

# 3.1 Verificación de valores únicos ----
# Se revisan los valores presentes en cada variable con NAs
# para identificar si existen códigos especiales antes
# de tomar cualquier decisión de tratamiento.

# Variables de programas sociales
vars_programas <- c(
  "prog_vaso_leche", "prog_comedor", "prog_desayuno_esc",
  "prog_almuerzo_esc", "prog_cuna_mas", "prog_canasta",
  "prog_otro1", "prog_otro2", "prog_otro3", "prog_no_recibio"
)

lapply(enaho_2025[vars_programas], table, useNA = "ifany")

# Hallazgo:
# Los 2,270 valores perdidos aparecen de forma consistente en todas las
# las variables de programas sociales, lo que indica que corresponden a
# hogares sin información del módulo y no a omisiones en preguntas
# individuales.

# Variables FIES
lapply(enaho_2025[paste0("fies_", 1:8)], table, useNA = "ifany")

# Hallazgo:
# La inspección de las variables FIES muestra que, además de las
# categorías de respuesta principales, existen códigos especiales
# definidos en el diccionario de la ENAHO. Estos se tratarán según
# su significado.

# 3.2 Verificación del origen de NAs con P700I ----
# P700I identifica al informante del capítulo 700.
# Si los NAs en programas coinciden con ausencia de P700I,
# la causa es estructural: el módulo no fue aplicado al hogar.

enaho_v1 <- read_parquet(
  "01_datos/procesados/enaho_2025_v1.parquet"
)

diagnostico_informante700 <- enaho_v1 %>%
  filter(P203.x == 1) %>%
  mutate(
    na_vaso = is.na(`P701$01`)
  ) %>%
  count(
    na_vaso,
    informante_ausente = is.na(P700I)
  )

print(diagnostico_informante700)

# 3.3 Diagnóstico geográfico de NAs en programas ----
# Se evalúa si los NAs se concentran en algún dominio
# para identificar el patrón de ausencia.

diagnostico_prog_dominio <- enaho_2025 %>%
  mutate(na_prog = is.na(prog_vaso_leche)) %>%
  group_by(dominio) %>%
  summarise(
    total  = n(),
    nas    = sum(na_prog),
    pct_na = round(nas / total * 100, 1),
    .groups = "drop"
  )

print(diagnostico_prog_dominio)

# 3.4 Diagnóstico geográfico de NAs en fies ----
diagnostico_fies_dominio <- enaho_2025 %>%
  mutate(na_fies = is.na(fies_1)) %>%
  group_by(dominio) %>%
  summarise(
    total  = n(),
    nas    = sum(na_fies),
    pct_na = round(nas / total * 100, 1),
    .groups = "drop"
  )

print(diagnostico_fies_dominio)

# 3.5 Verificación de solapamiento entre NAs de programas y fies ----
# Se evalúa si los hogares con NAs en programas son los mismos
# que tienen NAs en FIES, o si son grupos distintos.
diagnostico_solapamiento <- enaho_2025 %>%
  summarise(
    na_solo_prog = sum(is.na(prog_vaso_leche) & !is.na(fies_1)),
    na_solo_fies = sum(!is.na(prog_vaso_leche) & is.na(fies_1)),
    na_ambos     = sum(is.na(prog_vaso_leche) & is.na(fies_1)),
    na_ninguno   = sum(!is.na(prog_vaso_leche) & !is.na(fies_1))
  )

print(diagnostico_solapamiento)

# Hallazgo:
# - 1,379 hogares tienen NAs solo en programas (tienen FIES completo)
# - 118 hogares tienen NAs solo en fies (tienen programas completo)
# - 891 hogares tienen NAs en ambos módulos
# - 31,314 hogares no tienen NAs en ninguno
#
# Conclusión:
# Los NAs en programas y fies son problemas de ausencia
# independientes, no un solo bloque de hogares sin información.
# Total de hogares afectados: 1,379 + 118 + 891 = 2,388 hogares únicos.

# 4. Tratamiento de valores perdidos ----
# ------------------------------------------------------------------------------
# CASO 1: Variables de programas de asistencia alimentaria
# Porcentaje de NAs: 6.74% (2,270 hogares)
# Diagnóstico: Los NAs aparecen de forma consistente en todas las variables
# de programas, lo que indica ausencia de información a nivel de módulo,
# no omisiones en preguntas individuales. Se verificó que los hogares
# afectados sí tienen informante registrado en P700I, descartando ausencia
# estructural. Los NAs se concentran en el dominio 8 (Lima Metropolitana),
# donde alcanzan el 38.8% frente a menos del 1% en otros dominios.
# Esto indica que la ausencia depende de una variable observable (dominio),
# lo que corresponde a un patrón MAR (Missing at Random).
# Estrategia: Conservación de NAs sin imputación ni eliminación.
# El valor 0 ya representa explícitamente la no recepción del programa,
# por lo que un NA no es equivalente a 0. La exclusión de estos hogares ocurrirá
# naturalmente al construir variables de beneficiario en una etapa posterior.
# ------------------------------------------------------------------------------

# Verificación: los NAs se conservan
colSums(is.na(enaho_2025[, grep("^prog_", names(enaho_2025))]))

# ------------------------------------------------------------------------------
# CASO 2: Variables de inseguridad alimentaria (fies)
# Porcentaje de NAs: 2.99% (1,009 hogares)
# Diagnóstico: Los NAs aparecen de forma consistente en las 8 variables
# FIES, lo que indica ausencia de información a nivel de módulo.
# Se concentran en el dominio 8 (Lima Metropolitana), donde alcanzan
# el 16.1% frente a menos del 5% en otros dominios. Lo que corresponde a 
# un patrón MAR (Missing at Random).
# Adicionalmente, se identificaron valores 3 (No sabe) y 4 (No responde)
# en todas las variables FIES. Estos códigos no son respuestas válidas
# para la escala FIES, que solo acepta Sí (1) o No (2).
# Estrategia: Conversión de valores 3 y 4 a NA, seguida de conservación
# de NAs sin eliminación. No es posible determinar el nivel de inseguridad
# alimentaria de un hogar con respuestas incompletas. La exclusión de estos
# hogares ocurrirá naturalmente al construir el índice FIES en una etapa
# posterior. 
# ------------------------------------------------------------------------------

# Convertir valores 3 y 4 a NA
enaho_tratada <- enaho_2025 %>%
  mutate(
    across(
      fies_1:fies_8,
      ~ na_if(na_if(., 3), 4)
    )
  )

# Verificar conversión
lapply(enaho_tratada[paste0("fies_", 1:8)], table, useNA = "ifany")

# 5. Verificación del tamaño de la base ----
# Los valores perdidos identificados en el diagnóstico se conservan
# para su manejo explícito en etapas posteriores. Se verifica que
# el tratamiento aplicado no haya reducido el número de hogares.

resumen_n <- data.frame(
  hogares_antes = n_antes,
  hogares_despues = n_despues,
  hogares_eliminados = n_antes - n_despues
)

print(resumen_n)

# 6. Exportar base tratada ----
# Se exporta la base tratada y renombrada como tercera versión
# del dataset procesado. 
write_parquet(enaho_tratada, "01_datos/procesados/enaho_2025_v3.parquet")
