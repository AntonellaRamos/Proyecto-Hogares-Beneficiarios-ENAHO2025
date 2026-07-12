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

# 3. Recodificación de variables ----

# 3.1 Grupos de edad del jefe de hogar ----
# Cortes estándar INEI: <30, 30-59, 60+
enaho_2025 <- enaho_2025 %>%
  mutate(
    grupo_edad_jefe = case_when(
      edad_jefe < 30                       ~ "Joven (< 30)",
      edad_jefe >= 30 & edad_jefe < 60     ~ "Adulto (30-59)",
      edad_jefe >= 60                      ~ "Adulto mayor (60+)"
    ),
    grupo_edad_jefe = factor(
      grupo_edad_jefe,
      levels = c("Joven (< 30)", "Adulto (30-59)", "Adulto mayor (60+)")
    )
  )

# Verificar 
sum(is.na(enaho_2025$grupo_edad_jefe))
table(enaho_2025$grupo_edad_jefe)

# 3.2 Estado civil agrupado ----
# En pareja / Sin pareja (ruptura/viudez) / Soltero/a
enaho_2025 <- enaho_2025 %>%
  mutate(
    ecivil_agrupado = case_when(
      ecivil_jefe %in% c("Casado/a", "Conviviente")                    ~ "En pareja",
      ecivil_jefe %in% c("Separado/a", "Viudo/a", "Divorciado/a")      ~ "Sin pareja (ruptura/viudez)",
      ecivil_jefe == "Soltero/a"                                        ~ "Soltero/a"
    ),
    ecivil_agrupado = factor(
      ecivil_agrupado,
      levels = c("En pareja", "Sin pareja (ruptura/viudez)", "Soltero/a")
    )
  )

#Verificar
sum(is.na(enaho_2025$ecivil_agrupado))
table(enaho_2025$ecivil_agrupado)

# 4. Variable compuesta: condición de beneficiario ----
# Variable dicotómica: recibió al menos un programa alimentario.
# Se excluye prog_no_recibio (categoría de no recepción, no programa).
# Hogares con NA en todas las variables de programas. 

programas <- c(
  "prog_vaso_leche", "prog_comedor", "prog_desayuno_esc",
  "prog_almuerzo_esc", "prog_cuna_mas", "prog_canasta",
  "prog_otro1", "prog_otro2", "prog_otro3"
)

enaho_2025 <- enaho_2025 %>%
  mutate(
    beneficiario = case_when(
      if_any(all_of(programas), ~ . == "Sí") ~ "Sí",
      if_all(all_of(programas), ~ is.na(.))  ~ NA_character_,
      TRUE                                    ~ "No"
    ),
    beneficiario = factor(beneficiario, levels = c("Sí", "No"))
  )

# Verificar
table(enaho_2025$beneficiario, useNA = "ifany")

# 5. Índice FIES ----
# Se recodifican las respuestas FIES a valores binarios:
# Sí = 1 (respuesta afirmativa de experiencia de inseguridad alimentaria)
# No = 0
# Los valores faltantes se mantienen como NA.
#
# El puntaje FIES corresponde a la suma de respuestas afirmativas
# y toma valores entre 0 y 8. Si existe alguna respuesta faltante,
# el puntaje final del hogar se mantiene como NA.
#
# Clasificación según metodología FIES:
# 0-3  = Seguridad alimentaria
# 4-6  = Inseguridad alimentaria moderada
# 7-8  = Inseguridad alimentaria severa

enaho_2025 <- enaho_2025 %>%
  mutate(
    across(
      starts_with("fies_"),
      ~ case_when(
        . == "Sí" ~ 1,
        . == "No" ~ 0,
        TRUE      ~ NA_real_
      ),
      .names = "{.col}_num"
    ),
    fies_score = rowSums(
      across(ends_with("_num")),
      na.rm = FALSE
    ),
    fies_nivel = case_when(
      is.na(fies_score) ~ NA_character_,
      fies_score <= 3   ~ "Seguridad alimentaria",
      fies_score <= 6   ~ "Inseguridad alimentaria moderada",
      fies_score >= 7   ~ "Inseguridad alimentaria severa"
    ),
    fies_nivel = factor(
      fies_nivel,
      levels = c(
        "Seguridad alimentaria",
        "Inseguridad alimentaria moderada",
        "Inseguridad alimentaria severa"
      )
    )
  )

# Verificar
enaho_2025 %>%
  summarise(
    across(
      ends_with("_num"),
      ~ paste(names(table(.)), collapse = ", ")
    )
  )

# Verificar cortes bien aplicados
enaho_2025 %>%
  select(fies_score, fies_nivel) %>%
  distinct() %>%
  arrange(fies_score)
