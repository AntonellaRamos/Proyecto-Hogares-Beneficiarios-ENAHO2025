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

# 5. Documentación de decisiones metodológicas ----
dict_metadata <- list(
  sexo_jefe = "Variable original P207 del Módulo 200. Recodificada a factor con etiquetas: 1=Hombre, 2=Mujer.",
  edad_jefe = "Variable original P208A del Módulo 200. Sin transformación. Variable continua en años cumplidos.",
  ecivil_jefe = "Variable original P209 del Módulo 200. Recodificada a factor con 6 categorías: Casado/a, Conviviente, Viudo/a, Divorciado/a, Separado/a, Soltero/a.",
  dominio = "Variable original DOMINIO del Módulo 200. Recodificada a factor con 8 categorías geográficas del diseño muestral ENAHO.",
  factor07 = "Factor de expansión anual a nivel hogar proveniente del Módulo 700. Utilizado para obtener estimaciones representativas de la población de hogares peruanos.",
  grupo_edad_jefe = "Recodificación de edad_jefe en tres grupos usando cortes estándar INEI: joven (<30), adulto (30-59), adulto mayor (60+).",
  ecivil_agrupado = "Reagrupación de ecivil_jefe en tres categorías sustantivas: en pareja (casado/a y conviviente), sin pareja por ruptura o viudez (separado/a, viudo/a, divorciado/a) y soltero/a.",
  beneficiario = "Variable dicotómica construida a partir de 9 variables de programas del Módulo 700 (prog_vaso_leche a prog_otro3). Un hogar es beneficiario si reportó recibir al menos un programa. Se excluye prog_no_recibio. Hogares con NA en todas las variables de programas conservan NA.",
  fies_score = "Índice sumativo construido a partir de los 8 ítems de la Escala de Experiencia de Inseguridad Alimentaria (FIES) del Módulo 130, siguiendo la metodología oficial de la FAO. Cada ítem se recodificó como 1 (Sí) o 0 (No). Rango: 0-8. Hogares con NA en algún ítem reciben NA en el índice.",
  fies_nivel = "Clasificación del puntaje FIES según umbrales oficiales FAO: 0-1 = seguridad alimentaria o inseguridad leve; 2-3 = inseguridad moderada; 4-8 = inseguridad severa. Hogares con NA en fies_score conservan NA.",
  tipologia_1 = "Tipología MECE construida a partir del cruce de condición de beneficiario × nivel de inseguridad alimentaria. Genera 6 tipos: beneficiario con inseguridad leve, moderada o severa; y no beneficiario con inseguridad leve, moderada o severa. Hogares con NA en alguna variable quedan excluidos.",
  tipologia_2 = "Tipología MECE construida a partir del cruce de sexo_jefe × grupo_edad_jefe. Genera 6 tipos: hombre joven, hombre adulto, hombre adulto mayor, mujer joven, mujer adulta y mujer adulta mayor.",
  tipologia_3 = "Tipología MECE construida a partir del cruce de dominio geográfico × condición de beneficiario. Genera 16 combinaciones (8 dominios × 2 condiciones de beneficiario). Permite describir la distribución territorial de los hogares según su condición de beneficiario.",
  tipologia_4 = "Tipología MECE construida a partir del cruce de dominio geográfico × nivel de inseguridad alimentaria × condición de beneficiario. Genera combinaciones territoriales según condición de beneficiario y nivel de inseguridad alimentaria."
)

for (var in names(dict_metadata)) {
  attr(enaho_codebook_2025[[var]], "description") <- dict_metadata[[var]]
}

# Metadatos a nivel de estudio
metadata(enaho_codebook_2025)$name        <- "Base de Datos Analítica - Hogares Beneficiarios ENAHO 2025"
metadata(enaho_codebook_2025)$description <- "Submuestra de la Encuesta Nacional de Hogares (ENAHO) 2025 restringida a jefes de hogar, con variables sociodemográficas, de programas de asistencia alimentaria e inseguridad alimentaria (FIES)."
metadata(enaho_codebook_2025)$creator     <- "Antonella Ramos"

# Guardamos con metadatos
write_parquet(enaho_codebook_2025, "01_datos/procesados/enaho_2025_v7_codebook.parquet")

# 6. Tipo de variable ----
tipo_variables <- tibble(
  variable = names(enaho_codebook_2025),
  tipo_r = map_chr(enaho_codebook_2025, ~ class(.x)[1])
) %>%
  mutate(
    tipo_variable = case_when(
      tipo_r %in% c("numeric", "integer") ~ "Numérica",
      tipo_r %in% c("factor", "ordered") ~ "Categórica",
      tipo_r == "character" ~ "Texto",
      TRUE ~ tipo_r
    )
  ) %>%
  select(variable, tipo_variable)

# 7. Valores posibles y significados ----
valores_variables <- map_dfr(
  names(enaho_codebook_2025),
  function(var){
    
    x <- enaho_codebook_2025[[var]]
    
    if(var == "factor07"){
      
      tibble(
        variable = var,
        valores_posibles = "Variable continua: factor de expansión anual a nivel hogar"
      )
      
    } else if(is.factor(x)){
      
      tibble(
        variable = var,
        valores_posibles = paste(
          levels(x),
          collapse = " | "
        )
      )
      
    } else {
      
      tibble(
        variable = var,
        valores_posibles = paste(
          min(x, na.rm = TRUE),
          "a",
          max(x, na.rm = TRUE)
        )
      )
    }
  }
)

# 8. Frecuencias y distribución ----
frecuencias_variables <- enaho_codebook_2025 %>%
  select(where(is.factor)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  mutate(
    categoria = replace_na(
      as.character(categoria),
      "NA (sin información)"
    )
  ) %>%
  count(
    variable,
    categoria
  ) %>%
  group_by(variable) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100,
      2
    )
  ) %>%
  ungroup()

# 9. Construcción del diccionario final del codebook ----
etiquetas_variables <- tibble(
  variable = names(enaho_codebook_2025),
  etiqueta_descriptiva = map_chr(
    enaho_codebook_2025,
    ~ as.character(var_label(.x))
  )
)

fuente_variables <- tibble(
  variable = names(dict_metadata),
  fuente_y_descripcion = unlist(dict_metadata)
)


codebook_variables <- etiquetas_variables %>%
  left_join(
    tipo_variables,
    by = "variable"
  ) %>%
  left_join(
    valores_variables,
    by = "variable"
  ) %>%
  left_join(
    fuente_variables,
    by = "variable"
  )

# 10. Incorporación de frecuencias al codebook ----
codebook_final_2025 <- codebook_variables %>%
  left_join(
    frecuencias_variables %>%
      group_by(variable) %>%
      summarise(
        frecuencias = paste(
          paste0(categoria, ": ", n, " (", porcentaje, "%)"),
          collapse = " | "
        )
      ),
    by = "variable"
  )

# 11. Exportación del codebook final ----
write_csv(
  codebook_final_2025,
  "03_outputs/documentar/documentar_codebook_final_enaho_2025.csv"
)