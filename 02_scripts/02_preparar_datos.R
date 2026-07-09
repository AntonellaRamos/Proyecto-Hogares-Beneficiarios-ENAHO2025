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
library(gt)

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

# 5. Inspección inicial ----
dim(enaho_seleccion)
names(enaho_seleccion)
glimpse(enaho_seleccion)

# 6. Diagnóstico de valores perdidos ----
# Antes de tomar decisiones sobre el tratamiento de valores perdidos,
# se evalúa el patrón de ausencias por variable. El objetivo es identificar:
# (1) la proporción de valores perdidos, (2) las variables con mayor
# concentración de ausencias y (3) si existen problemas de disponibilidad
# de información que puedan afectar el análisis.
# El tratamiento de NAs se realiza en el script 03_tratamiento_nas.R.

# Se crea una versión de la base con etiquetas descriptivas para
# visualización y reporte. La base enaho_seleccion queda intacta
# para el análisis posterior.
etiquetas_variables <- c(
  "conglome"          = "Conglomerado",
  "vivienda"          = "Vivienda",
  "hogar"             = "Hogar",
  "ubigeo"            = "Ubigeo",
  "dominio"           = "Dominio geográfico",
  "estrato"           = "Estrato",
  "factor07"          = "Factor de expansión",
  "sexo_jefe"         = "Sexo del jefe de hogar",
  "edad_jefe"         = "Edad del jefe de hogar",
  "ecivil_jefe"       = "Estado civil del jefe de hogar",
  "prog_vaso_leche"   = "Programa Vaso de Leche",
  "prog_comedor"      = "Comedor Popular",
  "prog_desayuno_esc" = "Qali Warma (desayuno escolar)",
  "prog_almuerzo_esc" = "Qali Warma (almuerzo escolar)",
  "prog_cuna_mas"     = "Cuna Más",
  "prog_canasta"      = "Canasta de alimentos",
  "prog_otro1"        = "Otro programa 1",
  "prog_otro2"        = "Otro programa 2",
  "prog_otro3"        = "Otro programa 3",
  "prog_no_recibio"   = "No recibió ningún programa",
  "fies_1"            = "FIES 1: Preocupación por alimentos",
  "fies_2"            = "FIES 2: Alimentación saludable",
  "fies_3"            = "FIES 3: Variedad de alimentos",
  "fies_4"            = "FIES 4: Omitió comidas",
  "fies_5"            = "FIES 5: Comió menos de lo normal",
  "fies_6"            = "FIES 6: Se quedó sin alimentos",
  "fies_7"            = "FIES 7: Tuvo hambre sin comer",
  "fies_8"            = "FIES 8: Día entero sin comer"
)

enaho_grafico <- enaho_seleccion %>%
  rename(!!!setNames(names(etiquetas_variables), etiquetas_variables))

# 6.1 Visualización gráfica (naniar) ----
# Se utiliza un gráfico de barras para identificar visualmente las variables
# con mayor proporción de valores perdidos antes de definir su tratamiento.
grafico_nas <- gg_miss_var(enaho_grafico, show_pct = TRUE) +
  labs(
    title = "Porcentaje de valores perdidos por variable",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    y = "% de valores perdidos",
    x = "Variables",
    caption = paste0(
      "Fuente: Encuesta Nacional de Hogares (ENAHO) 2025, INEI. Elaboración propia.\n",
      "Nota: El porcentaje representa la proporción de valores NA respecto al total de hogares."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption  = element_text(hjust = 0, size = 9)
  )

print(grafico_nas)

ggsave(
  "03_outputs/acondicionar_grafico_nas.png",
  plot = grafico_nas,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# 6.2 Reporte tabular ----
# Se exporta en dos formatos:
# - .csv: formato reproducible para trazabilidad del pipeline.
# - .html: versión presentable con etiquetas descriptivas (ver bloque 6.3).
reporte_nas <- enaho_seleccion %>%
  summarise(across(everything(),
                   ~ round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "porcentaje_na") %>%
  arrange(desc(porcentaje_na))

write_csv(reporte_nas, "03_outputs/acondicionar_reporte_nas.csv")

# 6.3 Reporte presentable (gt) ----
# Se exporta como .html con etiquetas descriptivas, título, subtítulo
# y nota al pie para documentación del diagnóstico de NAs.
reporte_nas %>%
  mutate(variable = etiquetas_variables[variable]) %>%
  gt() %>%
  tab_header(
    title = "Porcentaje de valores perdidos por variable",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(16)),
    locations = cells_title(groups = "title")
  ) %>%
  cols_label(
    variable      = "Variable",
    porcentaje_na = "% de valores perdidos"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Nota: El porcentaje representa la proporción de valores NA respecto al total de hogares (n = ",
      nrow(enaho_seleccion), ")."
    )
  ) %>%
  gtsave("03_outputs/acondicionar_reporte_nas.html")