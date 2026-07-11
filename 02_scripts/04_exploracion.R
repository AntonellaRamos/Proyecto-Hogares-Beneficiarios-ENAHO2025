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
    # Sexo del jefe del hogar
    # 1 = Hombre, 2 = Mujer (Diccionario ENAHO 2025, Módulo 200)
    sexo_jefe = factor(sexo_jefe,
                       levels = c(1, 2),
                       labels = c("Hombre", "Mujer")),
    
    # Estado civil del jefe del hogar
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

# 4. Variables sociodemográficas del jefe del hogar ----
# 4.1 Sexo del jefe del hogar ----

# Tabla de frecuencias
tabla_sexo <- enaho_2025 %>%
  count(sexo_jefe) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  rename(
    "Sexo"       = sexo_jefe,
    "N"          = n,
    "Porcentaje (%)" = porcentaje
  )

# Tabla gt
tabla_sexo %>%
  gt() %>%
  tab_header(
    title = "Distribución de hogares según sexo del jefe del hogar",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Nota: La distribución representa la proporción de hogares según el sexo del jefe del hogar (n = ",
      sum(tabla_sexo$N),
      " hogares)."
    )
  ) %>%
  gtsave(
    "03_outputs/explorar_tabla_sexo_jefe.html"
  )

# Gráfico de barras
grafico_sexo <- enaho_2025 %>%
  count(sexo_jefe) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = sexo_jefe, y = porcentaje, fill = sexo_jefe)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Hombre" = "#457B9D",
      "Mujer" = "#9B5DE5"
    )
  ) +
  labs(
    title = "Distribución de hogares según sexo del jefe del hogar",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    x = "Sexo del jefe del hogar",
    y = "Porcentaje (%)",
    caption = paste0(
      "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI.\n",
      "Nota: La distribución representa la proporción de hogares según el sexo del jefe del hogar (n = ",
      nrow(enaho_2025),
      " hogares)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 9)
  )

print(grafico_sexo)

ggsave(
  "03_outputs/explorar_grafico_sexo_jefe.png",
  plot = grafico_sexo,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 4.2 Edad del jefe del hogar ----

# Estadísticas descriptivas
resumen_edad <- enaho_2025 %>%
  summarise(
    media   = round(mean(edad_jefe, na.rm = TRUE), 1),
    mediana = median(edad_jefe, na.rm = TRUE),
    de      = round(sd(edad_jefe, na.rm = TRUE), 1),
    minimo  = min(edad_jefe, na.rm = TRUE),
    maximo  = max(edad_jefe, na.rm = TRUE),
    q1      = quantile(edad_jefe, 0.25, na.rm = TRUE, names = FALSE),
    q3      = quantile(edad_jefe, 0.75, na.rm = TRUE, names = FALSE)
  )

print(resumen_edad)

# Histograma
grafico_edad_hist <- enaho_2025 %>%
  ggplot(aes(x = edad_jefe)) +
  geom_histogram(
    binwidth = 5,
    fill = "#264653",
    color = "white"
  ) +
  labs(
    title = "Distribución de la edad del jefe del hogar",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    x = "Edad (años cumplidos)",
    y = "Número de hogares",
    caption = paste0(
      "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI.\n",
      "Nota: La distribución representa la edad del jefe del hogar en años cumplidos (n = ",
      nrow(enaho_2025),
      " hogares). Los intervalos del histograma corresponden a grupos de 5 años."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    )
  )

print(grafico_edad_hist)

ggsave(
  "03_outputs/explorar_histograma_edad_jefe.png",
  plot = grafico_edad_hist,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Tabla descriptiva
tabla_edad <- resumen_edad %>%
  pivot_longer(
    cols = everything(),
    names_to = "Estadístico",
    values_to = "Valor"
  ) %>%
  mutate(
    Estadístico = recode(
      Estadístico,
      media = "Media",
      mediana = "Mediana",
      de = "Desviación estándar",
      minimo = "Mínimo",
      q1 = "Primer cuartil (Q1)",
      q3 = "Tercer cuartil (Q3)",
      maximo = "Máximo"
    )
  )

# Tabla gt
tabla_edad %>%
  gt() %>%
  tab_header(
    title = "Estadísticos descriptivos de la edad del jefe del hogar",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  cols_label(
    Estadístico = "Estadístico",
    Valor = "Edad (años cumplidos)"
  ) %>%
  cols_align(
    align = "left",
    columns = Estadístico
  ) %>%
  cols_align(
    align = "right",
    columns = Valor
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Nota: Los estadísticos descriptivos corresponden a la edad del jefe del hogar (n = ",
      nrow(enaho_2025),
      " hogares)."
    )
  ) %>%
  gtsave(
    "03_outputs/explorar_tabla_edad_jefe.html"
  )

# 4.3 Estado civil del jefe del hogar ----

# Tabla de frecuencias
tabla_ecivil <- enaho_2025 %>%
  count(ecivil_jefe, name = "N") %>%
  mutate(porcentaje = round(N / sum(N) * 100, 1)) %>%
  arrange(desc(N))

# Tabla gt
tabla_ecivil %>%
  rename(
    "Estado civil del jefe del hogar" = ecivil_jefe,
    "Número de hogares" = N,
    "Porcentaje (%)" = porcentaje
  ) %>%
  gt() %>%
  tab_header(
    title = "Distribución de hogares según estado civil del jefe del hogar",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "center",
    columns = c("Número de hogares", "Porcentaje (%)")
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Nota: La distribución representa la proporción de hogares según el estado civil del jefe del hogar (n = ",
      sum(tabla_ecivil$N),
      " hogares)."
    )
  ) %>%
  gtsave(
    "03_outputs/explorar_tabla_ecivil_jefe.html"
  )

# Gráfico de barras
grafico_ecivil <- enaho_2025 %>%
  count(ecivil_jefe) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    ecivil_jefe = fct_reorder(ecivil_jefe, n, .desc = TRUE)
  ) %>%
  ggplot(aes(x = ecivil_jefe, y = porcentaje)) +
  geom_col(
    fill = "#264653"
  ) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Distribución de hogares según estado civil del jefe del hogar",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    x = "Estado civil del jefe del hogar",
    y = "Porcentaje (%)",
    caption = paste0(
      "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI.\n",
      "Nota: La distribución representa la proporción de hogares según el estado civil del jefe del hogar (n = ",
      nrow(enaho_2025),
      " hogares)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    )
  )

print(grafico_ecivil)

ggsave(
  "03_outputs/explorar_grafico_ecivil_jefe.png",
  plot = grafico_ecivil,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 5. Programas de asistencia alimentaria ----

# 5.1 Frecuencia de cada programa ----
tabla_programas <- enaho_2025 %>%
  summarise(
    across(
      c(prog_vaso_leche, prog_comedor, prog_desayuno_esc,
        prog_almuerzo_esc, prog_cuna_mas, prog_canasta,
        prog_otro1, prog_otro2, prog_otro3),
      ~ sum(. == "Sí", na.rm = TRUE)
    )
  ) %>%
  pivot_longer(everything(),
               names_to  = "programa",
               values_to = "n") %>%
  mutate(
    porcentaje = round(n / nrow(enaho_2025) * 100, 1),
    programa = recode(programa,
                      "prog_vaso_leche"   = "Programa Vaso de Leche",
                      "prog_comedor"      = "Comedor Popular",
                      "prog_desayuno_esc" = "Qali Warma (desayuno)",
                      "prog_almuerzo_esc" = "Qali Warma (almuerzo)",
                      "prog_cuna_mas"     = "Cuna Más",
                      "prog_canasta"      = "Canasta de alimentos",
                      "prog_otro1"        = "Otro programa 1",
                      "prog_otro2"        = "Otro programa 2",
                      "prog_otro3"        = "Otro programa 3"
    )
  ) %>%
  arrange(desc(n))

# Gráfico de barras
grafico_programas <- tabla_programas %>%
  mutate(
    programa = fct_reorder(programa, n)
  ) %>%
  ggplot(aes(x = programa, y = porcentaje)) +
  geom_col(
    fill = "#2A9D8F"
  ) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Hogares beneficiarios por programa de asistencia alimentaria",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    x = "Programa de asistencia alimentaria",
    y = "Porcentaje de hogares (%)",
    caption = paste0(
      "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI.\n",
      "Nota: La frecuencia representa el número y proporción de hogares que reportan recibir cada programa de asistencia alimentaria (n = ",
      nrow(enaho_2025),
      " hogares).\nUn hogar puede registrar más de un programa."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    )
  )

print(grafico_programas)

ggsave(
  "03_outputs/explorar_grafico_programas.png",
  plot = grafico_programas,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Tabla gt
tabla_programas %>%
  rename(
    "Programa de asistencia alimentaria" = programa,
    "Número de hogares" = n,
    "Porcentaje (%)" = porcentaje
  ) %>%
  gt() %>%
  tab_header(
    title = "Frecuencia de hogares según programa de asistencia alimentaria",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Nota: La frecuencia representa el número y la proporción de hogares que reportan recibir cada programa de asistencia alimentaria (n = ",
      nrow(enaho_2025),
      " hogares). Un hogar puede registrar más de un programa."
    )
  ) %>%
  gtsave(
    "03_outputs/explorar_tabla_programas.html"
  )

# 6. Inseguridad alimentaria (FIES) ----

# Distribución de respuestas por ítem FIES
etiquetas_fies <- c(
  "fies_1" = "FIES 1: Preocupación por alimentos",
  "fies_2" = "FIES 2: Alimentación saludable",
  "fies_3" = "FIES 3: Variedad de alimentos",
  "fies_4" = "FIES 4: Omitió comidas",
  "fies_5" = "FIES 5: Comió menos de lo normal",
  "fies_6" = "FIES 6: Se quedó sin alimentos",
  "fies_7" = "FIES 7: Tuvo hambre sin comer",
  "fies_8" = "FIES 8: Día entero sin comer"
)

# Tabla de frecuencias
tabla_fies <- enaho_2025 %>%
  select(starts_with("fies_")) %>%
  pivot_longer(everything(),
               names_to  = "item",
               values_to = "respuesta") %>%
  filter(!is.na(respuesta)) %>%
  group_by(item, respuesta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(item = etiquetas_fies[item])

# Gráfico de barras
grafico_fies <- tabla_fies %>%
  filter(respuesta == "Sí") %>%
  mutate(
    item = fct_reorder(item, porcentaje)
  ) %>%
  ggplot(aes(x = item, y = porcentaje)) +
  geom_col(
    fill = "#E76F51"
  ) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Porcentaje de hogares con respuesta afirmativa por ítem FIES",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    x = "Ítem de la Escala de Experiencia de Inseguridad Alimentaria (FIES)",
    y = "Porcentaje de hogares (%)",
    caption = paste0(
      "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI.\n",
      "Nota: El gráfico presenta el porcentaje de hogares que respondieron afirmativamente (\"Sí\") a cada ítem de la Escala de Experiencia de Inseguridad Alimentaria (FIES).\n",
      "Los porcentajes se calcularon sobre el total de respuestas válidas de cada ítem; se excluyeron los valores perdidos (NA)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    )
  )

print(grafico_fies)

ggsave(
  "03_outputs/explorar_grafico_fies.png",
  plot = grafico_fies,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Tabla de frecuencias: respuestas afirmativas por ítem FIES
tabla_fies_si <- tabla_fies %>%
  filter(respuesta == "Sí") %>%
  select(item, n, porcentaje)

# Tabla gt
tabla_fies_si %>%
  rename(
    "Ítem FIES" = item,
    "Número de hogares" = n,
    "Porcentaje (%)" = porcentaje
  ) %>%
  gt() %>%
  tab_header(
    title = "Porcentaje de hogares con respuesta afirmativa por ítem FIES",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = "Nota: El gráfico presenta el porcentaje de hogares que respondieron afirmativamente (\"Sí\") a cada ítem de la Escala de Experiencia de Inseguridad Alimentaria (FIES).\nLos porcentajes se calcularon sobre el total de respuestas válidas de cada ítem. Se excluyeron los valores perdidos (NA)."
  ) %>%
  gtsave(
    "03_outputs/explorar_tabla_fies.html"
  )

# 7. Exploración bivariada ----

# Beneficiario de algún programa vs. respuesta afirmativa en fies_1

# Recodifica la variable de no recepción del programa para identificar hogares beneficiarios (Sí/No)
enaho_biv <- enaho_2025 %>%
  mutate(
    beneficiario = ifelse(prog_no_recibio == "No", "Sí", "No"),
    beneficiario = factor(beneficiario, levels = c("Sí", "No"))
  )

# Tabla de frecuencias bivariada entre beneficiario y FIES
tabla_fies_biv <- enaho_biv %>%
  select(beneficiario, starts_with("fies_")) %>%
  pivot_longer(
    cols = starts_with("fies_"),
    names_to = "item_fies",
    values_to = "respuesta"
  ) %>%
  filter(
    !is.na(respuesta),
    !is.na(beneficiario)
  ) %>%
  group_by(item_fies, beneficiario) %>%
  summarise(
    porcentaje = round(mean(respuesta == "Sí") * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    item_fies = recode(item_fies, !!!etiquetas_fies)
  )

# Tabla gt
tabla_fies_biv %>%
  pivot_wider(
    names_from = beneficiario,
    values_from = porcentaje
  ) %>%
  rename(
    "Ítem FIES" = item_fies,
    "Beneficiario" = Sí,
    "No beneficiario" = No
  ) %>%
  gt() %>%
  tab_header(
    title = "Experiencias de inseguridad alimentaria según condición de beneficiario",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_column_labels()
  ) %>%
  cols_label(
    `Ítem FIES` = "Experiencia de inseguridad alimentaria",
    Beneficiario = "Beneficiario (%)",
    `No beneficiario` = "No beneficiario (%)"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI."
  ) %>%
  tab_source_note(
    source_note = "Nota: La tabla presenta el porcentaje de hogares que respondió afirmativamente a cada ítem de la Escala de Experiencia de Inseguridad Alimentaria (FIES), según condición de beneficiario. Se excluyeron los valores perdidos (NA)."
  ) %>%
  gtsave(
    "03_outputs/explorar_tabla_biv_beneficiario_fies8.html"
  )

# Gráfico de barras
grafico_biv_fies <- tabla_fies_biv %>%
  mutate(
    item_fies = fct_reorder(item_fies, porcentaje)
  ) %>%
  ggplot(aes(
    x = item_fies,
    y = porcentaje,
    fill = beneficiario
  )) +
  geom_col(
    position = "dodge"
  ) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    position = position_dodge(width = 0.9),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  scale_fill_manual(
    values = c(
      "Sí" = "#457B9D",
      "No" = "#A8DADC"
    )
  ) +
  labs(
    title = "Experiencias de inseguridad alimentaria según condición de beneficiario",
    subtitle = "Proyecto: Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025",
    x = "Ítem de la Escala de Experiencia de Inseguridad Alimentaria (FIES)",
    y = "Porcentaje de hogares (%)",
    fill = "Beneficiario de programa",
    caption = paste0(
      "Fuente: Elaboración propia con datos de la Encuesta Nacional de Hogares (ENAHO) 2025, INEI.\n",
      "Nota: El gráfico presenta el porcentaje de hogares que respondieron afirmativamente (\"Sí\") a cada ítem de la Escala de Experiencia de Inseguridad Alimentaria (FIES),\n",
      "según condición de beneficiario. Los porcentajes se calcularon sobre las respuestas válidas de cada ítem. Se excluyeron los valores perdidos (NA)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9
    ),
    legend.position = "bottom"
  )

print(grafico_biv_fies)

ggsave(
  "03_outputs/explorar_grafico_biv_beneficiario_fies.png",
  plot = grafico_biv_fies,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 7. Exportar base con variables transformadas ----
# Se exporta la con variables transformadas y renombrada como cuarta versión
# del dataset procesado.
write_parquet(enaho_2025, "01_datos/procesados/enaho_2025_v4.parquet")