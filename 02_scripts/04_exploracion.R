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
  width = 8,
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
