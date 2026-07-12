# README — Perfil sociodemográfico de hogares beneficiarios de programas de asistencia alimentaria en Perú, 2025.
## Autor: Antonella Ramos Rios
## Encuesta: ENAHO 2025
## Módulos utilizados: Características de los Miembros del Hogar (200), Programas Sociales (Miembros del Hogar) (700) e Inseguridad Alimentaria (130)
## Unidad de análisis: Hogares

---

## Descripción del proyecto
Este repositorio incluye el código y el flujo de trabajo completo del procesamiento de datos sobre hogares beneficiarios de programas de asistencia alimentaria en Perú. Se utilizan datos de la Encuesta Nacional de Hogares (ENAHO) del año 2025 trabajados íntegramente en R.
 
El proyecto procesa el perfil sociodemográfico de los hogares a partir de las siguientes dimensiones:
 
- **Sociodemográficas**: Características de los miembros del hogar, edad y sexo (Módulo 200).
- **Programas sociales**: Participación en programas de asistencia alimentaria (Módulo 700).
- **Inseguridad alimentaria**: Experiencias de acceso a alimentos según la [Escala FIES de la FAO](https://www.fao.org/measuring-hunger/access-to-food/about-the-food-insecurity-experience-scale-(fies)/es) (Módulo 130).

---

## 1. EXTRAER 

### Información de la fuente
- **Fuente**: Instituto Nacional de Estadística e Informática (INEI).
- **Encuesta**: ENAHO Metodología Actualizada.
- **Año**: 2025.
- **Período**: Anual (enero-diciembre).
- **Portal de descarga**: [INEI - Microdatos ENAHO](https://proyectos.inei.gob.pe/microdatos/index.htm)
- **Fecha de descarga**: 07/07/2026

### Módulos seleccionados
 
#### Módulo 200: Características de los miembros del hogar
- **Nombre original INEI**: `Enaho01-2025-200.csv`
- **Descripción**: Información sociodemográfica de los miembros del hogar. 
- **Justificación**: Contiene las variables de identificación personal y características sociodemográficas necesarias para describir el perfil de la población beneficiaria y para la integración de las bases de datos.
 
#### Módulo 700: Programas Sociales (Miembros del Hogar)
- **Nombre original INEI**: `Enaho01-2025-700.csv` 
- **Descripción**: Participación de los hogares en programas sociales de asistencia alimentaria. 
- **Justificación**: Contiene la variable `P701`, que registra si el hogar recibió algún programa de asistencia alimentaria (Vaso de Leche, Qali Warma, Comedor Popular, entre otros). Es la fuente principal para identificar a los hogares beneficiarios, unidad de análisis del proyecto. El módulo viene naturalmente a nivel hogar (una fila por hogar) e incluye el factor de expansión a nivel hogar (`FACTOR07`).
 
#### Módulo 130: Inseguridad Alimentaria
- **Nombre original INEI**: `Enaho01-2025-130.csv` 
- **Descripción**: Experiencias de inseguridad alimentaria de los miembros del hogar durante el mes anterior. 
- **Justificación**: Contiene las 8 preguntas de la Escala de Experiencia de Inseguridad Alimentaria (FIES) de la FAO, que permiten construir un índice de inseguridad alimentaria por hogar (leve, moderada, severa). 

### Ubicación en el proyecto
- **Datos originales**: `01_datos/originales/`

### Archivos técnicos consultados
El archivo descargado del INEI incluye documentos técnicos de referencia (ficha técnica, diccionario de variables y cuestionario). Estos archivos fueron consultados durante el procesamiento pero no se incluyen en el repositorio por no ser parte del flujo de datos.

---

## 2. GESTIONAR
### Convenciones de nombres
**Scripts**: Numerados por orden de ejecución con prefijo de dos dígitos, seguidos de un nombre descriptivo del propósito en minúsculas y con  guiones bajos. El número indica el orden en que deben ejecutarse.
- Ejemplo: `00_creacion_carpetas_enlace_github.R`

**Datos originales**: Los nombres de archivo indicados en la sección **EXTRAER** corresponden a la denominación original del INEI tal como se descargaron del portal de microdatos. Para facilitar la trazabilidad dentro del proyecto, los archivos fueron renombrados localmente siguiendo la convención `enaho_mNNN_YYYY.csv`.
- **Módulo 200: Características de los miembros del hogar**
   - Nombre original INEI: `Enaho01-2025-200.csv`
   - Nombre en el proyecto: `enaho_m200_2025.csv` 

- **Módulo 700: Programas Sociales (Miembros del Hogar)**
   - Nombre original INEI: `Enaho01-2025-700.csv` 
   - Nombre en el proyecto: `enaho_m700_2025.csv` 

- **Módulo 130: Inseguridad Alimentaria**
   - Nombre original INEI: `Enaho01-2025-130.csv`
   - Nombre en el proyecto: `enaho_m130_2025.csv`

**Datos procesados**: Tras la unión de módulos en `01_carga_union_modulos.R`, la base integrada se exporta en formato `.parquet`. El sufijo de versión (`_v1`, `_v2`, etc.) se incrementa cada vez que una transformación sustantiva  del dataset, como la creación de nuevas variables o recodificaciones, genera una nueva versión de la base procesada.
- Ejemplo: `enaho_2025_v1.parquet`
- Ubicación: `01_datos/procesados/`

**Outputs**:  Nombrados con prefijo que indica la dimensión del procesamiento seguido del tipo de contenido.
- Ejemplo: `acondicionar_grafico_nas.png`
- Ubicación: `03_outputs`

### Control de versiones 
El proyecto está versionado con Git y alojado públicamente en GitHub. 
Cada commit corresponde a una unidad lógica de trabajo con un mensaje descriptivo del cambio realizado.

### Librerías utilizadas
El proyecto está desarrollado utilizando R (versión 4.5.1), con las siguientes librerías:

- `tidyverse`: Manipulación, limpieza, joins, transformación
- `readr`: Importar datos en formato CSV
- `arrow`: Guardar y leer datos Parquet
- `janitor`: Limpieza y estandarización de nombres de variables
- `naniar`: Diagnóstico y visualización de valores perdidos
- `survey`: Incorporación del diseño muestral y cálculo de estimaciones con el factor de expansión.
- `gt`: Generación de tablas presentables en formato HTML
- `htmltools`: Inserción de tablas HTML en el informe descriptivo.
- `knitr`: Integración de gráficos y resultados en el informe descriptivo.
- `rmarkdown`: Compilación de informes en HTML
- `labelled`: Inyección de etiquetas y metadatos en variables
- `codebook`: Generación automatizada de codebooks
- `dataMaid`: Auditoría y reportes de calidad de datos
- `renv`: Control de versiones de librerías

Los paquetes utilizados y sus versiones exactas están registrados en `renv.lock`, generado con `renv::init()`. Para restaurar el ambiente en otra máquina basta ejecutar `renv::restore()`.

### Orden de ejecución de scripts

| Script | Propósito |
|--------|-----------|
| `00_creacion_carpetas_github.R` | Configuración del entorno, creación de carpetas y enlace con GitHub |
| `01_carga_union_modulos.R` | Importación y merge de módulos 200, 700 y 130 |
| `02_acondicionamiento.R` | Selección, renombrado e inspección de variables |
| `03_tratamiento_nas.R` | Diagnóstico y tratamiento de valores perdidos |
| `04_exploracion.R` | Conversión de tipos, estadísticas descriptivas y visualizaciones |
| `05_informe_descriptivo.Rmd` | Informe descriptivo interpretado en HTML |
| `06_clasificacion.R` | Recodificación de variables, índice FIES y tipologías |
| `07_reporte_variables_recodificadas.Rmd` | Reporte que integra las tablas de las variables recodificadas y categorías derivadas construidas en HTML |
| `08_documentar.R` | Metadatos, decisiones metodológicas y codebook final |

### Estructura del directorio

```
Proyecto-Hogares-Beneficiarios-ENAHO2025/
│
├── 01_datos/
│   ├── originales/              # Datos originales INEI: carpeta excluida del repositorio por peso
│   │   ├── enaho_m130_2025.csv  # Módulo 130: Inseguridad Alimentaria
│   │   ├── enaho_m200_2025.csv  # Módulo 200: Características de los miembros del hogar
│   │   └── enaho_m700_2025.csv  # Módulo 700: Programas Sociales
│   └── procesados/              # Bases procesadas en formato .parquet
│       ├── enaho_2025_v1.parquet  # Merge de módulos 200, 700 y 130
│       ├── enaho_2025_v2.parquet  # Variables seleccionadas y renombradas
│       ├── enaho_2025_v3.parquet  # NAs tratados
│       ├── enaho_2025_v4.parquet  # Variables guardadas antes de ponderación
│       ├── enaho_2025_v5.parquet  # Tipos corregidos y diseño muestral definido
│       ├── enaho_2025_v6.parquet  # Variables clasificadas y tipologías
│       └── enaho_2025_v7_codebook.parquet  # Base con metadatos inyectados
│
├── 02_scripts/
│   ├── 01_carga_union_modulos.R
│   ├── 02_acondicionamiento.R
│   ├── 03_tratamiento_nas.R
│   ├── 04_exploracion.R
│   ├── 05_informe_descriptivo.Rmd
│   ├── 06_clasificacion.R
│   ├── 07_reporte_variables_recodificadas.Rmd
│   └── 08_documentar.R
│
├── 03_outputs/
│   ├── acondicionar/  # Outputs del script 02 y 03 con prefijo "acondicionar_"
│   ├── explorar/      # Outputs del script 04 y 05 con prefijo "explorar_"
│   ├── clasificar/    # Outputs del script 06 con prefijo "clasificar_"
│   └── documentar/    # Outputs del script 07 con prefijo "documentar_"
│
├── .Rprofile
├── .gitignore         # Excluye 01_datos/originales/ del repositorio
├── 00_creacion_carpetas_github.R
├── Proyecto-Hogares-Beneficiarios-ENAHO2025.Rproj
└── renv.lock
```

---

## 3. ACONDICIONAR

El acondicionamiento se realizó en dos scripts: `02_acondicionamiento.R` (selección, renombrado e inspección) y `03_tratamiento_nas.R` (diagnóstico  y tratamiento de valores perdidos).

### Integración de módulos
La unión de los módulos 200, 700 y 130 se realizó en `01_carga_union_modulos.R` mediante `left_join()`, usando como llave de identificación `CONGLOME + VIVIENDA + HOGAR`. Se optó por `left_join` para conservar todos los hogares del Módulo 200 como base de referencia, asignando `NA` donde no hubiera coincidencia en los módulos 700 y 130. Se verificó que la N resultante fuera igual al número de filas del Módulo 200 (33,702 hogares).

### Filtro de unidad de análisis
Se filtró el Módulo 200 a los jefes de hogar para trabajar a nivel hogar, usando al jefe como representante sociodemográfico del hogar. 

Esta estrategia metodológica se respalda en tres fundamentos teóricos y operativos:
- **Evitar la duplicación de observaciones**: Dado que cada hogar posee un identificador único compuesto por las variables de conglomerado, vivienda y hogar, mantener a todos los miembros duplicaría las características agregadas de la vivienda en el dataset. Al filtrar por el jefe de hogar, se garantiza un registro por cada hogar analizado.
Representación sociodemográfica: Las características del jefe de hogar permiten aproximarse al perfil sociodemográfico del hogar, dado que la ENAHO organiza la composición del hogar a partir de la relación de parentesco con el jefe/a del hogar [(INEI, Diccionario de Variables ENAHO 2025)](https://proyectos.inei.gob.pe/iinei/srienaho/Descarga/DocumentosMetodologicos/2025-37/Diccionario.pdf?utm_source).
- **Consistencia con módulos del INEI**: El propio INEI diseña módulos específicos (como el de Programas Sociales e Inseguridad Alimentaria) para ser respondidos directamente por el jefe del hogar o el ama de casa, asumiendo metodológicamente que sus respuestas unifican la situación del hogar. El filtro `P203 == 1` permite hacer fusiones de bases de datos (merge) exactas y limpias con estos módulos agregados.

### Selección y renombrado de variables
Se seleccionaron únicamente las variables relevantes para el proyecto y se renombraron en el mismo paso para mayor legibilidad. Los sufijos `.x` generados por el merge (variables compartidas entre módulos) se resolviero explícitamente en el `select()`, priorizando siempre el Módulo 200 como fuente de variables sociodemográficas. La base resultante (`enaho_2025_v2.parquet`) contiene 33,702 hogares y 28 variables con nombres descriptivos.

### Diagnóstico de valores perdidos
El diagnóstico identificó dos grupos de variables con valores perdidos:

**Variables de programas sociales**: 6.74% de NAs (2,270 hogares). Los NAs aparecen de forma consistente en todas las variables del módulo, lo  que indica ausencia de información a nivel de módulo. Se verificó que los  hogares afectados sí tienen informante registrado en `P700I`, descartando  ausencia estructural. Los NAs se concentran en el dominio 8 (Lima  Metropolitana, 38.8%) frente a menos del 1% en otros dominios. Es un patrón **MAR** (Missing at Random).

**Variables FIES**: 2.99% de NAs (1,009 hogares). Los NAs aparecen de forma consistente en las 8 variables, con concentración en el dominio 8 (16.1%) frente a menos del 5% en otros dominios. Es un patrón **MAR**. Adicionalmente se identificaron valores 3 (No sabe) y 4 (No responde) en todas las variables FIES.

- Nota: Se verificó además que los NAs de ambos módulos corresponden a hogares distintos: 1,379 hogares tienen NAs solo en programas sociales, 118 solo en FIES, 891 en ambos módulos y 31,314 sin NAs en ninguno.

### Tratamiento de valores perdidos

En ambos casos se optó por **conservar los NAs** sin imputación ni eliminación listwise. Esta decisión responde a que la exclusión de casos incompletos se realizará de forma controlada en CLASIFICAR, al momento de construir las variables compuestas (variable de beneficiario y índice FIES), evitando así reducir la N de forma prematura.

Como excepción, en las variables FIES se convirtieron los valores 3 (No sabe) y 4 (No responde) a NA, por no ser respuestas válidas para la escala, que solo acepta Sí (1) o No (2).

### Outputs generados
Los outputs se encuentran en `03_outputs/`:

| Archivo | Descripción |
|---------|-------------|
| `acondicionar_grafico_nas.png` | Gráfico de barras con % de NAs por variable |
| `acondicionar_reporte_nas.csv` | Reporte tabular reproducible de NAs por variable |
| `acondicionar_reporte_nas.html` | Reporte presentable con etiquetas descriptivas |

---

## 4. EXPLORAR

La exploración se realizó en dos archivos: `04_exploracion.R` (estadísticas descriptivas, visualizaciones y exploración bivariada) y `05_informe_descriptivo.Rmd` (informe interpretado en HTML). El análisis es exclusivamente descriptivo, responde a la pregunta "¿qué hay en los datos?" sin establecer relaciones causales ni contrastar hipótesis.

### Conversión de tipos de datos
Las variables categóricas se convirtieron de `<dbl>` a `factor` con etiquetas legibles según el diccionario de variables de la ENAHO 2025: `sexo_jefe`, `ecivil_jefe`, `dominio`, las variables de programas sociales (`prog_*`) (0/1 → No/Sí) y las variables de inseguridad alimentaria (`fies_*`) (1/2 → Sí/No).

### Diseño muestral y factor de expansión
Todos los cálculos de frecuencia, proporción y estadísticos descriptivos incorporan el factor de expansión `FACTOR07` del Módulo 700 mediante el paquete `survey`. Se definió un objeto de diseño muestral con `svydesign()` que pondera cada hogar según el número de hogares que representa en la población objetivo. Los resultados se interpretan como estimaciones poblacionales y no como frecuencias muestrales. La base con tipos corregidos y diseño muestral definido se exportó como `enaho_2025_v5.parquet`.

-*Nota metodológica: En una primera etapa del proyecto se realizó una exploración descriptiva utilizando únicamente la muestra de hogares. Posteriormente, el análisis fue actualizado para incorporar el factor de expansión de la ENAHO 2025, de modo que las estimaciones fueran representativas de la población de hogares del país. Con fines de reproducibilidad y trazabilidad del proceso, se conservó la versión previa (`enaho_2025_v4.parquet`) y se generó `enaho_2025_v5.parquet` como la versión definitiva empleada en el análisis.*

### Variables exploradas

**Exploración univariada**: Se exploraron seis variables (sexo, edad y estado civil del jefe de hogar, dominio geográfico, participación en programas de asistencia alimentaria y escala FIES). Las variables categóricas se analizaron con tablas de frecuencias ponderadas (`svytable()`) y gráficos de barras. La variable continua `edad_jefe` se exploró con estadísticos descriptivos ponderados (`svymean()`, `svyvar()`, `svyquantile()`) e histograma.

**Exploración bivariada**: Se realizaron diez cruces en el script `04_exploracion.R`, combinando variables sociodemográficas, condición de beneficiario e inseguridad alimentaria. Para el informe descriptivo `05_informe_descriptivo.Rmd` se seleccionaron cuatro cruces por su mayor relevancia temática respecto a la pregunta central del proyecto: 

1. Beneficiario de algún programa × inseguridad alimentaria (todos los ítems FIES)
2. Sexo del jefe × condición de beneficiario
3. Dominio geográfico × programas de asistencia alimentaria
4. Edad del jefe × inseguridad alimentaria (todos los ítems FIES)

Los cinco cruces restantes (estado civil × sexo, sexo × dominio, edad × sexo, estado civil × beneficiario y sexo × programas) se encuentran disponibles en `03_outputs/` como evidencia del proceso exploratorio, pero no se incluyeron en el informe por ser menos centrales al tema del proyecto.

Las estimaciones en todos los cruces incorporan el factor de expansión mediante `svyby()`. Los cruces categórica × categórica se presentan como tablas de contingencia ponderadas. El cruce continua × categórica (edad × FIES) se presenta como boxplot por grupo.

### Outputs generados

Los outputs se encuentran en `03_outputs/`:

- En total, se generaron 32 outputs en formato `.png` (gráficos) y `.html` (tablas), además del informe descriptivo (`explorar_informe_descriptivo.html`). El script utilizado para generar el informe se encuentra en `02_scripts/`.

---

## 5. CLASIFICAR

La clasificación se realizó en dos archivos: `06_clasificacion.R` (recodificación de variables existentes, construcción de variables derivadas y definición de tipologías) y `07_reporte_variables_recodificadas.Rmd` (reporte de las variables recodificadas y categorías construidas en HTML). Las transformaciones se basan en criterios metodológicos externos (estándares INEI y metodología FAO) para garantizar comparabilidad.

### Recodificaciones

**Grupos de edad del jefe de hogar** (`grupo_edad_jefe`): La variable continua `edad_jefe` se recodificó en tres grupos usando los cortes estándar del INEI: joven (< 30 años), adulto (30-59 años) y adulto mayor (60 años o más).

**Estado civil agrupado** (`ecivil_agrupado`): Las seis categorías originales de `ecivil_jefe` se reagruparon en tres: en pareja (casado/a y conviviente), sin pareja por ruptura o viudez (separado/a, viudo/a, divorciado/a) y soltero/a. La reagrupación responde a la distinción sustantiva entre hogares con jefatura de pareja, hogares que atravesaron una ruptura y hogares de jefatura individual nunca emparejada.

### Variable compuesta: condición de beneficiario

Se construyó una variable dicotómica (`beneficiario`) que identifica si el hogar recibió al menos uno de los nueve programas de asistencia alimentaria registrados. Se excluyó `prog_no_recibio` porque corresponde a una categoría de no recepción y no a un programa alimentario. Los hogares con valores faltantes en todas las variables de programas conservan NA, evitando asumir ausencia de recepción.

### Índice FIES

Se construyó un índice sumativo (`fies_score`) basado parcialmente en la metodología FIES desarrollada por la FAO. Siguiendo su enfoque general, los 8 ítems de experiencia de inseguridad alimentaria fueron recodificados en variables binarias (1 = respuesta afirmativa, 0 = respuesta negativa) y agregados en un puntaje de severidad con rango de 0 a 8. 

Los hogares con valores faltantes en alguno de los ítems conservan NA, debido a que se requiere información completa para obtener una clasificación válida. A partir del puntaje acumulado se construyó la variable categórica `fies_nivel` con tres niveles de inseguridad alimentaria:

- **0-1**: Seguridad alimentaria
- **2-3**: Inseguridad alimentaria moderada
- **4-8**: Inseguridad alimentaria severa

- *Nota metodológica: Estos puntos de corte corresponden a una clasificación operativa utilizada para el análisis descriptivo del estudio y no replican los procedimientos completos de calibración y establecimiento de umbrales oficiales de la escala FIES. Para mayor referencia sobre la metodología FIES, consultar la documentación de la [FAO]([https://proyectos.inei.gob.pe/iinei/srienaho/Descarga/DocumentosMetodologicos/2025-37/Diccionario.pdf?utm_source](https://pmc.ncbi.nlm.nih.gov/articles/PMC6121128/#sec2).*

### Tipologías

Se construyeron cuatro tipologías MECE (mutuamente excluyentes y colectivamente exhaustivas). Los hogares con NA en alguna de las variables que componen la tipología quedan excluidos de la clasificación.

**Tipología 1: Beneficiario × inseguridad alimentaria**: Cruza la condición de beneficiario con el nivel FIES, generando 6 tipos. Permite identificar hogares que reciben programas pese a no presentar inseguridad alimentaria severa, y hogares con inseguridad severa sin cobertura de programas.

**Tipología 2: Perfil sociodemográfico del jefe de hogar**: Cruza sexo con grupo de edad, generando 6 tipos (hombre/mujer × joven/adulto/adulto mayor). Permite describir perfiles diferenciados de jefatura dentro de los hogares de la muestra.

**Tipología 3: Cobertura territorial de programas**: Cruza dominio geográfico con condición de beneficiario, generando 16 combinaciones (8 dominios × 2 condiciones). Permite identificar diferencias territoriales en la presencia de hogares beneficiarios y no beneficiarios de programas alimentarios.

**Tipología 4: Vulnerabilidad alimentaria territorial**: Cruza dominio geográfico, nivel FIES y condición de beneficiario, generando hasta 48 combinaciones posibles (8 dominios × 3 niveles FIES × 2 condiciones). Permite identificar la distribución territorial de los distintos niveles de inseguridad alimentaria según cobertura de programas.

### Verificación de las tipologías

La verificación de las tipologías incluyó la revisión de la cobertura de las categorías generadas, la presencia de casos en cada combinación y la proporción de hogares excluidos por valores faltantes en las variables utilizadas.

Las tipologías construidas presentan categorías con observaciones en la muestra analizada; sin embargo, no todas muestran una distribución homogénea entre sus categorías. Algunas combinaciones presentan menor número de casos, especialmente aquellas que incorporan niveles severos de inseguridad alimentaria y condición de beneficiario dentro de determinados dominios territoriales.

Asimismo, los hogares con información faltante en alguna de las variables de construcción fueron excluidos de la clasificación. Por ello, las tipologías se interpretan como categorías descriptivas de los hogares clasificados y no como estimaciones poblacionales.

### Outputs generados

Los outputs se encuentran en `03_outputs/`:

| Archivo | Descripción |
|---|---|
| `clasificar_tabla_tipologia1.html` | Distribución de hogares según condición de beneficiario e inseguridad alimentaria |
| `clasificar_tabla_tipologia2.html` | Distribución de hogares según perfil sociodemográfico del jefe de hogar |
| `clasificar_tabla_tipologia3.html` | Distribución de hogares según dominio geográfico y condición de beneficiario |
| `clasificar_tabla_tipologia4.html` | Distribución de hogares según dominio geográfico, nivel de inseguridad alimentaria y condición de beneficiario |

Adicionalmente, se generó el reporte `clasificar_reporte_variables_recodificadas.html`, que integra las tablas de las variables recodificadas y categorías derivadas construidas. El script utilizado para generar este reporte se encuentra en `02_scripts/`.

----

## 6. DOCUMENTAR

La documentación se realizó en el script `06_documentar.R` e incluyó la selección de variables analíticas, incorporación de metadatos, documentación de decisiones metodológicas y generación automatizada del codebook final.

### Variables documentadas
Se documentaron 14 variables correspondientes a las etapas de exploración, clasificación y construcción de índices y tipologías:

- **Variables base exploradas**: `sexo_jefe`, `edad_jefe`, `ecivil_jefe`, `dominio`, `factor07`
- **Variables recodificadas**: `grupo_edad_jefe`, `ecivil_agrupado`
- **Variables compuestas**: `beneficiario`, `fies_score`, `fies_nivel`
- **Tipologías**: `tipologia_1`, `tipologia_2`, `tipologia_3`, `tipologia_4`

### Metadatos incorporados
Para cada variable se documentó: etiqueta descriptiva (`var_label()`), fuente original en el diccionario ENAHO 2025, tipo de variable, valores posibles y decisión metodológica que justifica su construcción o recodificación. Los metadatos a nivel de estudio incluyen nombre del dataset, descripción de la submuestra y autoría.

### Construcción del codebook
El codebook se construyó manualmente en cuatro pasos:

1. **Tipo de variable**: clasificación de cada variable como numérica, categórica o texto a partir de su clase en R.
2. **Valores posibles**: para variables categóricas, los niveles del factor; para variables numéricas, el rango mínimo-máximo; para `factor07`, descripción textual.
3. **Frecuencias y distribución**: conteo y porcentaje por categoría para todas las variables factor, incluyendo NAs como categoría explícita.
4. **Ensamblaje**: integración de etiquetas descriptivas, tipo de variable, valores posibles, decisiones metodológicas y frecuencias en un único dataframe exportado como `.csv`.

### Outputs generados
Los outputs se encuentran en `03_outputs/documentar/`:

| Archivo | Descripción |
|---------|-------------|
| `documentar_codebook_final_enaho_2025.csv` | Codebook final con etiquetas, tipos, valores posibles, fuentes y frecuencias de todas las variables documentadas |
