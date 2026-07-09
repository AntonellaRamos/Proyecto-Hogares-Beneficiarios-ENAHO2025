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

### Control de versiones 
El proyecto está versionado con Git y alojado públicamente en GitHub. 
Cada commit corresponde a una unidad lógica de trabajo con un mensaje descriptivo del cambio realizado.

### Librerías utilizadas
El proyecto está desarrollado utilizando R (versión 4.5.1), con las siguientes librerías:

- `tidyverse`: Manipulación, limpieza, joins, transformación
- `readr`: Importar datos CSV
- `arrow`: Guardar y leer datos Parquet
- `renv`: Control de versiones de librerías

Los paquetes utilizados y sus versiones exactas están registrados en `renv.lock`, generado con `renv::init()`. Para restaurar el ambiente en otra máquina basta ejecutar `renv::restore()`.
