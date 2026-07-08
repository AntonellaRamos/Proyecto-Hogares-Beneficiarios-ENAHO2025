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

