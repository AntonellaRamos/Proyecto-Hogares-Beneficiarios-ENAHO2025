# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre el perfil
# sociodemográfico de hogares beneficiarios de programas de
# asistencia alimentaria en el Perú durante el año 2025
# Propósito: Crear sistema de carpetas, inicializar renv
# y enlazar con GitHub
# Autor: Antonella Ramos
# Fecha: 08/07/2026
# =========================================================

# 1. Crear carpetas ----
dir.create("01_datos")
dir.create("01_datos/originales")
dir.create("01_datos/procesados")
dir.create("02_scripts")
dir.create("03_outputs")

# 2. Enlace con GitHub ----
usethis::use_git()
usethis::use_github()