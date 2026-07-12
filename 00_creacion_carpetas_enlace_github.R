# =========================================================
# Proyecto: ENAHO 2025 - Procesamiento de datos sobre el perfil
# sociodemográfico de hogares beneficiarios de programas de
# asistencia alimentaria en el Perú durante el año 2025
# Propósito: Crear sistema de carpetas, inicializar renv
# y enlazar con GitHub
# Autor: Antonella Ramos
# Fecha: 08/07/2026
# Ultima modificación: 12/07/2026
# =========================================================

# 1. Crear carpetas ----
dir.create("01_datos")
dir.create("01_datos/originales")
dir.create("01_datos/procesados")
dir.create("02_scripts")
dir.create("03_outputs")
dir.create("03_outputs/acondicionar")
dir.create("03_outputs/explorar")
dir.create("03_outputs/clasificar")
dir.create("03_outputs/documentar")

# 2. Enlace con GitHub ----
usethis::use_git()
usethis::use_github()

# 3. Mover archivos según prefijo ---
mover_outputs <- function(prefijo, carpeta) {
  
  archivos <- list.files(
    "03_outputs",
    pattern = paste0("^", prefijo),
    full.names = TRUE
  )
  
  file.rename(
    archivos,
    file.path("03_outputs", carpeta, basename(archivos))
  )
}

# Mover outputs
mover_outputs("acondicionar", "acondicionar")
mover_outputs("explorar", "explorar")
mover_outputs("clasificar", "clasificar")