library(data.table)

# Este script está preparado para leer archivos base de imputaciones múltiples
# en formato CSV, usando data.table::fread.

# Define el número de imputaciones
imps <- 1:5
year <- 2014
# Función para cargar una imputación desde un archivo CSV
# El formato del nombre de archivo esperado es, por ejemplo, "databol1.csv"
load_imp_csv <- function(path_fmt, i) {
    file_path <- sprintf(path_fmt, i)
    if (file.exists(file_path)) {
        dt <- fread(file_path)
        dt[, imputation := i]
        return(dt)
    } else {
        warning(paste("File not found:", file_path))
        return(NULL)
    }
}

# Rutas a los archivos CSV.
# Se asume una estructura de carpetas similar a la original, pero con CSVs.
# Ejemplo: "datasets/full_csv/EFF_2022/databol%d.csv"
databol_path_fmt <- paste0("datasets/full_csv/EFF ", year, "/databol%d.csv")
otras_path_fmt <- paste0("datasets/full_csv/EFF ", year, "/otras_secciones_", year, "_imp%d.csv")
sec6_path_fmt <- paste0("datasets/full_csv/EFF ", year, "/seccion6_", year, "_imp%d.csv")

# Cargar y combinar las imputaciones para cada sección
databol <- rbindlist(lapply(imps, function(i) load_imp_csv(databol_path_fmt, i)), fill = TRUE)
otras <- rbindlist(lapply(imps, function(i) load_imp_csv(otras_path_fmt, i)), fill = TRUE)
sec6 <- rbindlist(lapply(imps, function(i) load_imp_csv(sec6_path_fmt, i)), fill = TRUE)

# Unir las tablas si se cargaron datos
if (nrow(databol) > 0) {
    # Eliminar pesos si existen en tablas adicionales para evitar duplicados
    if ("facine3" %in% names(otras)) otras[, facine3 := NULL]
    if ("facine3" %in% names(sec6)) sec6[, facine3 := NULL]

    # Realizar los merges
    household_id <- paste0("h_", year)
    eff_year <- merge(databol, otras, by = c(household_id, "imputation"), all.x = TRUE)
    eff_year <- merge(eff_year, sec6, by = c(household_id, "imputation"), all.x = TRUE)

    # Escribir el archivo final comprimido
    output_path <- paste0("datasets/eff/", year, "-EFF.microdat.csv.gz")
    fwrite(eff_year, output_path, compress = "gzip")
    message(paste("Exportado:", output_path))
} else {
    message("No se encontraron archivos de imputación en formato CSV en las rutas especificadas.")
}
