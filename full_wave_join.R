library(data.table)
library(haven)

imps <- 1:5
load_imp <- function(fmt, i) as.data.table(read_dta(sprintf(fmt, i)))[, imputation := i]

databol <- rbindlist(lapply(imps, \(i) load_imp("datasets/full/EFF_2022/databol%d.dta", i)), fill = TRUE)
otras <- rbindlist(lapply(imps, \(i) load_imp("datasets/full/EFF_2022/otras_secciones_imp%d.dta", i)), fill = TRUE)
sec6 <- rbindlist(lapply(imps, \(i) load_imp("datasets/full/EFF_2022/seccion6_imp%d.dta", i)), fill = TRUE)

# eliminar pesos si existen en tablas adicionales
if ("facine3" %in% names(otras)) otras[, facine3 := NULL]
if ("facine3" %in% names(sec6)) sec6[, facine3 := NULL]

eff_2022 <- merge(databol, otras, by = c("h_2022", "imputation"), all.x = TRUE)
eff_2022 <- merge(eff_2022, sec6, by = c("h_2022", "imputation"), all.x = TRUE)

fwrite(eff_2022, "datasets/eff/2022-EFF.microdat.gz", compress = "gzip")
message("Exportado: datasets/eff/2022-EFF.microdat.gz")
