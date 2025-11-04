# test_survey.R
library(data.table)
library(survey)

# 1) cargar el microdato combinado
eff <- fread("datasets/eff/2022-EFF.microdat.gz")

# 2) asegurar tipos
eff[, facine3 := as.numeric(facine3)]
eff[, renthog21_eur22 := as.numeric(renthog21_eur22)]

# 3) definir los 5 dise<U+00F1>os de encuesta (uno por imputaci<U+00F3>n)
designs <- lapply(sort(unique(eff$imputation)), function(i) {
    svydesign(ids = ~1, weights = ~facine3, data = eff[imputation == i])
})

# 4) c<U+00E1>lculos ponderados
mean_inc <- sapply(designs, \(d) coef(svymean(~renthog21_eur22, d, na.rm = TRUE)))

median_inc <- sapply(designs, \(d) {
    q <- svyquantile(~renthog21_eur22, d, quantiles = 0.5, ci = FALSE, na.rm = TRUE)
    as.numeric(q) # ahora: extrae el n<U+00FA>mero del objeto lista
})

# 5) mostrar resultados
print(data.table(
    imputation = sort(unique(eff$imputation)),
    mean_income = mean_inc,
    median_income = median_inc
))

# 6) estimaci<U+00F3>n promedio simple (pooling na<U+00EF>ve)
message(sprintf("Media promedio imputaciones: %.2f", mean(mean_inc, na.rm = TRUE)))
message(sprintf("Mediana promedio imputaciones: %.2f", mean(median_inc, na.rm = TRUE)))
