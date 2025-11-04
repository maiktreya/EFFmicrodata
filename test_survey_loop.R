# test_survey.R
library(magrittr)
library(data.table)
library(survey)

# 1) cargar el microdato combinado
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)
mean_inc_c <- c()
median_inc_c <- c()

for (year in 2005)
{
    eff <- fread(paste0("datasets/eff/", year, "-EFF.microdat.", ifelse(year == 2022, "gz", "csv"), ""))

    # 2) asegurar tipos
    eff[, facine3 := as.numeric(facine3)]
    eff[, renthog := as.numeric(renthog)]

    # 3) definir los 5 dise<U+00F1>os de encuesta (uno por imputaci<U+00F3>n)
    designs <- lapply(sort(unique(eff$imputation)), function(i) {
        svydesign(ids = ~1, weights = ~facine3, data = eff[imputation == i])
    })

    # 4) c<U+00E1>lculos ponderados
    mean_inc <- vapply(designs, \(d) coef(svymean(~renthog, d, na.rm = TRUE)), numeric(1)) %>% mean()

    median_inc <- vapply(designs, \(d) {
        q <- svyquantile(~renthog, d, quantiles = 0.5, ci = FALSE, na.rm = TRUE)
        as.numeric(q[[1]])
    }, numeric(1)) %>% mean()
    mean_inc_c <- c(mean_inc, mean_inc_c)
    median_inc_c <- c(median_inc, median_inc_c)
}
# 5) mostrar resultados
print(data.table(
    mean_income = mean_inc_c,
    median_income = median_inc_c
))
