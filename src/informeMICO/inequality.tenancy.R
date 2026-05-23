# analyze_combined_stats.R
library(data.table)
library(magrittr)
library(survey)
library(mitools)

# 1. Load Data
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning
eff[, facine3      := as.numeric(facine3)]
eff[, renthog      := as.numeric(renthog)]
eff[, riquezanet   := as.numeric(riquezanet)]
eff[, p2_5         := as.numeric(p2_5)][is.na(p2_5), p2_5 := 0]
eff[, otraspr      := as.numeric(otraspr)][is.na(otraspr), otraspr := 0]
eff[, riquezainmo  := p2_5 + otraspr]
eff[, regten := factor(p2_1, levels = c(1:3), labels = c("Alquiler", "Propiedad", "Cesion"))]
eff[, bage   := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
    "Entre 55 y 64 anos", "Entre 65 y 74 anos", "Mayor de 74 anos"
))]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data  <- mitools::imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Run Vectorized Estimations
mi_mean_rent <- with(mi_design, svyby(~renthog, ~ year + regten, design = .design, svymean, na.rm = TRUE))
mi_mean_riq  <- with(mi_design, svyby(~riquezanet, ~ year + regten, design = .design, svymean, na.rm = TRUE))
mi_med_rent  <- with(mi_design, svyby(~renthog, ~ year + regten, design = .design, svyquantile, quantiles = 0.5, keep.var = TRUE, na.rm = TRUE))
mi_med_riq   <- with(mi_design, svyby(~riquezanet, ~ year + regten, design = .design, svyquantile, quantiles = 0.5, keep.var = TRUE, na.rm = TRUE))

# 5. Pool Results 
pool_mean_rent <- MIcombine(mi_mean_rent)
pool_mean_riq  <- MIcombine(mi_mean_riq)
pool_med_rent  <- MIcombine(mi_med_rent)
pool_med_riq   <- MIcombine(mi_med_riq)

# 6. Column-Bind directly into a single data.table
final_stats <- data.table(
    group             = names(coef(pool_mean_rent)), # Pull names once
    mean_renthog      = as.numeric(coef(pool_mean_rent)),
    mean_riquezanet   = as.numeric(coef(pool_mean_riq)),
    median_renthog    = as.numeric(coef(pool_med_rent)),
    median_riquezanet = as.numeric(coef(pool_med_riq))
)

# 7. Split groups and clean up
final_stats[, c("year", "regten") := tstrsplit(group, ".", fixed = TRUE)]
final_stats[, group := NULL] 

setcolorder(final_stats, c("year", "regten", "mean_renthog", "median_renthog", "mean_riquezanet", "median_riquezanet"))
print(final_stats)
fwrite(final_stats, "out/informeMICO/inequality-tenancy.csv")