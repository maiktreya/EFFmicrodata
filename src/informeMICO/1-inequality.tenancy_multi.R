# analyze_combined_stats.R
library(data.table)
library(survey)
library(mitools)

# 1. Load Data
rm(list = ls())
gc(full = TRUE)
eff <- fread("datasets/full_eff_refined.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, renthog := as.numeric(renthog)]
eff[, riquezanet := as.numeric(riquezanet)]
eff[, regten := factor(
    fcase(
        renta_alq > 0 & n_props_alq == 4, 5L,
        renta_alq > 0 & n_props_alq == 3, 4L,
        renta_alq > 0 & n_props_alq == 2, 3L,
        renta_alq > 0 & n_props_alq == 1, 2L,
        p2_1 == 3, 1L, # Cesion
        p2_1 == 2, 0L, # Propiedad
        p2_1 == 1, -1L # Alquiler
    ),
    levels = -1L:5L,
    labels = c(
        "Alquiler", "Propiedad", "Cesion",
        "Casero1", "Casero2", "Casero3", "Casero4+"
    )
)]
# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Run Vectorized Estimations
mi_mean_rent <- with(mi_design, svyby(~renthog, ~ year + regten, design = .design, svymean, na.rm = TRUE))
mi_mean_riq <- with(mi_design, svyby(~riquezanet, ~ year + regten, design = .design, svymean, na.rm = TRUE))
mi_med_rent <- with(mi_design, svyby(~renthog, ~ year + regten, design = .design, svyquantile, quantiles = 0.5, na.rm = TRUE))
mi_med_riq <- with(mi_design, svyby(~riquezanet, ~ year + regten, design = .design, svyquantile, quantiles = 0.5, na.rm = TRUE))

# 5. Pool Results
pool_mean_rent <- MIcombine(mi_mean_rent)
pool_mean_riq <- MIcombine(mi_mean_riq)
pool_med_rent <- MIcombine(mi_med_rent)
pool_med_riq <- MIcombine(mi_med_riq)

# 6. Column-Bind directly into a single data.table
final_stats <- data.table(
    group             = names(coef(pool_mean_rent)), # Pull names once
    mean_renthog      = as.numeric(coef(pool_mean_rent)),
    mean_riquezanet   = as.numeric(coef(pool_mean_riq)),
    median_renthog    = as.numeric(coef(pool_med_rent)),
    median_riquezanet = as.numeric(coef(pool_med_riq))
)

# 7. Split groups and clean up
final_stats[, c("year", "regten") := tstrsplit(group, ".", fixed = TRUE)][, group := NULL]
setcolorder(final_stats, c("year", "regten", "mean_renthog", "median_renthog", "mean_riquezanet", "median_riquezanet"))
setorder(final_stats, year)

# Calculate wealth/income ratios relative to the alquiler for each wave
final_stats[, ratio_mean_wealth_alq := mean_riquezanet / mean_riquezanet[regten == "Alquiler"], by = year]
final_stats[, ratio_median_wealth_alq := median_riquezanet / median_riquezanet[regten == "Alquiler"], by = year]
final_stats[, ratio_mean_income_alq := mean_renthog / mean_renthog[regten == "Alquiler"], by = year]
final_stats[, ratio_median_income_alq := median_renthog / median_renthog[regten == "Alquiler"], by = year]

# 8. Print results on screen an export to file
print(final_stats)
fwrite(final_stats, "out/informeMICO/1-inequality-tenancy_multi.csv")
