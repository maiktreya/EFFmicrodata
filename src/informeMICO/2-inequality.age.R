# analyze_combined_stats.R
library(data.table)
library(survey)
library(mitools)

# 1. Load Data
rm(list = ls())
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, renthog := as.numeric(renthog)]
eff[, riquezanet := as.numeric(riquezanet)]
eff[, bage := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
    "Entre 55 y 64 anos", "Entre 65 y 74 anos"
))]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Run Vectorized Estimations
mi_mean_rent <- with(mi_design, svyby(~renthog, ~ year + bage, design = .design, svymean, na.rm = TRUE))
mi_mean_riq  <- with(mi_design, svyby(~riquezanet, ~ year + bage, design = .design, svymean, na.rm = TRUE))
mi_med_rent  <- with(mi_design, svyby(~renthog, ~ year + bage, design = .design, svyquantile, quantiles = 0.5, na.rm = TRUE))
mi_med_riq   <- with(mi_design, svyby(~riquezanet, ~ year + bage, design = .design, svyquantile, quantiles = 0.5, na.rm = TRUE))

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
final_stats[, c("year", "bage") := tstrsplit(group, ".", fixed = TRUE)][, group := NULL]

# Calculate wealth ratios relative to the youngest cohort ("Menor de 35 anos") for each wave
final_stats[, ratio_mean_wealth := mean_riquezanet / mean_riquezanet[bage == "Menor de 35 anos"], by = year]
final_stats[, ratio_median_wealth := median_riquezanet / median_riquezanet[bage == "Menor de 35 anos"], by = year]
final_stats[, ratio_mean_renthog := mean_renthog / mean_renthog[bage == "Menor de 35 anos"], by = year]
final_stats[, ratio_median_renthog := median_renthog / median_renthog[bage == "Menor de 35 anos"], by = year]

# Organize columns and sort by wave
setcolorder(final_stats, c(
    "year", "bage", "mean_renthog", "median_renthog", 
    "mean_riquezanet", "median_riquezanet", 
    "ratio_mean_wealth", "ratio_median_wealth",
    "ratio_mean_renthog", "ratio_median_renthog"
))
setorder(final_stats, year)

# 8. Print results on screen and export to file
print(final_stats)
fwrite(final_stats, "out/informeMICO/2-inequality-age-64.csv")