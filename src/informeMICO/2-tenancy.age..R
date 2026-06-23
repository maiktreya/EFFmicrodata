# analyze_homeownership_age.R
library(data.table)
library(survey)
library(mitools)

# 1. Load Data
rm(list = ls())
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning & Dummy Creation
eff[, facine3 := as.numeric(facine3)]
eff[, is_owner := as.numeric(p2_1 == 2)] # 1 if owner, 0 otherwise
eff[, bage := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
    "Entre 55 y 64 anos", "Entre 65 y 74 anos", "Mayores de 74 anos"
))]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Run Vectorized Estimations (Mean of binary dummy = Rate)
mi_homeownership <- with(mi_design, svyby(~is_owner, ~ year + bage, design = .design, svymean, na.rm = TRUE))

# 5. Pool Results
pool_homeownership <- MIcombine(mi_homeownership)

# 6. Column-Bind directly into a single data.table
final_stats <- data.table(
    group               = names(coef(pool_homeownership)), 
    homeownership_rate = as.numeric(coef(pool_homeownership))
)

# 7. Split groups and clean up
final_stats[, c("year", "bage") := tstrsplit(group, ".", fixed = TRUE)][, group := NULL]

# Calculate homeownership ratios relative to the youngest cohort ("Menor de 35 anos") for each wave
final_stats[, ratio_homeownership := homeownership_rate / homeownership_rate[bage == "Menor de 35 anos"], by = year]

# Organize columns and sort by wave
setcolorder(final_stats, c("year", "bage", "homeownership_rate", "ratio_homeownership"))
setorder(final_stats, year, bage)

# 8. Print results on screen and export to file
print(final_stats)
fwrite(final_stats, "out/informeMICO/2-homeownership-age.csv")