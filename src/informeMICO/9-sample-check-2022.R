# 9-sample-check-2022.R
library(data.table)

# 1. Load Data
rm(list = ls())
gc(full = TRUE)
eff <- fread("datasets/full_eff_refined.gz")

# 2. Vectorized Data Cleaning (Must match original scripts)
eff[, facine3 := as.numeric(facine3)]

# Tenancy Cleaning
eff[renta_alq > 0, p2_1 := 4] 
eff[, regten := factor(p2_1, levels = c(1:4), labels = c("Alquiler", "Propiedad", "Cesion", "Casero"))]

# Age Cohort Cleaning
eff[, bage := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
    "Entre 55 y 64 anos", "Entre 65 y 74 anos", "Mayores de 74 anos"
))]

# 3. Filter for 2022 and a Single Imputation 
# We filter by imputation == 1 so we are counting unique physical households, 
# not multiplying them by the number of MI sets.
eff_2022 <- eff[year == 2022 & imputation == 1]

# 4. Calculate Subgroup Metrics
subgroup_check <- eff_2022[, .(
    n_households        = .N,                             # Unweighted sample count
    weighted_population = sum(facine3, na.rm = TRUE)     # Population representation
), by = .(regten, bage)]

# 5. Calculate percentages to observe structural distortions
total_n   <- sum(subgroup_check$n_households)
total_pop <- sum(subgroup_check$weighted_population)

subgroup_check[, pct_sample := (n_households / total_n) * 100]
subgroup_check[, pct_population := (weighted_population / total_pop) * 100]

# 6. Sort by n_households ASCENDING to surface problematic cells immediately
setorder(subgroup_check, n_households)

# 7. Print results and save
print(subgroup_check)
fwrite(subgroup_check, "out/informeMICO/9-sample-check-2022.csv")