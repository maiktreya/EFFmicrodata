# 10-precise-min-sample-size.R
library(data.table)
library(survey)

# 1. Load Data
rm(list = ls())
gc(full = TRUE)
eff <- fread("datasets/full_eff_refined.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, renthog := as.numeric(renthog)]
eff[, riquezanet := as.numeric(riquezanet)]

# Tenancy Cleaning
eff[renta_alq > 0, p2_1 := 4] 
eff[, regten := factor(p2_1, levels = c(1:4), labels = c("Alquiler", "Propiedad", "Cesion", "Casero"))]

# Age Cohort Cleaning
eff[, bage := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
    "Entre 55 y 64 anos", "Entre 65 y 74 anos", "Mayores de 74 anos"
))]

# 3. Isolate 2022 Wave and use a single imputation layer for sample diagnostics
eff_2022_single <- eff[year == 2022 & imputation == 1]
single_design <- svydesign(ids = ~1, weights = ~facine3, data = eff_2022_single)

# 4. Run Estimates and extract complex survey CVs from a single design object
stats_rent <- svyby(~renthog, ~ regten + bage, design = single_design, svymean, na.rm = TRUE, vartype = "cv")
stats_riq  <- svyby(~riquezanet, ~ regten + bage, design = single_design, svymean, na.rm = TRUE, vartype = "cv")

# 5. Extract actual unweighted household counts per cell
counts <- eff_2022_single[, .(n_actual = .N), by = .(regten, bage)]

# 6. Build the Diagnostics Table
diagnostics <- data.table(
    group           = names(cv(stats_rent)),
    cv_est_rent     = as.numeric(cv(stats_rent)),
    cv_est_riq      = as.numeric(cv(stats_riq))
)

# Parse group back into clean factor columns
diagnostics[, c("regten", "bage") := tstrsplit(group, ".", fixed = TRUE)][, group := NULL]

# Merge actual household counts
diagnostics <- merge(diagnostics, counts, by = c("regten", "bage"))

# 7. Compute variance parameters (accounting for complex design effects implicitly)
diagnostics[, cv_pop_deff_rent := n_actual * cv_est_rent^2]
diagnostics[, cv_pop_deff_riq  := n_actual * cv_est_riq^2]

# 8. Apply Sample Size Formula Parameters
z <- 1.96         # 95% Confidence Interval
e_strict <- 0.10  # Strict 10% Margin of Error
e_relaxed <- 0.20 # Relaxed 20% Margin of Error

# Compute required minimum sample sizes
diagnostics[, n_min_rent_strict  := ceiling((z^2 / e_strict^2) * cv_pop_deff_rent)]
diagnostics[, n_min_rent_relaxed := ceiling((z^2 / e_relaxed^2) * cv_pop_deff_rent)]

diagnostics[, n_min_riq_strict   := ceiling((z^2 / e_strict^2) * cv_pop_deff_riq)]
diagnostics[, n_min_riq_relaxed  := ceiling((z^2 / e_relaxed^2) * cv_pop_deff_riq)]

# 9. Create Flag Statuses for Wealth
diagnostics[, status_wealth_strict := ifelse(n_actual >= n_min_riq_strict, "PASS", "FAIL")]
diagnostics[, status_wealth_relaxed := ifelse(n_actual >= n_min_riq_relaxed, "PASS", "FAIL")]

# Clean up temporary variance tracking columns
diagnostics[, c("cv_pop_deff_rent", "cv_pop_deff_riq") := NULL]

# Organize columns and sort by current actual sample size
setcolorder(diagnostics, c("regten", "bage", "n_actual", 
                           "cv_est_rent", "n_min_rent_strict", "n_min_rent_relaxed",
                           "cv_est_riq", "n_min_riq_strict", "n_min_riq_relaxed", 
                           "status_wealth_strict", "status_wealth_relaxed"))
setorder(diagnostics, n_actual)

# 10. Output results
print(diagnostics)
fwrite(diagnostics, "out/informeMICO/9-precise-min-sample-size.csv")