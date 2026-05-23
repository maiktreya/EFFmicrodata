# analyze_yearly_data.R
library(data.table)
library(magrittr)
library(survey)
library(convey) # For inequality metrics
library(mitools) # For handling multiple imputations cleanly

# 1. Load data
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning (Performed on the entire data.table at once)
eff[, facine3 := as.numeric(facine3)]
eff[, renthog := as.numeric(renthog)]
eff[, riquezanet := as.numeric(riquezanet)]
eff[, p2_5 := as.numeric(p2_5)][is.na(p2_5), p2_5 := 0]
eff[, otraspr := as.numeric(otraspr)][is.na(otraspr), otraspr := 0]
eff[, riquezainmo := p2_5 + otraspr]
eff[, regten := factor(p2_1, levels = c(1:3), labels = c(
    "Alquiler",
    "Propiedad",
    "Cesion"
))]
eff[, bage := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos",
    "Entre 35 y 44 anos",
    "Entre 45 y 54 anos",
    "Entre 55 y 64 anos",
    "Entre 65 y 74 anos",
    "Mayor de 74 anos"
))]

# 3. Handle Multiple Imputation (Assuming the imputation identifier variable is named 'num_imp', ranging 1 to 5)
# We split the massive table into a list of 5 independent, parallel data.tables.
eff_list <- split(eff, by = "imputation")
mi_data <- mitools::imputationList(eff_list) # <-- Changed := to <-

# 4. Create a Multiply-Imputed Survey Design Object
# Passing a list of dataframes/data.tables to svydesign automatically creates an 'imputationList' design.
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 5. Run the Vectorized Estimations across all Imputations
mi_results <- with(
    mi_design,
    svyby(~renthog, ~ year + regten + bage, design = .design, svymean, na.rm = TRUE)
)

# 6. Pool results using Rubin's Rules
# MIcombine merges the point estimates and adjusts the standard errors to account for imputation variance.
pooled_stats <- MIcombine(mi_results)

# 7. Convert pooled results back to a clean data.table for presentation
final_stats <- data.table(
    group = names(coef(pooled_stats)),
    mean  = coef(pooled_stats),
    se    = SE(pooled_stats)
)

# Split by "." instead of ":"
final_stats[, c("year", "regten", "bage") := tstrsplit(group, ".", fixed = TRUE)]
final_stats[, group := NULL]

print(final_stats)
