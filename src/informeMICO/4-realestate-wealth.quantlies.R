# analyze_combined_stats.R
library(data.table)
library(survey)
library(mitools)

# 1. Load Data
rm(list = ls())
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, p2_5 := as.numeric(p2_5)][is.na(p2_5), p2_5 := 0] # viv. principal
eff[, otraspr := as.numeric(otraspr)][is.na(otraspr), otraspr := 0] # otras. prop.
eff[, riquezainmo := p2_5 + otraspr]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Run Vectorized Estimations
mi_med_riq_inm <- with(mi_design, svyby(~riquezainmo, ~year,
    design = .design, svyquantile,
    quantiles = c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
    na.rm = TRUE
))

mi_shares_riq_inm <- with(mi_design, svyby(~riquezainmo, ~year,
    design = .design, svytotal,
    na.rm = TRUE
))

# 5. Pool Results
pool_med_riq_inm <- MIcombine(mi_med_riq_inm)
pool_shares_riq_inm <- MIcombine(mi_shares_riq_inm)

# 6. Column-Bind directly into a single data.table
final_stats <- data.table(
    group = names(coef(pool_med_riq_inm)), # Pull names once
    median_riquezainmo = as.numeric(coef(pool_med_riq_inm)),
    share_riquezainmo = as.numeric(prop.table(coef(pool_shares_riq_inm))) * 100
)

# 7. Split groups and clean up
final_stats[, c("year", "riquezainmo") := tstrsplit(group, ":", fixed = TRUE)][, group := NULL]
setcolorder(final_stats, c("year", "riquezainmo"))
setorder(final_stats, year)

# 8. Print results on screen and export to file
print(final_stats)
fwrite(final_stats, "out/informeMICO/4-inmo-inequality-quantiles_new.csv")