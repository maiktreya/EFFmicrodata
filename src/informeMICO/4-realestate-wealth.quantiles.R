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
eff[, riquezainmo := as.numeric(riquezainmo)]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Run Vectorized Estimations (Quantiles First)
probs <- c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99)
mi_med_riq_inm <- with(mi_design, svyby(~riquezainmo, ~year,
    design = .design, svyquantile,
    quantiles = probs,
    na.rm = TRUE
))

# 5. Pool Quantile Results
pool_med_riq_inm <- MIcombine(mi_med_riq_inm)

# --- Vectorized Quantile Share Calculation ---
q_dt <- data.table(group = names(coef(pool_med_riq_inm)), q_val = as.numeric(coef(pool_med_riq_inm)))
q_dt[, c("year", "q_str") := tstrsplit(group, ":", fixed = TRUE)]
q_dt[, year := type.convert(year, as.is = TRUE)] # Match data type of 'year' in eff_list
eff_list <- lapply(eff_list, function(df) {
    for (p in probs) {
        q_s <- paste0("riquezainmo.", p)
        col_name <- paste0("riq_le_", p)
        df[q_dt[q_str == q_s], (col_name) := fifelse(riquezainmo <= q_val, riquezainmo, 0), on = "year"]
    }
    df
})

# Re-create the design with updated data containing the new indicators
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4b. Run Vectorized Estimations (Cumulative Totals & Total Wealth)
v_names <- paste0("riq_le_", probs)
share_formula <- as.formula(paste("~", paste(c(v_names, "riquezainmo"), collapse = " + ")))
mi_shares_riq_inm <- with(mi_design, svyby(share_formula, ~year,
    design = .design, svytotal,
    na.rm = TRUE
))

# 5b. Pool Share Results
pool_shares_riq_inm <- MIcombine(mi_shares_riq_inm)

# 6. Column-Bind directly into a single data.table
all_shares_coef <- coef(pool_shares_riq_inm)

# Extract total wealth per year to act as the localized denominator
total_wealth_by_year <- all_shares_coef[grep(":riquezainmo$", names(all_shares_coef))]
names(total_wealth_by_year) <- sub(":riquezainmo$", "", names(total_wealth_by_year))

# Isolate cumulative totals and divide by their specific year's total wealth
cum_totals <- all_shares_coef[grep(":riq_le_", names(all_shares_coef))]
year_tags <- sub(":riq_le_.*$", "", names(cum_totals))
denom <- total_wealth_by_year[year_tags]

final_stats <- data.table(
    group = names(coef(pool_med_riq_inm)),
    median_riquezainmo = as.numeric(coef(pool_med_riq_inm)),
    share_riquezainmo = as.numeric(cum_totals / denom) * 100
)
# ---------------------------------------------

# 7. Split groups and clean up
final_stats[, c("year", "riquezainmo") := tstrsplit(group, ":", fixed = TRUE)][, group := NULL]
setcolorder(final_stats, c("year", "riquezainmo", "share_riquezainmo"))
setorder(final_stats, year)

# 8. Print results on screen and export to file
print(final_stats)
fwrite(final_stats, "out/informeMICO/4-inmo-inequality-quantiles.csv")
