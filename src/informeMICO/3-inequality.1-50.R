# analyze_wealth_shares.R
library(data.table)
library(survey)
library(mitools)

# 1. Load Data
rm(list = ls())
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, riquezanet := as.numeric(riquezanet)]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)

# 4. Custom Function: Calculate Totals for Top 1% and Bottom 50%
# This runs natively inside svyby for each year independently
calc_bracket_totals <- function(formula, design, ...) {
    # A. Get the exact 50th and 99th percentiles for this specific subset
    qs <- svyquantile(formula, design, quantiles = c(0.50, 0.99), na.rm = TRUE)
    q50 <- as.numeric(coef(qs)[1])
    q99 <- as.numeric(coef(qs)[2])

    # B. Isolate the wealth falling into these brackets
    # update() dynamically adds these columns to the active survey design subset
    des_up <- update(design,
        wealth_bot50 = ifelse(riquezanet <= q50, riquezanet, 0),
        wealth_top1  = ifelse(riquezanet >= q99, riquezanet, 0)
    )

    # C. Return the weighted absolute totals
    svytotal(~ wealth_bot50 + wealth_top1, des_up, na.rm = TRUE)
}

# 5. Run Vectorized Estimations
# svyby will slice the design by year, feed it to our custom function, and return the totals
mi_totals <- with(
    mi_design,
    svyby(~riquezanet, ~year, design = .design, FUN = calc_bracket_totals)
)

# 6. Pool Results (Applying Rubin's Rules for MI)
pool_totals <- MIcombine(mi_totals)

# 7. Convert to a clean data.table
final_stats <- data.table(
    group        = names(coef(pool_totals)),
    total_wealth = as.numeric(coef(pool_totals))
)

# Split group (e.g., "2002:wealth_bot50") into distinct columns
final_stats[, c("year", "bracket") := tstrsplit(group, ":", fixed = TRUE)][, group := NULL]

# 8. The SQL "PIVOT": Cast the long data into a wide table for clean side-by-side comparison
final_table <- dcast(final_stats, year ~ bracket, value.var = "total_wealth")

# Rename columns for presentation
setnames(final_table,
    old = c("wealth_bot50", "wealth_top1"),
    new = c("total_bottom_50", "total_top_1")
)
final_table[, rati050_1 := total_top_1 / total_bottom_50]

# 9. Print results on screen and export to file
print(final_table)
fwrite(final_table, "out/informeMICO/3-inequality-ratio50_1.csv")
