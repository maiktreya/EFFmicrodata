#!/usr/bin/env Rscript
# tenure_shares.R
#
# Compute, for each EFF survey wave, the weighted share of households that:
#   - have no housing assets,
#   - are homeowners (dummy `np2_1`), and
#   - own 2 or more properties (count `p2_33`).
#
# Notes on variables (per user context):
#   - `np2_1` is a 0/1 dummy for being homeowner.
#   - `p2_33` stands for the total number of properties (populated for
#     households with properties; may be NA otherwise). We classify
#     "2+ properties" as `p2_33 >= 2`.

suppressPackageStartupMessages({
    library(data.table)
})

# Waves included
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# Accumulator
summary_table <- data.table()

for (year in period) {
    message(sprintf("Processing year: %d", year))

    # Load per<U+2011>year mean<U+2011>imputed microdata
    file_path <- sprintf("datasets/eff/%d-EFF.microdat.csv", year)
    eff <- fread(file_path, showProgress = FALSE)

    # Ensure types
    eff[, facine3 := as.numeric(facine3)]
    eff[, np2_1 := as.numeric(np2_1)]
    # p2_33 may be character; coerce safely
    if (!is.numeric(eff[["p2_33"]])) {
        suppressWarnings(eff[, p2_33 := as.numeric(p2_33)])
    } else {
        eff[, p2_33 := as.numeric(p2_33)]
    }

    # Total weight
    total_w <- eff[, sum(facine3, na.rm = TRUE)]
    if (!is.finite(total_w) || total_w <= 0) next

    # Indicators
    # Any property recorded if p2_33 > 0 (covers owners not living in main home)
    eff[, any_property := 0]
    eff[, homeowner := 0]
    eff[, two_plus := 0]
    eff[, no_housing_assets := 0]
    eff[np2_1 == 1 | p2_33 > 0, any_property := 1]
    # Homeowner (main residence) per provided dummy
    eff[np2_1 == 1 & p2_33 == 0, homeowner := 1]
    # Two or more properties
    eff[np2_1 == 1 & p2_33 > 0, two_plus := 1]
    # No housing assets: not homeowner and no other properties
    eff[!homeowner & !any_property, no_housing_assets := 1]

    # Weighted shares
    share_no_assets <- eff[no_housing_assets == TRUE, sum(facine3, na.rm = TRUE)] / total_w
    share_homeowner <- eff[homeowner == TRUE, sum(facine3, na.rm = TRUE)] / total_w
    share_two_plus <- eff[two_plus == TRUE, sum(facine3, na.rm = TRUE)] / total_w

    summary_table <- rbindlist(list(
        summary_table,
        data.table(
            year = year,
            share_no_housing_assets = share_no_assets,
            share_homeowners = share_homeowner,
            share_two_or_more_props = share_two_plus
        )
    ), use.names = TRUE)
}

# Output
if (!dir.exists("out")) dir.create("out", recursive = TRUE)
fwrite(summary_table, "out/tenure_shares.csv")
print(summary_table)
