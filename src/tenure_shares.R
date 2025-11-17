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
    library(survey)
    library(magrittr)
})

# Waves included
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# Accumulator
year_shares <- year_mean_props <- year_otherprops <- list()

# Loop
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

    # data transform (define CATEGORICALS: nproperties and otherprops & TOTAL NUM: nprops)
    eff[is.na(eff)] <- 0
    eff[, nprops := 0][, nprops := np2_1 + p2_33]
    eff[, nproperties := 0]
    eff[nprops == 0, nproperties := 0]
    eff[nprops == 1, nproperties := 1]
    eff[nprops >= 2, nproperties := 2]
    eff[, otherprops := 0][p2_33 > 0, otherprops := 1]
    eff[, nproperties := factor(nproperties, levels = c(0, 1, 2), labels = c("No assets", "Homeowner", "2+"))]
    eff[, otherprops := factor(otherprops, levels = c(0, 1), labels = c("No assets", "Other than main"))]


    # survey design definition
    design <- svydesign(ids = ~1, weights = ~facine3, data = eff)
    i <- as.character(year)

    # population stats calculation
    year_shares[[i]] <- prop.table(svytable(~nproperties, design = design)) %>% as.data.table()
    year_mean_props[[i]] <- svyby(~nprops, ~bage, design, svymean) %>% as.data.table()
    year_otherprops[[i]] <- svyby(~otherprops, ~bage, design, svymean) %>% as.data.table()
}

# Output
summary_table_shares <- rbindlist(year_shares, use.names = TRUE)
summary_table_mean_props <- rbindlist(year_mean_props, use.names = TRUE)
summary_table_otherprops <- rbindlist(year_otherprops, use.names = TRUE)

# Export
fwrite(summary_table, "out/tenure_shares.csv")
fwrite(summary_table_mean_props, "out/new/tenure_mean_props.csv")
fwrite(summary_table_otherprops, "out/new/tenure_otherprops.csv")

print(summary_table)
print(summary_table_mean_props)
print(summary_table_otherprops)
