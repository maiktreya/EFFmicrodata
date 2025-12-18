#!/usr/bin/env Rscript
# wealth_age_shares.R
#
# Compute, for each survey wave, the share of total population and the share of
# total net wealth held by age groups 1<U+2013>6 in variable `bage`.

suppressPackageStartupMessages({
    library(data.table)
    library(survey)
    library(magrittr)
})

# Waves included
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# Accumulator
obj <- list()
summary_table <- data.table()

for (year in period) {
    # Load per<U+2011>year mean<U+2011>imputed microdata
    file_path <- sprintf("datasets/eff/%d-EFF.microdat.csv", year)
    eff <- fread(file_path)

    # Ensure types
    eff[, facine3 := as.numeric(facine3)]
    eff[, riquezanet := as.numeric(riquezanet)]

    dt <- svydesign(ids = ~1, data = eff, weights = eff$facine3)

    quant <- svyquantile(~riquezanet, dt, quantile = c(0.5, 0.99), na.rm = TRUE)
    pre <- quant$riquezanet[, 1]
    obj1 <- svytotal(~riquezanet, dt, na.rm = TRUE)
    obj2 <- svytotal(~riquezanet, subset(dt, riquezanet < pre["0.5"]), na.rm = TRUE)
    obj3 <- svytotal(~riquezanet, subset(dt, riquezanet > pre["0.99"]), na.rm = TRUE)

    obj[[year]] <- data.table(share.low50 = obj2[1] / obj1[1], share.top99 = obj3[1] / obj1[1])
}
summary_table <- cbind(period, rbindlist(obj)) %>% print()

# Output
if (!dir.exists("out")) dir.create("out", recursive = TRUE)
fwrite(summary_table, "out/wealth_quantile_shares.csv")
