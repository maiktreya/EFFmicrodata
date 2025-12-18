# Compute, for each survey wave, the share of total population and the share of
# total net wealth held by selected quantiles groups

suppressPackageStartupMessages({
    library(data.table)
    library(survey)
    library(magrittr)
})

# Waves included
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# Accumulators
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

    # Compute quantiles
    quant <- svyquantile(~riquezanet, dt, quantile = c(0.5, 0.99), na.rm = TRUE)
    pre <- quant$riquezanet[, 1]

    # Compute shares
    obj1 <- svytotal(~riquezanet, dt, na.rm = TRUE)
    obj2 <- svytotal(~riquezanet, subset(dt, riquezanet < pre["0.5"]), na.rm = TRUE)
    obj3 <- svytotal(~riquezanet, subset(dt, riquezanet >= pre["0.99"]), na.rm = TRUE)

    # Accumulate
    obj[[year]] <- data.table(share.low50 = obj2[1] / obj1[1], share.top99 = obj3[1] / obj1[1])
}

# Vectorize to matrix
summary_table <- cbind(period, rbindlist(obj)) %>% print()

# Output
if (!dir.exists("out")) dir.create("out", recursive = TRUE)
fwrite(summary_table, "out/wealth_quantile_shares.csv")
