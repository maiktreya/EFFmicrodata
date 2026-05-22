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
obj <- c()
summary_table <- data.table()

for (year in period) {
    # Load per<U+2011>year mean<U+2011>imputed microdata
    file_path <- sprintf("datasets/eff/%d-EFF.microdat.csv", year)
    eff <- fread(file_path)

    # Ensure types
    eff[, facine3 := as.numeric(facine3)]
    eff[, renta_alquiler := as.numeric(p2_31)]
    eff[, reg_tenencia := factor(p2_19)]

    dt <- svydesign(ids = ~1, data = eff, weights = eff$facine3)

    # Compute quantiles
    quant <- svymean(~renta_alquiler, subset(dt, reg_tenencia == "1"), na.rm = TRUE)

    obj <- c(obj, quant[[1]])
}

names(obj) <- period

rental_grp <- diff(log(obj))
