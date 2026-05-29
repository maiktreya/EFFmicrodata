library(data.table)
rm(list = ls())
dt <- fread("datasets/full_eff.gz")

# Properties 1-3: rent amounts
dt[, prop1 := 0][p2_35a_1 == 1 & p2_43_1 > 0, prop1 := p2_43_1]
dt[, prop2 := 0][p2_35a_2 == 1 & p2_43_2 > 0, prop2 := p2_43_2]
dt[, prop3 := 0][p2_35a_3 == 1 & p2_43_3 > 0, prop3 := p2_43_3]

# Properties 4+: type flags + joint rent
dt[, prop41 := 0L][p2_42s1_4 == 4 & p2_43_4 > 0, prop41 := 1L]
dt[, prop42 := 0L][p2_42s2_4 == 4 & p2_43_4 > 0, prop42 := 1L]
dt[, prop43 := 0L][p2_42s3_4 == 4 & p2_43_4 > 0, prop43 := 1L]
dt[, prop44 := 0L][p2_42s4_4 == 4 & p2_43_4 > 0, prop44 := 1L]
dt[, prop45 := 0L][p2_42s5_4 == 4 & p2_43_4 > 0, prop45 := 1L]
dt[, prop4 := as.double(prop41 + prop42 + prop43 + prop44 + prop45)]
dt[prop4 > 0, prop4 := p2_43_4]

# Total rental income (annualised)
dt[, renta_alq := (prop1 + prop2 + prop3 + prop4) * 12]

# Number of rented properties (lower bound for 4+ block, exact for 1-3)
dt[, n_props_alq := (prop1 > 0) + (prop2 > 0) + (prop3 > 0) +
    prop41 + prop42 + prop43 + prop44 + prop45]

# Cleanup intermediates
dt[, c(
    "prop1", "prop2", "prop3", "prop4",
    "prop41", "prop42", "prop43", "prop44", "prop45"
) := NULL]

fwrite(dt, "datasets/full_eff_exp.gz")
