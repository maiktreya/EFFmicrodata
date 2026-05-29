# prepare syntetic variables

library(data.table)
rm(list = ls())
gc(full = TRUE)
eff <- fread("datasets/full_eff.gz")

# Properties 1-3: rent amounts
eff[, prop1 := 0][p2_35a_1 == 1 & p2_43_1 > 0, prop1 := p2_43_1]
eff[, prop2 := 0][p2_35a_2 == 1 & p2_43_2 > 0, prop2 := p2_43_2]
eff[, prop3 := 0][p2_35a_3 == 1 & p2_43_3 > 0, prop3 := p2_43_3]

# Properties 4+: type flags + joint rent
eff[, prop41 := 0][p2_42s1_4 == 4 & p2_43_4 > 0, prop41 := 1]
eff[, prop42 := 0][p2_42s2_4 == 4 & p2_43_4 > 0, prop42 := 1]
eff[, prop43 := 0][p2_42s3_4 == 4 & p2_43_4 > 0, prop43 := 1]
eff[, prop44 := 0][p2_42s4_4 == 4 & p2_43_4 > 0, prop44 := 1]
eff[, prop45 := 0][p2_42s5_4 == 4 & p2_43_4 > 0, prop45 := 1]
eff[, prop4 := prop41 + prop42 + prop43 + prop44 + prop45]
eff[prop4 > 0, prop4 := p2_43_4]

# Total rental income (annualised)
eff[, renta_alq := (prop1 + prop2 + prop3 + prop4) * 12]

# Number of rented properties (lower bound for 4+ block, exact for 1-3)
eff[, n_props_alq := (prop1 > 0) + (prop2 > 0) + (prop3 > 0) + (prop4 > 0)]

# Cleanup intermediates
eff[, c(
    "prop1", "prop2", "prop3", "prop4",
    "prop41", "prop42", "prop43", "prop44", "prop45"
) := NULL]
