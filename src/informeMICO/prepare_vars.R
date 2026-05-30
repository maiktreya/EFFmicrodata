# ==============================================================================
# PREPARE SYNTHETIC VARIABLES: HOUSING WEALTH & RENTAL INCOME (EFF)
# ==============================================================================

library(data.table)
rm(list = ls())
gc(full = TRUE)

# Load raw microdata
eff <- fread("datasets/full_eff.gz")

# ------------------------------------------------------------------------------
# PHASE 1: UNIVERSAL DATA CLEANING & TYPE COERCION
# ------------------------------------------------------------------------------

# Clean main residence value (always a residential vivienda)
eff[, v_principal := as.numeric(p2_5)]
eff[is.na(v_principal), v_principal := 0]

# Generate names for all secondary property columns to clean at once
clean_cols <- c(
    paste0("p2_35a_", 1:4), # Property Types (1 = Residential)
    paste0("p2_43_", 1:4), # Monthly Rental Income
    paste0("p2_39_", 1:4) # Market Value of Properties
)

# Coerce all target columns to numeric and safely replace NAs with 0
for (col in clean_cols) {
    eff[, (col) := as.numeric(get(col))]
    eff[is.na(get(col)), (col) := 0]
}

# ------------------------------------------------------------------------------
# PHASE 2: CALCULATE RESIDENTIAL RENTAL INCOME
# ------------------------------------------------------------------------------

# Extract monthly rent ONLY if asset is a residential unit and rent is positive
eff[, prop1_rent := 0][p2_35a_1 == 1 & p2_43_1 > 0, prop1_rent := p2_43_1]
eff[, prop2_rent := 0][p2_35a_2 == 1 & p2_43_2 > 0, prop2_rent := p2_43_2]
eff[, prop3_rent := 0][p2_35a_3 == 1 & p2_43_3 > 0, prop3_rent := p2_43_3]
eff[, prop4_rent := 0][p2_35a_4 == 1 & p2_43_4 > 0, prop4_rent := p2_43_4]

# Calculate total annualized rental income
eff[, renta_alq := (prop1_rent + prop2_rent + prop3_rent + prop4_rent) * 12]

# Count the number of active rented residential properties
eff[, n_props_alq := (prop1_rent > 0) + (prop2_rent > 0) + (prop3_rent > 0) + (prop4_rent > 0)]

# ------------------------------------------------------------------------------
# PHASE 3: CALCULATE GROSS HOUSING WEALTH
# ------------------------------------------------------------------------------

# Extract market values ONLY if the asset is a residential unit (drop garages/land/etc.)
eff[, prop1_val := 0][p2_35a_1 == 1, prop1_val := p2_39_1]
eff[, prop2_val := 0][p2_35a_2 == 1, prop2_val := p2_39_2]
eff[, prop3_val := 0][p2_35a_3 == 1, prop3_val := p2_39_3]
eff[, prop4_val := 0][p2_35a_4 == 1, prop4_val := p2_39_4]

# Total Gross Housing Wealth = Main Residence + Residential Secondary Properties
eff[, riquezainmo := v_principal + prop1_val + prop2_val + prop3_val + prop4_val]

# ------------------------------------------------------------------------------
# PHASE 4: CLEANUP INTERMEDIATE VARIABLES & SAVE
# ------------------------------------------------------------------------------

# Identify all temporary variables created during calculations
temp_cols <- c(
    "prop1_rent", "prop2_rent", "prop3_rent", "prop4_rent",
    "prop1_val",  "prop2_val",  "prop3_val",  "prop4_val"
)

# Drop them from memory to keep the data.table lean
eff[, (temp_cols) := NULL]

