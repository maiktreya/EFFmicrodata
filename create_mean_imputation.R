# create_mean_imputation.R
#
# This script creates a single, "mean-imputed" dataset from the stacked
# multiple imputation microdata. It calculates the average of each numeric
# variable across the 5 imputations for each household.
#
# This is a "naive" pooling method, useful for basic exploration but not
# recommended for formal statistical inference, where Rubin's rules
# (e.g., using the `survey` and `mitools` packages) should be used.

library(data.table)

# 1. Load the combined microdata with all 5 imputations
eff_stacked <- fread("datasets/eff/2022-EFF.microdat.gz")

# 2. Identify numeric and non-numeric columns to process
#    - `id_vars`: Columns to group by (household identifier).
#    - `num_vars`: Numeric columns to be averaged.
#    - `char_vars`: Character columns to keep (taking the first value).
id_vars <- "h_2022"
all_vars <- names(eff_stacked)
num_vars <- all_vars[sapply(eff_stacked, is.numeric) & !(all_vars %in% c(id_vars, "imputation"))]
char_vars <- all_vars[sapply(eff_stacked, is.character) & !(all_vars %in% id_vars)]

# 3. Group by household and calculate the mean for numeric variables
eff_mean_imputed <- eff_stacked[,
    c(
        lapply(.SD[, ..num_vars], mean, na.rm = TRUE), # Average numeric vars
        lapply(.SD[, ..char_vars], first)
    ), # Keep first char var
    by = h_2022
]

# 4. Export the result to a new CSV file
fwrite(eff_mean_imputed, "datasets/eff/2022-EFF.microdat.csv")
message("Successfully created mean-imputed dataset at: datasets/eff/2022-EFF.mean_imputed.csv")
