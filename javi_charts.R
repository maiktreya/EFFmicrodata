# analyze_yearly_data.R
#
# This script loops through the yearly, pre-averaged EFF microdata files
# to calculate and compile key income statistics for each survey year.

library(data.table)
library(magrittr)
library(survey)

# 1. Define the survey years to analyze (excluding 2022, which has a different structure)
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# 2. Initialize vectors to store the results
summary_table <- data.table()

# 3. Loop through each year, load the data, and perform calculations
for (year in period) {
    message(paste("Processing year:", year))

    # Construct file path and load the pre-averaged data for the year
    file_path <- paste0("datasets/eff/", year, "-EFF.microdat.csv")
    eff <- fread(file_path)

    # Ensure correct data types for survey variables
    eff[, facine3 := as.numeric(facine3)]
    eff[, renthog := as.numeric(renthog)]

    # Define the survey design for the year
    design <- svydesign(ids = ~1, weights = ~facine3, data = eff)

    # Calculate weighted mean and median income
    mean_inc <- coef(svyby(~riquezanet, ~bage, design, svymean, na.rm = TRUE))

    summary_table <- cbind(summary_table, c(year, mean_inc))
}
# year born mean(eff$p1_2b_1) %>% print()
# age mean(eff$p1_2d_1) %>% print()

# 4. Combine and display the final results in a table
print(summary_table)
