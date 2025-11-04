# analyze_yearly_data.R
#
# This script loops through the yearly, pre-averaged EFF microdata files
# to calculate and compile key income statistics for each survey year.

library(data.table)
library(survey)

# 1. Define the survey years to analyze (excluding 2022, which has a different structure)
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# 2. Initialize vectors to store the results
mean_inc_c <- c()
median_inc_c <- c()

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
    mean_inc <- coef(svymean(~renthog, design, na.rm = TRUE))
    median_inc <- svyquantile(~renthog, design, quantiles = 0.5, ci = FALSE, na.rm = TRUE)[[1]][1]

    # Append results to the storage vectors
    mean_inc_c <- c(mean_inc_c, mean_inc)
    median_inc_c <- c(median_inc_c, median_inc)
}

# 4. Combine and display the final results in a table
results <- data.table(year = period, mean_income = mean_inc_c, median_income = median_inc_c)
print(results)
