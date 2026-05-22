# analyze_yearly_data.R
#
# This script loops through the yearly, pre-averaged EFF microdata files
# to calculate and compile key income statistics for each survey year.

library(data.table)
library(magrittr)
library(survey)

# 1. Define the survey years to analyze (excluding 2022, which has a different structure)
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# 2. Initialize a list to store the results
results_list <- list()

# 3. Loop through each year, load the data, and perform calculations
for (year in period) {
    message(paste("Processing year:", year))

    # Construct file path and load the pre-averaged data for the year
    file_path <- paste0("datasets/eff/", year, "-EFF.microdat.csv")
    eff <- fread(file_path)

    # Ensure correct data types for survey variables
    eff[, facine3 := as.numeric(facine3)]
    eff[, renthog := as.numeric(renthog)]
    eff[, riquezanet := as.numeric(riquezanet)]
    eff[, riquezainmo := as.numeric(p2_5) + as.numeric(otraspr)][, riquezainmo := as.numeric(riquezainmo)]
    eff[, regten := factor(p2_1, levels = c(1:3), labels = c(
        "Alquiler",
        "Propiedad",
        "Cesion"
    ))]
    eff[, bage := factor(bage, levels = c(1:6), labels = c(
        "Menor de 35 anos",
        "Entre 35 y 44 anos",
        "Entre 45 y 54 anos",
        "Entre 55 y 64 anos",
        "Entre 65 y 74 anos",
        "Mayor de 74 anos"
    ))]

    # Define the survey design for the year
    design <- svydesign(ids = ~1, weights = ~facine3, data = eff)

    yearly_stats <- svyby(~renthog, ~regten+bage, design, svymean, na.rm = TRUE) %>%
        as.data.table()

    # Add year column and rename for clarity
    yearly_stats[, year := year]
    setnames(yearly_stats, new = c("year", "age_group", "mean_wealth", "regten", "int"))

    # Store the results for this year
    results_list[[as.character(year)]] <- yearly_stats[, .(year, age_group, mean_wealth)]
}

# 4. Combine and display the final results in a table
summary_table <- rbindlist(results_list, use.names = TRUE)
print(summary_table, nrows = 20)