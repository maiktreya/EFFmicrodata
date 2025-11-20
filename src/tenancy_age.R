# wealth_age.R
#
# This script loops through the yearly, pre-averaged EFF microdata files
# to calculate and compile mean net wealth by age group for each survey year.

library(data.table)
library(survey)
library(magrittr)

# 1. Define the survey years to analyze
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# 2. Initialize a list to store the results
results_list <- list()

# 3. Loop through each year, load the data, and perform calculations
for (year in period) {
    message(paste("Processing year:", year))

    # Construct file path and load the pre-averaged data for the year
    file_path <- sprintf("datasets/eff/%d-EFF.microdat.csv", year)
    eff <- fread(file_path, showProgress = FALSE)

    # Ensure correct data types for survey variables
    eff[, facine3 := as.numeric(facine3)]
    # The variable 'bage' is used for grouping, ensure it's a factor for clarity
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

    # Calculate weighted mean net wealth by age group
    # svyby returns a survey object; convert it to a data.table
    yearly_stats <- svyby(~np2_1, ~bage, design, svymean, na.rm = TRUE) %>%
        as.data.table()

    # Add year column and rename for clarity
    yearly_stats[, year := year]
    setnames(yearly_stats, old = c("bage", "np2_1"), new = c("age_group", "mean_wealth"))

    # Store the results for this year
    results_list[[as.character(year)]] <- yearly_stats[, .(year, age_group, mean_wealth)]
}

# 4. Combine and display the final results in a table
summary_table <- rbindlist(results_list, use.names = TRUE)
print(summary_table, nrows = 20)
fwrite(summary_table, "out/tenancy_age.csv")
