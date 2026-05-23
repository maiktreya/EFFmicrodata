# analyze_yearly_data.R
#
# This script loops through the yearly, pre-averaged EFF microdata files
# to calculate key income statistics, Gini for real estate wealth (riquezainmo),
# and plot Lorenz curves for each survey year.

library(data.table)
library(magrittr)
library(survey)
library(convey) # Added for inequality metrics

# 1. Define the survey years to analyze
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# 2. Initialize lists to store the results
results_list <- list()

# Set up a grid layout for your plots so you can compare Lorenz curves side-by-side
par(mfrow = c(2, 4))

# 3. Loop through each year, load the data, and perform calculations
for (year in period) {
    message(paste("Processing year:", year))

    # Construct file path and load the pre-averaged data for the year
    file_path <- paste0("datasets/eff/", year, "-EFF.microdat.csv.gz")
    eff <- fread(file_path)

    # Ensure correct data types for survey variables
    eff[, facine3 := as.numeric(facine3)]
    eff[, renthog := as.numeric(renthog)]
    eff[, riquezanet := as.numeric(riquezanet)]

    # Handle NAs during transformation just in case any missing rows break math
    eff[, p2_5 := as.numeric(p2_5)][is.na(p2_5), p2_5 := 0]
    eff[, otraspr := as.numeric(otraspr)][is.na(otraspr), otraspr := 0]
    eff[, riquezainmo := p2_5 + otraspr]
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

    # Prep design for inequality metrics
    convey_design <- convey_prep(design)

    # --- Calculations ---

    # A. Calculate Mean Income stats by category
    yearly_stats <- svyby(~renthog, ~ regten + bage, design, svymean, na.rm = TRUE) %>%
        as.data.table()

    # B. Calculate Overall Gini Coefficient for Real Estate Wealth (riquezainmo)
    # Note: Wealth distributions often contain zero or negative values.
    # 'convey' handles zero/negative values by default, but it's good practice to ensure.
    # B. Calculate Overall Gini Coefficient for Real Estate Wealth (riquezainmo)
    gini_riquezainmo <- svygini(~riquezainmo, design = convey_design, na.rm = TRUE)
    gini_val <- as.numeric(gini_riquezainmo) # Extract just the numeric value

    # C. Plot the Lorenz Curve for this year
    svylorenz(~riquezainmo,
        design = convey_design,
        main = paste("Lorenz Curve:", year),
        xlab = "Cumulative % of Households",
        ylab = "Cumulative % of Real Estate Wealth",
        curve.col = "darkblue",
        lwd = 2,
        na.rm = TRUE
    )

    # D. Add the Gini Value to the Chart
    # round(..., 3) keeps it clean with three decimal places
    text(
        x = 0.1, y = 0.9,
        labels = paste("Gini:", round(gini_val, 3)),
        adj = c(0, 0.5), # Left-aligns the text at the x coordinate
        col = "darkred",
        font = 2
    ) # Makes the text bold

    # Append the calculated Gini directly to your summary statistics rows
    yearly_stats[, gini_riquezainmo := as.numeric(gini_riquezainmo)]
    yearly_stats[, year := year]

    # Store the results for this year
    results_list[[as.character(year)]] <- yearly_stats[, .SD]
}

# Reset plotting grid layout back to default (1x1)
par(mfrow = c(1, 1))

# 4. Combine and display the final results in a table
summary_table <- rbindlist(results_list, use.names = TRUE)
print(summary_table, nrows = 20)
