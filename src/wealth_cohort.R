# wealth_cohort.R
# Cohort wealth analysis using standardized cohort-age anchors.
# Each cohort is anchored to its upper birth-year boundary to track
# wealth accumulation at equivalent points in the cohort lifecycle.
library(magrittr)
library(data.table)
library(survey)

# 1. Configuration
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)
cuts <- c(1928, 1945, 1964, 1980, 1996)
anchor_years <- cuts[-1] # Upper boundaries: 1945, 1964, 1980, 1996
cohort_anchors <- data.table(cohort = c("Silent", "Boomer", "GenX", "Millennial"), anchor_year = anchor_years)
results_list <- list()

# 2. Cohort assignment function
assign_cohort <- function(birth_year, scheme = "standard") {
    if (scheme == "standard") {
        cut(birth_year,
            breaks = cuts,
            labels = c("Silent", "Boomer", "GenX", "Millennial"),
            right = TRUE
        )
    } else if (scheme == "decade") {
        paste0(floor(birth_year / 10) * 10, "s")
    }
}

# 3. MAIN LOGIC: Loop through waves
for (year in period) {
    message(paste("Processing year:", year))

    # Load data
    eff <- paste0("datasets/eff/", year, "-EFF.microdat.csv") %>% fread()
    # Prepare data
    eff[, facine3 := as.numeric(facine3)]
    eff[, riquezanet := as.numeric(riquezanet)]
    eff[, cohort := assign_cohort(p1_2b_1)]
    eff <- eff[!is.na(cohort)]

    # Define survey design
    design <- svydesign(ids = ~1, weights = ~facine3, data = eff)

    # Calculate weighted means with confidence intervals
    res_dt <- svyby(~riquezanet, ~cohort, design, svymean, na.rm = TRUE) %>% as.data.table()

    # Add year and compute cohort-age using lookup table
    res_dt[, year := year]
    res_dt <- merge(res_dt, cohort_anchors, by = "cohort", all.x = TRUE)
    res_dt[, cohortage := year - anchor_year]

    # Select and order final columns
    res_dt <- res_dt[, .(
        year,
        cohort = as.character(cohort),
        cohortage,
        mean_riquezanet = as.numeric(riquezanet)
    )]

    results_list[[as.character(year)]] <- res_dt
}

# 4. Combine results across all years
summary_table <- rbindlist(results_list, use.names = TRUE)

# 5. Display and export results
print(summary_table)
fwrite(summary_table, "out/wealth_cohort.csv")
