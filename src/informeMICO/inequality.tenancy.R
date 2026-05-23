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

eff <- fread("datasets/full_eff.gz")

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

# A. Calculate Mean Income stats by category
yearly_stats <- svyby(~renthog, ~ year + regten + bage, design, svymean, na.rm = TRUE) %>%
    as.data.table()
print(yearly_stats)
