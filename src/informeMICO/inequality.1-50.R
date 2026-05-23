# analyze_yearly_data_gini.R
library(data.table)
library(magrittr)
library(survey)
library(convey) 
library(mitools) 

# 1. Load Data
eff <- fread("datasets/full_eff.gz")

# 2. Vectorized Data Cleaning
eff[, facine3      := as.numeric(facine3)]
eff[, renthog      := as.numeric(renthog)]
eff[, riquezanet   := as.numeric(riquezanet)]
eff[, p2_5         := as.numeric(p2_5)][is.na(p2_5), p2_5 := 0]
eff[, otraspr      := as.numeric(otraspr)][is.na(otraspr), otraspr := 0]
eff[, riquezainmo  := p2_5 + otraspr]

eff[, regten := factor(p2_1, levels = c(1:3), labels = c("Alquiler", "Propiedad", "Cesion"))]
eff[, bage   := factor(bage, levels = c(1:6), labels = c(
    "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
    "Entre 55 y 64 anos", "Entre 65 y 74 anos", "Mayor de 74 anos"
))]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data  <- mitools::imputationList(eff_list)

# 4. Create and Prep Survey Design
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)
# Apply convey_prep to the entire multiply-imputed design object at once
mi_design <- convey_prep(mi_design)

# 5. Run the Vectorized Estimations (Parallel execution over imputations)
# A. Mean Income by Year, Tenure, and Age
mi_means <- with(mi_design, 
    svyby(~renthog, ~ year + regten + bage, design = .design, svymean, na.rm = TRUE)
)

# B. Gini Coefficient for Real Estate Wealth by Year ONLY
mi_ginis <- with(mi_design, 
    svyby(~riquezainmo, ~year, design = .design, svygini, na.rm = TRUE)
)

# 6. Pool Results using Rubin's Rules
pooled_means <- MIcombine(mi_means)
pooled_ginis <- MIcombine(mi_ginis)

# 7. Tidy the Gini Results Table
final_ginis <- data.table(
    year = names(coef(pooled_ginis)),
    gini = coef(pooled_ginis),
    se   = SE(pooled_ginis)
)
# Ensure year is numeric for easy merging/subsetting
final_ginis[, year := as.numeric(year)] 

print(final_ginis)

# 8. Plotting the Lorenz Curves
# We extract the first implicate's design for plotting, but label it with the POOLED Gini.
plot_design <- mi_design$designs[[1]]
period      <- sort(unique(eff$year))

par(mfrow = c(2, 4))

# lapply replaces the 'for' loop for a cleaner, functional plotting approach
invisible(lapply(period, function(yr) {
    # Isolate the data for this specific year
    sub_des <- subset(plot_design, year == yr)
    
    # Extract the pooled Gini value we calculated in step 6 & 7
    yr_gini <- final_ginis[year == yr, gini]

    # Draw the Lorenz Curve
    svylorenz(~riquezainmo, 
        design = sub_des,
        main = paste("Lorenz Curve:", yr),
        xlab = "Cumulative % of Households",
        ylab = "Cumulative % of Real Estate Wealth",
        curve.col = "darkblue", 
        lwd = 2, 
        na.rm = TRUE
    )

    # Annotate with the exact pooled Gini
    text(
        x = 0.1, y = 0.9, 
        labels = paste("Gini:", round(yr_gini, 3)),
        adj = c(0, 0.5), 
        col = "darkred", 
        font = 2
    )
}))

par(mfrow = c(1, 1))