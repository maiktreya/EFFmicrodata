# analyze_yearly_data_gini.R
library(data.table)
library(survey)
library(mitools)
library(convey)

# 1. Load Data
rm(list = ls())
eff <- fread("datasets/full_eff_refined.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, riquezainmo := as.numeric(riquezainmo)]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)

# 4. Create and Prep Survey Design
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)
# Apply convey_prep to the entire multiply-imputed design object at once
mi_design <- convey_prep(mi_design)

# 5. Gini Coefficient for Real Estate Wealth by Year ONLY
mi_ginis <- with(
    mi_design,
    svyby(~riquezainmo, ~year, design = .design, svygini, na.rm = TRUE)
)

# 6. Pool Results using Rubin's Rules
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

# 8. Exporting the Lorenz Curves to PNG
plot_design <- mi_design$designs[[1]]
des_2002 <- subset(plot_design, year == 2002)
des_2022 <- subset(plot_design, year == 2022)
gini_2002 <- final_ginis[year == 2002, gini]
gini_2022 <- final_ginis[year == 2022, gini]

png("out/informeMICO/charts/5-lorenz_2002_vs_2022.png", width = 800, height = 600, res = 120)

# Wrap the svylorenz calls to silence the harmless base R abline() parameter spam
suppressWarnings({
    # Draw the first Lorenz Curve (2002)
    svylorenz(~riquezainmo,
        design = des_2002,
        main = "Wealth Inequality Shift: 2002 vs 2022",
        xlab = "Cumulative % of Households",
        ylab = "Cumulative % of Real Estate Wealth",
        curve.col = "darkblue",
        lwd = 2,
        type = "o", # "o" draws both lines and points
        pch = 19, # Solid circular dots
        quantiles = seq(0, 1, by = 0.05), # Calculate and plot every 5%
        ci = FALSE
    )

    # Overlay the second Lorenz Curve (2022)
    svylorenz(~riquezainmo,
        design = des_2022,
        curve.col = "darkred",
        lwd = 2,
        type = "o",
        pch = 19,
        quantiles = seq(0, 1, by = 0.05),
        ci = FALSE,
        add = TRUE
    )
})

# Add the legend (updated to include the dots)
legend("topleft",
    legend = c(
        paste("2002 (Gini:", round(gini_2002, 3), ")"),
        paste("2022 (Gini:", round(gini_2022, 3), ")")
    ),
    col = c("darkblue", "darkred"),
    lwd = 2,
    pch = 19, # Tells the legend to display the dots on the lines
    bty = "n",
    cex = 1.1
)

# Close graph and export to file image and table
dev.off()
fwrite(final_ginis, "out/informeMICO/5-inequality-inmo-gini.csv")
