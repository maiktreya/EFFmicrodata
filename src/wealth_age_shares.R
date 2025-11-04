#!/usr/bin/env Rscript
# wealth_age_shares.R
#
# Compute, for each survey wave, the share of total population and the share of
# total net wealth held by age groups 1–6 in variable `bage`.

suppressPackageStartupMessages({
  library(data.table)
})

# Waves included
period <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022)

# Accumulator
summary_table <- data.table()

for (year in period) {
  message(sprintf("Processing year: %d", year))

  # Load per‑year mean‑imputed microdata
  file_path <- sprintf("datasets/eff/%d-EFF.microdat.csv", year)
  eff <- fread(file_path)

  # Ensure types
  eff[, facine3 := as.numeric(facine3)]
  eff[, riquezanet := as.numeric(riquezanet)]
  eff[, bage := as.integer(bage)]

  # Keep requested age groups only
  eff <- eff[bage %in% 1:6]
  if (nrow(eff) == 0L) next

  # Totals (weighted)
  total_pop <- eff[, sum(facine3, na.rm = TRUE)]
  total_wealth <- eff[, sum(facine3 * riquezanet, na.rm = TRUE)]

  # Grouped sums
  by_age <- eff[, .(
    pop_weight = sum(facine3, na.rm = TRUE),
    wealth_weighted = sum(facine3 * riquezanet, na.rm = TRUE)
  ), by = bage][order(bage)]

  # Shares
  by_age[, population_share := if (is.finite(total_pop) && total_pop > 0) pop_weight / total_pop else NA_real_]
  by_age[, wealth_share := if (is.finite(total_wealth) && total_wealth != 0) wealth_weighted / total_wealth else NA_real_]

  by_age[, year := year]
  setcolorder(by_age, c("year", "bage", "population_share", "wealth_share", "pop_weight", "wealth_weighted"))

  summary_table <- rbindlist(list(summary_table, by_age), use.names = TRUE)
}

# Output
if (!dir.exists("out")) dir.create("out", recursive = TRUE)
fwrite(summary_table, "out/wealth_age_shares.csv")
print(summary_table)

