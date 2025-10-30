# README — EFF 2022 Microdata Processing and Analysis

## 1. Overview and Metadata Context

This project works with the **Encuesta Financiera de las Familias (EFF) 2022**,
Spain’s official household wealth survey conducted by the **Banco de España**.

Each wave of the EFF provides five **multiple-imputation datasets**, released in three main sections:

| Section             | Description                                                                             | File pattern                                            | Identifier(s) |
| ------------------- | --------------------------------------------------------------------------------------- | ------------------------------------------------------- | ------------- |
| **databol**         | Core household-level data: wealth, debt, income, housing, reference person demographics | `databol1.dta` – `databol5.dta`                         | `h_2022`      |
| **otras_secciones** | Other sections of the questionnaire: socio-demographics, employment, financial products | `otras_secciones_imp1.dta` – `otras_secciones_imp5.dta` | `h_2022`      |
| **seccion6**        | Section 6, household expenditures and related details                                   | `seccion6_imp1.dta` – `seccion6_imp5.dta`               | `h_2022`      |

**Key identifiers and weights:**

| Variable          | Meaning                                               |
| ----------------- | ----------------------------------------------------- |
| `h_2022`          | Unique household identifier across all sections       |
| `facine3`         | Household sampling weight                             |
| `imputation`      | Added manually (1–5) to track each imputed dataset    |
| `renthog21_eur22` | Total household income (2021 euros, adjusted to 2022) |

All datasets share the same number of imputations (5), where each imputation represents a *plausible version* of the complete dataset under multiple imputation procedures.

---

## 2. Data Wrangling and Merge Strategy

The workflow is intentionally minimal and high-performance, relying solely on **`data.table`** and **`haven`**.

### 🔹 Step 1. Read and combine imputations

Each `.dta` file is read using `haven::read_dta()`.
The five imputations per section are stacked with `rbindlist()` while adding an `imputation` index.

```r
load_imp <- function(fmt, i) as.data.table(read_dta(sprintf(fmt, i)))[, imputation := i]
```

### 🔹 Step 2. Merge all sections

All three sections are merged sequentially using the household identifier `h_2022` and the imputation index:

```r
eff_2022 <- merge(databol, otras, by = c("h_2022", "imputation"), all.x = TRUE)
eff_2022 <- merge(eff_2022, sec6,  by = c("h_2022", "imputation"), all.x = TRUE)
```

Only the household weights `facine3` from the `databol` section are retained to avoid duplicates.

### 🔹 Step 3. Export compressed output

The combined dataset is exported as a compressed CSV:

```r
fwrite(eff_2022, "datasets/eff/2022-EFF.microdat.gz", compress = "gzip")
```

This produces a single file roughly 10–20× smaller than the uncompressed version,
and it can be read directly with `fread()`.

---

## 3. Robust Estimation with Multiple Imputation and Survey Weights

The **EFF** data require two types of correction for valid inference:

1. **Survey design weights (`facine3`)** to ensure representativeness.
2. **Multiple imputations** to handle missingness in key variables.

The recommended approach uses the **`survey`** package.
Each imputation is treated as a separate design, and statistics are computed independently.

### Example: Weighted household income estimation

```r
library(data.table)
library(survey)

eff <- fread("datasets/eff/2022-EFF.microdat.gz")
eff[, facine3 := as.numeric(facine3)]
eff[, renthog21_eur22 := as.numeric(renthog21_eur22)]

designs <- lapply(sort(unique(eff$imputation)), function(i)
  svydesign(ids = ~1, weights = ~facine3, data = eff[imputation == i])
)

mean_inc <- sapply(designs, \(d) coef(svymean(~renthog21_eur22, d, na.rm = TRUE)))
median_inc <- sapply(designs, \(d) {
  as.numeric(svyquantile(~renthog21_eur22, d, quantiles = 0.5, ci = FALSE, na.rm = TRUE))
})
```

This produces one estimate per imputation.
Simple teaching examples can average them directly:

```r
mean(mean_inc)      # naïve pooled mean
mean(median_inc)    # naïve pooled median
```

For rigorous analyses, use **Rubin’s pooling rules** via `mitools::MIcombine()`:

```r
library(mitools)
MIcombine(results = mean_inc)
```

---

## 4. Summary

| Component                             | Description                                                                 |
| ------------------------------------- | --------------------------------------------------------------------------- |
| **full_wave_join.R**                  | Reads, merges, and exports the EFF 2022 microdata (5 imputations × 3 files) |
| **test_survey.R**                     | Demonstrates robust estimation with sampling weights using `survey`         |
| **datasets/eff/2022-EFF.microdat.gz** | Final compressed dataset (ready for analysis)                               |
| **facine3**                           | Must always be used as survey weight                                        |
| **renthog21_eur22**                   | Recommended income variable for cross-sectional estimation                  |

---

### ✳️ Notes

* `fread()` can read `.gz` files directly, so compression is transparent.
* The merge process can safely handle additional variables or future waves (just update file paths).
* Always check `summary(eff$facine3)` before computing weighted statistics — missing or zero weights will bias results.

---

**Maintainer:** Miguel
**Language:** R (data.table + survey + haven)
**Purpose:** Efficient, reproducible, and pedagogically clean pipeline for handling the Spanish EFF 2022 microdata.
**License:** GPL3
