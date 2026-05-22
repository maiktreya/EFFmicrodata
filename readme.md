# EFF Microdata: ETL + Analysis (2002–2022)

Normalize • Minimize • Unify

An attempt to normalize, minimize, and unify the fragmentary and tedious base files provided by Banco de España for working with the Encuesta Financiera de las Familias (EFF) microdata.

This repo contains a minimal, fast R pipeline to prepare and analyze the Spanish household wealth survey (EFF) across multiple waves. It focuses on stacking the 5 imputations per wave, exporting ready‑to‑use microdata, and providing lightweight examples for weighted analysis.

---

## Repository Layout

- `ELT/`
  - `full_wave_join_dta.R` — Merge 5 imputations from the original Stata `.dta` files into a single stacked microdata file for the wave, exporting `datasets/eff/<year>-EFF.microdat.gz`.
  - `full_wave_join_csv.R` — Same idea when sources are CSVs (paths are configurable at the top of the script).
  - `create_mean_imputation.R` — Convenience script to build a single “mean‑imputed” flat file per wave at `datasets/eff/<year>-EFF.microdat.csv`. Useful for quick charts; not a substitute for Rubin pooling in formal inference.
- `datasets/`
  - `full/` — Raw official microdata by wave (e.g., `EFF_2022/databol*.dta`, `otras_secciones_imp*.dta`, `seccion6_imp*.dta`).
  - `eff/` — Ready‑to‑use outputs per wave: both stacked `.gz` and mean‑imputed `.csv` for many years (2002–2022).
- `examples/`
  - `test_survey.R` — Minimal example: weighted mean/median using the 5 imputations (2022).
  - `analyze_yearly_data.R` — Loops over years using the mean‑imputed `.csv` files to compile simple indicators.
- `src/`
  - `wealth_age.R`, `tenancy_age.R` — Small analysis snippets built on the prepared files.
- `doc/` — Official documentation: user guide, questionnaire, and annexes for EFF 2022.
- `out/` — Scratch folder for your tables/figures (ignored by git).

---

## Quick Start

- Install R packages: `data.table`, `haven`, `survey` (optional: `mitools`, `magrittr`).
- Put raw files for the target wave under `datasets/full/<WAVE>/` (see `datasets/full/EFF_2022/` as a reference).
- Build the stacked file from `.dta` sources: run `ELT/full_wave_join_dta.R`.
  - Output: `datasets/eff/<year>-EFF.microdat.gz` (directly readable by `data.table::fread`).
- (Optional) Create the quick “mean‑imputed” flat file: run `ELT/create_mean_imputation.R` after setting `year`.
  - Output: `datasets/eff/<year>-EFF.microdat.csv`.
- Run examples:
  - Weighted analysis per imputation: `examples/test_survey.R` (uses `.gz`).
  - Yearly loop for quick indicators: `examples/analyze_yearly_data.R` (uses `.csv`).

---

## Analyses (src/) and Outputs (out/)

- `src/wealth_cohort.R` → `out/wealth_cohort.csv`
  - Weighted mean net wealth by cohort and aligned cohort-age (uses mean‑imputed `.csv`).
- `src/tenancy_cohort.R` → `out/tenancy_cohort.csv`
  - Homeownership rate (`np2_1`) by cohort with the same cohort‑age alignment.
- `src/wealth_age.R` → `out/wealth_age.csv`
  - Weighted mean net wealth by respondent age (`bage`) for each year.
- `src/tenancy_age.R` → optional variant for tenure by age.

Run any analysis with Rscript, e.g.:

```
Rscript src/wealth_cohort.R
Rscript src/tenancy_cohort.R
Rscript src/wealth_age.R
```

All outputs are written under `out/` and can be joined or charted as needed.

---

## Key Variables

- Household id: `h_<year>` (e.g., `h_2022`) — join key across sections and imputations.
- Survey weight: `facine3` — must be used in any design‑based estimate.
- Income examples: `renthog21_eur22` (2022), `renthog` (earlier waves).
- Imputation index: `imputation` (1–5) — added during stacking.
- % HOMEOWNERSHIP (P2_35_1)
- N OF PROPS BY AGE AND CLASS (p2_33)
- MULTIPROP (s6_owner)
- RENTAL INCOME EFF (p7_2)
- PENSION FUNDS ASSETS (VALOR)
- FINANCIAL ASSETS (riquezafin)
- FINANCIAL ASSETS NO SAVINGS (riquezafin)
- MAIN RESIDENCE DEBT (dvivpral)
- DEBT RATIO (ratio_deuda)
- TOTAL DEBT (vdeuda)
- OVERDB (overburden)
- NET WORTH (riquezanet)
- OTHER REAL ASSETS DEBT (deuoprop)
- MAIN RESIDENCE VALUE (np2)
- OTHER REAL ASSETS VALUE (otraspr)
- HOUSING ASSETS (otraspr + np2)
- INHERITANCE (P2_2)
- MORTAGED (dvivpral)
- COMPONENTES OF FINANCIAL ASSETS
- "p4_15" -> ACCIONES COTIZADAS EN BOLSA
- "p4_24" -> ACCIONES NO COTIZADAS EN BOLSA
- "p4_35" -> VALORES RENTA FIJA
- "allf" -> FONDOS DE INVERSION
- "p4_43" -> CARTERAS GESTIONADAS (SOLO 2017)

---

## Notes

- `fread()` supports `.gz` natively; no manual decompression needed.
- When merging sections, keep `facine3` only once to avoid duplicates (handled in `ELT/full_wave_join_*`).
- For inference, compute estimates per imputation and pool (e.g., `mitools::MIcombine()`), rather than using the “mean‑imputed” files.

---

Maintainer: Miguel
Language: R
License: GPL‑3
