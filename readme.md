**EFF Microdata: ETL + Analysis (2002–2022)**

This repo contains a minimal, fast R pipeline to prepare and analyze the Spanish household wealth survey (EFF) across multiple waves. It focuses on stacking the 5 imputations per wave, exporting ready‑to‑use microdata, and providing lightweight examples for weighted analysis.

**Stack:** R with `data.table`, `haven`, `survey` (+ optional `mitools`).

**Data location:** Place official EFF files locally under `datasets/full/<WAVE>/` (e.g., `datasets/full/EFF_2022/`). The repo already includes example outputs under `datasets/eff/` for 2002–2022.

---

**Repository Layout**

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

**Quick Start**

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

**Key Variables**

- Household id: `h_<year>` (e.g., `h_2022`) — join key across sections and imputations.
- Survey weight: `facine3` — must be used in any design‑based estimate.
- Income examples: `renthog21_eur22` (2022), `renthog` (earlier waves).
- Imputation index: `imputation` (1–5) — added during stacking.

---

**Notes**

- `fread()` supports `.gz` natively; no manual decompression needed.
- When merging sections, keep `facine3` only once to avoid duplicates (handled in `ELT/full_wave_join_*`).
- For inference, compute estimates per imputation and pool (e.g., `mitools::MIcombine()`), rather than using the “mean‑imputed” files.

---

Maintainer: Miguel
Language: R
License: GPL‑3
