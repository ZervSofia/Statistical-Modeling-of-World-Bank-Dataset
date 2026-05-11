# Statistical Modeling of World Bank Dataset
# World Bank unemployment modeling in R

This repository contains two connected regression modeling assignments completed during my master’s, refactored into a clean, modular R project.

The goal is to model country-level unemployment using World Bank–style indicators from the `spData::worldbank_df` dataset, compare alternative model specifications, and evaluate them with cross-validation, bootstrap, and model selection techniques.

---

## Project structure

```text
R/                  # All reusable functions
scripts/            # Entry-point scripts for each assignment
outputs/            # Saved plots and CV results
