# MRN Report 2025 Datapack

This repository contains the reproducible scripts and curated data assets that underpin the MRN Report 2025. The datapack cleans Office for Students (OfS) student characteristics data, backcasts the share of Muslim students prior to 2018, combines MRN survey evidence on subject switching and progression delays, and produces the figures, regressions, and sensitivity checks cited in the published report.

## Repository structure

```
repo-root/
|-- R/
|   |-- 0-set-up.R                # package loading, data import, helper objects
|   |-- 1-backcast-student-numbers.R
|   |-- 2-clean-data.R
|   |-- 3-estimation-functions.R
|   `-- 4-results.R
|-- data/
|   |-- hesa subject mappings.xlsx
|   |-- Student-characteristics-populations-2024.csv
|   `-- Wide (filtered).xlsx      # proprietary MRN survey data (optional input)
|-- report/                       # empty output folder to save exported plots/tables
|-- .gitattributes
|-- .gitignore
`-- README.md
```

| File | Description |
| --- | --- |
| `R/0-set-up.R` | Clears the workspace, installs/loads required packages, sets file paths, and loads all raw inputs. The script also creates reproducible fallback objects (`group_X`, `subject_transitions`, `no_switching_mat`, `max_flex_mat`, `year_1max`, `year_3max`) whenever proprietary files are not available. |
| `R/1-backcast-student-numbers.R` | Cleans the OfS student characteristics extract, engineers predictors, imputes missing Muslim shares with `mice`, and fits a penalised regression (`glmnet`) to backcast pre-2018 values by subject x level x population. Outputs `ofs_imputed`. |
| `R/2-clean-data.R` | Reshapes `ofs_imputed` into the long-format `progression_data` object that stores entrants, qualifiers, and progression counts by level, subject group, year, and faith. |
| `R/3-estimation-functions.R` | Defines reusable helper functions: weighted qualifier calculators, MRN plotting theme, plotting helpers for progression rates, and the regression wrapper used throughout the report. |
| `R/4-results.R` | Orchestrates the full suite of analyses (baseline plots, regression gap estimates, sensitivity checks for delay parameters and switching matrices, and diagnostic charts). Running this script after the others recreates the report-ready visuals. |

## Data sources

- **HESA subject mappings (`data/hesa subject mappings.xlsx`)**  
  Used to aggregate detailed HESA subjects to the broad groups shown in the report. If the file is missing, `0-set-up.R` generates a hard-coded mapping so that the pipeline remains reproducible.
- **OfS student characteristics extract (`data/Student-characteristics-populations-2024.csv`)**  
  Download the latest CSV from the [Office for Students student characteristics page](https://www.officeforstudents.org.uk/data-and-analysis/student-characteristics-data/) and place it in `data/`. The file is used verbatim and is required to run the backcasting model.
- **MRN survey data (`data/Wide (filtered).xlsx`)**  
  Proprietary survey microdata containing subject transitions and self-reported progression delays. Because it cannot be redistributed, the repository ships with the code only. When this file is absent, the set-up script injects the aggregated transition matrix and delay estimates that appear in the published report so that public users can still reproduce every figure.

All input files should remain inside the `data/` directory. Update filenames in `R/0-set-up.R` only if you deviate from the default naming.

## Getting started

1. Install R (v4.3 or later recommended) and ensure you can install packages from CRAN. `Rtools`/Xcode may be required on Windows/macOS for packages that need compilation.
2. Clone or download `MRN-Report-2025`, then open the project root in your preferred IDE.
3. Update the `path_main` assignment at the top of `R/0-set-up.R` so that it points to your local clone (or replace it with `path_main <- getwd()` when running from the project root). All other paths are derived from this variable.
4. Run `source("R/0-set-up.R")` inside a fresh R session. The script installs and loads the full package set (`tidyverse`, `mice`, `glmnet`, `tidymodels`, `ggplot2`, `ggthemes`, `writexl`, etc. - see the file for the complete list) and imports every required dataset.
5. Optional but recommended: install the **Silka** font family locally so that the custom MRN plotting theme matches the report. If Silka is unavailable, R will fall back to your default sans-serif font.

> Tip: The package installation loop only runs for missing libraries. Subsequent sessions simply load the required packages.

## Reproducing the analysis

Run the scripts sequentially from a clean session:

```r
source("R/0-set-up.R")              # packages + data + helper objects
source("R/1-backcast-student-numbers.R")  # build ofs_imputed with backcast counts
source("R/2-clean-data.R")          # create progression_data
source("R/3-estimation-functions.R")# load modelling/plotting helpers
source("R/4-results.R")             # render all figures, regressions, and diagnostics
```

The final script prints ggplot objects to the active device; use `ggsave()` to export them into the `report/` folder if you need static files.

## Analytical workflow in brief

1. **Backcasting Muslim representation** - Cleans the OfS extract, imputes missing fields, and fits a penalised logistic regression (with 10-fold cross-validation via `tidymodels`) to obtain predicted Muslim entrants/qualifiers for all years back to 2012.
Aggregates the imputed entrants/qualifiers by subject group, converts counts into progression-ready panels, and stores the result in `progression_data`.
3. **Weighted qualifier pooling** - Uses `group_X` delay parameters and the subject switching matrix to build weighted averages of prior qualifiers (`compute_weighted_q()`), enabling scenario analysis for linear vs. constant weighting, no switching, or maximum switching cases.
4. **Reporting outputs** - Generates baseline progression-over-time plots, subject-level gap charts, regression-based progression gap estimates, and multiple sensitivity analyses (delay lengths, switching weights, and stochastic perturbations).


## Outputs

Running `R/4-results.R` produces:

- Progression rate trends (Muslim vs. non-Muslim) by level.
- Subject-level progression gap plots with clear over-/under-progression markers.
- Regression-based progression rate comparisons, including confidence intervals.
- Sensitivity figures comparing delay weighting schemes, switching assumptions, and stochastic noise applied to the transition matrix.
- Diagnostic panels that contrast actual vs. backcast Muslim entrant/qualifier counts.

Save any plot with `ggsave(filename = "report/progression-over-time.png", width = 10, height = 6)` or similar.

## Troubleshooting

- **Package installation** - The automatic installer requires internet access. If you are working offline, install the listed packages beforehand.
- **Font warnings** - If Silka is absent, update `base_family` in `set_mrn_theme()` to a font available on your system.
- **Long runtimes** - The `mice` imputations and `glmnet` tuning can take several minutes on first run. Reduce `maxit`/`m` or shrink the tuning grid if you only need a quick smoke test.
- **File paths** - Mis-specified `path_main` or missing `data/*.xlsx` / `.csv` files are the most common causes of errors. Check the console log emitted by `R/0-set-up.R` to confirm that each dataset loaded successfully.

## License and attribution

The MRN Report 2025 datapack is provided for transparency around the published analysis. It does not include the proprietary MRN survey microdata, and no explicit open-source licence has been granted. Please contact the MRN analytics team before redistributing the code or derived datasets, and cite the MRN Report 2025 when referencing these materials.
