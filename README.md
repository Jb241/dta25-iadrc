# IADRC Dementia Diagnosis and Depression Project

Stata code for preparing the IADRC clinical data used in a dementia-labeling paper and analyzing relationships among dementia/MCI diagnosis, depressive symptoms, and cognitive measures.

## Project structure

The scripts use paths relative to the `Code` directory. Place this repository in a `Code` folder and arrange the surrounding folders like this:

```
Parent folder/
├── Output/
│   └── Dta25 Output/
│       ├── Figures/
│       ├── Statistics/
│       └── Tables/
│           └── raw/
├── Dta25 Code/                this repository
└── Dta25 Data/
    └── raw and intermediate/
```

Create the output directories before running the analysis. Input data and generated `.dta` files should not be stored in this repository.

## Data

Place these source files in `Dta25 Data/raw and intermediate/`:

- `IADC_clinical_2025-12-05.xlsx`
- `IADRC_clinical_2026-05-11.xlsx`
- `IADC_clinical_12APR2024.csv`

The datasets are available from Siyun Peng for members of the SNAD research team.

## Requirements

- Stata commands `esttab`, `mimrgns`, and `mibeta` available via installation

## Workflow

| Step | File | Input | Output |
|---|---|---|---|
| 1. Recode | `d2_iadc-recodes.do` | Three source files in `Dta25 Data/raw and intermediate/` | Recoded intermediate data and `Dta25 Data/iadc_dta25.dta` |
| 2. Impute | `d2_iadc_mi.do` | `Dta25 Data/iadc_dta25.dta` | `Dta25 Data/mi_iadc_dta25_recoded.dta` |
| 3. Analyze | `d9_iadc-analysis.do` | `Dta25 Data/mi_iadc_dta25_recoded.dta` | Tables, figures, model estimates, and CSV summaries under the `Output` folders |

## Output

The analysis writes tables and figures to `Output/Dta25 Output/`. This output folder must exist before the scripts run.
