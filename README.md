# Context-dependent temperature-size relationships in marine fishes: evidence from the Black Sea

HGAM analysis of temperature-size relationships in marine fishes

## What this code does

This repository reproduces the statistical workflow used to test the temperature–size rule (TSR) on Black Sea fish species and to quantify where TSR works as a general tendency versus where it becomes context-dependent. The script links life-history traits (`Linf`, `Lmax`, `TL1`, `Amax`, optionally `Lmat` if available) to environmental predictors (`SST`, `PP`, `S`) using pooled hierarchical GAMs (HGAM) and species-level models.

In addition to TSR support/reverse/weak classification, the code evaluates whether the Azov–Black Sea basin differs from other regions after nonlinear environmental adjustment and species structure control. A sensitivity analysis is also run in the overlapping SST range between basin groups to separate basin effects from thermal-range imbalance. All tables/figures are exported into `Results/`.
## How to reproduce results
1. Clone this repository.
2. Install R (recommended version: 4.3 or higher).
3. Install required packages:
install.packages(c("readxl", "dplyr", "tidyr", "purrr", "ggplot2", "mgcv", "car"))
4. Place the input dataset file in the project root directory with the exact name:
Table_S1_Life_history_dataset.xlsx
5. In the main script, set the working directory (line 24):
   working_dir = "."
   (this should point to the folder containing Table_S1_Life_history_dataset.xlsx)
6. Run the script (tsr_script).

**Outputs:**
Results/ — CSV summary tables
Results/figures/ — PNG figures
