# Shared credit analysis preparation

This directory builds the reusable modeling results used by the credit applications. The Shiny apps should filter and visualize these results rather than repeat expensive model calculations at runtime.

## Run everything

Install `celavi` from GitHub if necessary:

```r
pak::pkg_install("jbkunst/celavi")
```

Then run from the repository root:

```r
source("R/credit-data/99-prepare-all.R")
```

The scripts can also be run one at a time:

1. `01-prepare-data.R`: train/test split, four models, test predictions and the base artifact.
2. `02-prepare-shap.R`: local SHAP values for every test observation, using test as background.
3. `03-prepare-effects.R`: ICE and ALE values. PDP is derived as the mean ICE curve.
4. `04-prepare-importance.R`: permutation, drop-column, SAGE and global SHAP importance.
5. `05-prepare-evaluation.R`: threshold, gains, AUC, Gini and KS results.
6. `06-export-artifacts.R`: write a minimal RDS beside each application.

Each stage updates `R/credit-data/credit-analysis.rds`. This is the complete reusable artifact for a possible future application that combines all four views.

## Reuse decisions

The preparation deliberately avoids storing equivalent objects twice:

- `test` is also the SHAP background and the observed variable distribution;
- `predictions` supports score distributions and evaluation summaries;
- `shap_values` supports local/dependence views and `mean(abs(shap))` global importance;
- `ice_values` supports ICE directly and PDP by averaging over `row_id`;
- `threshold_curve` supports both ROC and KS;
- `gains_curve` supports cumulative gains, Lorenz-style views and lift.

The models are reduced with `butcher` only after prediction equivalence is checked. XGBoost is serialized as raw bytes.

## Importance methods

Permutation importance is calculated with `celavi` and log-loss. Drop-column models are retrained from the original training sample. SAGE uses a marginal Monte Carlo path approximation with test rows as the reference distribution. Global SHAP is calculated as `mean(abs(shap))` and therefore measures prediction movement rather than loss.

## Exported artifacts

- `shap-explorer/shap-credit.rds`
- `variable-effects/credit-effects.rds`
- `global-feature-importance/credit-importance.rds`
- `model-evaluation/credit-evaluation.rds`

Generated RDS files are not included in this draft PR. Run the preparation locally, inspect the printed sizes and commit them later only when the corresponding app needs the artifact for deployment.
