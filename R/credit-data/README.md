# Shared credit analysis preparation

This directory builds the reusable modeling results used by the credit applications. The Shiny apps should filter and visualize these results rather than repeat expensive calculations at runtime.

## Run everything

Install `celavi` from GitHub if necessary:

```r
pak::pkg_install("jbkunst/celavi")
```

Then run from the repository root:

```r
source("R/credit-data/99-prepare-all.R")
```

The stages can also be run separately:

1. `01-prepare-data.R`: split, models and test predictions.
2. `02-prepare-shap.R`: local SHAP values for the complete test sample.
3. `03-prepare-effects.R`: ICE and ALE values.
4. `04-prepare-importance.R`: Permutation, Drop-column, SAGE and global SHAP.
5. `05-prepare-evaluation.R`: ROC/KS thresholds, gains, AUC, Gini and KS.

## Intermediate model artifact

`01-prepare-data.R` creates:

```text
R/credit-data/credit-models.rds
```

It contains `train`, `test`, the four reduced models and test predictions. `train` is retained only because Drop-column importance must retrain every model without each variable. This intermediate file is not part of any Shiny app and is ignored by Git.

## Reuse decisions

Equivalent results are not stored twice:

- `test` is also the SHAP background and the observed variable distribution;
- `predictions` supports score distributions and all evaluation calculations;
- `shap_values` supports dependence plots and `mean(abs(shap))` global importance;
- `ice_values` supports ICE directly and PDP by averaging over `row_id`;
- `threshold_curve` supports both ROC and KS;
- `gains_curve` supports cumulative gains, Lorenz-style views and lift.

The models are reduced with `butcher` after prediction equivalence is checked. XGBoost is serialized as raw bytes.

## Importance methods

Permutation importance is calculated with `celavi` and log-loss. Drop-column models are retrained from the original training sample. SAGE uses a marginal Monte Carlo path approximation with test rows as the reference distribution. Global SHAP is calculated as `mean(abs(shap))`, so it measures prediction movement rather than loss.

## App artifacts

- `shap-explorer/shap-credit.rds`
- `variable-effects/credit-effects.rds`
- `global-feature-importance/credit-importance.rds`
- `model-evaluation/credit-evaluation.rds`

These four artifacts share identifiers and conventions, so a future combined app can load and reuse them without recalculating the methods. None includes the training sample.
