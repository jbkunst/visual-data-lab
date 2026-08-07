# Shared credit analysis preparation

This directory builds the reusable modeling results used by the credit applications. The Shiny apps should filter and visualize these results rather than repeat expensive calculations at runtime.

## Run everything

Install `celavi` from GitHub if necessary:

```r
pak::pkg_install("jbkunst/celavi")
```

Then run from the repository root:

```r
source("R/credit-data/06-prepare-combined.R")
```

The stages can also be run separately:

1. `01-prepare-data.R`: split, models and test predictions.
2. `02-prepare-shap.R`: local SHAP values for the complete test sample.
3. `03-prepare-effects.R`: ICE and ALE values.
4. `04-prepare-importance.R`: Permutation, Drop-column, SAGE and global SHAP.
5. `05-prepare-evaluation.R`: ROC/KS thresholds, gains, AUC, Gini and KS.
6. `06-prepare-combined.R`: run stages 01 to 05 and consolidate their artifacts.

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

Permutation importance is calculated with `celavi` over 100 iterations for both log-loss and `1 - AUC ROC`. Drop-column models are retrained from the original training sample. SAGE uses a marginal Monte Carlo path approximation with test rows as the reference distribution. Global SHAP is calculated as `mean(abs(shap))`, so it measures prediction movement rather than loss.

## App artifacts

- `shap-explorer/shap-credit.rds`
- `variable-effects/credit-effects.rds`
- `global-feature-importance/credit-importance.rds`
- `model-evaluation/credit-evaluation.rds`

## Combined artifact

`06-prepare-combined.R` creates:

```text
R/credit-data/credit-analysis.rds
```

It is a self-contained analysis artifact. Common objects are stored once:

- `train`, `test`, `predictors`, `models`, `predictions` and `baseline`;
- SHAP, ICE and ALE values under `explanations`;
- all global importance results in `importance_values`;
- threshold, gains and summary results under `evaluation`;
- common metadata once, plus method-specific metadata.

`status_bad` is stored in `test`, not repeated inside every model prediction.
Likewise, gains scores are recovered from `predictions` by `model` and `row_id`.

The four app-specific artifacts do not contain the training sample. The combined artifact stores it once so it can also support auditing and retraining without loading the intermediate model artifact.

This PR only prepares the artifacts. It does not yet change the existing `shap-explorer` app to read the new path or schema.
