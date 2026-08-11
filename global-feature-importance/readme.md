Each panel asks a different global question about the selected model.

- **Permutation** measures how much test loss increases after shuffling one variable.
- **Drop-column** retrains the model without one variable and measures the increase in log-loss.
- **SAGE** restores variables along random paths and attributes reductions in a selected loss function.
- **Global SHAP** averages the absolute local SHAP contribution of each variable.

Positive values indicate useful variables. Small negative values can occur through
sampling variation or when a variable slightly harms out-of-sample performance.
Because the methods use different definitions and units, compare rankings rather
than bar lengths across panels.

SAGE is defined relative to a loss function, so its importance values depend on
what aspect of model quality that function measures. This app offers log-loss,
which evaluates the predicted probabilities and their calibration, and
`1 − AUC ROC`, which evaluates ranking only. Log-loss is the default. Without
aligned variables, `1 − AUC` is expected to be near 0.5; useful variables reduce
it toward the full model's value.

Permutation and this marginal Monte Carlo approximation of SAGE can create
unlikely profiles when values from different clients are combined. This matters
most when predictors are strongly correlated.

The SAGE decomposition starts from the average value of the selected loss
function with no aligned variables, subtracts the mean SAGE contribution of
every variable, and ends at the full model's loss. It uses the stored SAGE
results; the app does not rerun the Monte Carlo calculation.

References: [Permutation feature importance](https://christophm.github.io/interpretable-ml-book/feature-importance.html),
[SAGE](https://github.com/iancovert/sage), and
[SHAP](https://christophm.github.io/interpretable-ml-book/shap.html).
