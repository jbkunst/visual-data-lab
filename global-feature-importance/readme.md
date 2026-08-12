This app connects model quality with the variables that create it.

- **Permutation loss** compares the selected model's test loss with its loss
  after one variable is shuffled. A larger deterioration means that the model
  relies more strongly on that variable.
- **SAGE decomposition** starts with no aligned variables and attributes the
  reduction in test loss across variables, ending at the full model.
- **Metric diagnostic** explains the selected quality measure using train and
  test data: individual penalties for log-loss, the ROC curve for AUC, the
  CAP/Lorenz curve for Gini, and cumulative good/bad rates for KS.
- **Cumulative gains** compares all models on the test sample and highlights the
  selected model. It shows how many defaults are captured by targeting the
  riskiest share of the portfolio.

AUC ROC, Gini, and KS are better when higher; log-loss is better when lower.
Internally, importance uses `1 − AUC ROC` and `1 − KS` so every method receives
a loss, but the app translates the results back to the selected quality metric.
The Gini diagnostic uses the CAP/Lorenz view and compares the observed model
with random and perfect selection.

Individual log-loss is small when the model assigns high probability to the
observed outcome. A long right tail identifies confidently wrong predictions;
these may be difficult cases, data errors, distribution shift, or overconfident
model behavior and should be investigated rather than automatically discarded.

Permutation and this marginal Monte Carlo approximation of SAGE can create
unlikely profiles when values from different clients are combined. This matters
most when predictors are strongly correlated. SAGE contributions are displayed
from largest to smallest, but each contribution already averages many possible
variable orders.

References: [Permutation feature importance](https://christophm.github.io/interpretable-ml-book/feature-importance.html),
[SAGE](https://github.com/iancovert/sage),
[ROC and AUC](https://en.wikipedia.org/wiki/Receiver_operating_characteristic),
and [Kolmogorov–Smirnov test](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test).
