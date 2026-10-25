This app decomposes the selected model's test performance by variable.

- **Permutation loss** compares the selected model's test loss with its loss
  after one variable is shuffled. A larger deterioration means that the model
  relies more strongly on that variable.
- **Metric diagnostic** explains the selected quality measure using train and
  test data: individual penalties for log-loss, the ROC curve for AUC, the
  CAP/Lorenz curve for Gini, and cumulative good/bad rates for KS.
- **SAGE decomposition** averages many variable orders and assigns each
  variable a share of the change from the shuffled baseline to the full model.

AUC, Gini, and KS are better when higher; log-loss is better when lower.
Correlated variables may share their contribution between them.
