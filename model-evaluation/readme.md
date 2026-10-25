The four panels evaluate the selected classification model on the same test sample.

- **ROC** compares true-positive and false-positive rates across thresholds.
- **KS** shows the gap between those rates and where it is largest.
- **Gains** shows how many positive cases are captured by targeting the
  highest scores, with random and perfect models as references.
- **Log-loss** shows each prediction's penalty. Confident mistakes receive the
  largest penalties.

Each card reports its corresponding test metric: AUC, KS, Gini, or mean
log-loss. Higher is better for the first three; lower is better for log-loss.

The results describe ranking and probability quality. They do not select an
operational threshold or include classification costs.
