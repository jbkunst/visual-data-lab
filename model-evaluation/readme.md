The four panels evaluate the selected model on the same held-out test sample.

- **ROC** compares the true-positive and false-positive rates across thresholds.
- **KS** is the largest gap between those rates and identifies its threshold.
- **Gains** shows the fraction of defaults captured by targeting the riskiest clients.
- **Lift** compares that capture rate with random selection.

The sidebar summarizes log-loss, AUC, Gini, and KS. Lower log-loss is better;
higher values are better for the other three metrics.

These curves evaluate ranking and probability quality on this particular test
sample. They do not determine the operational threshold or account for the costs
of false positives and false negatives.
