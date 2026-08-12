This credit-risk lab shows how a continuous predictor becomes a set of risk bins using
[`risk3r`](https://github.com/jbkunst/risk3r).

1. The selected algorithm chooses the cut points.
2. Each bin compares its share of bad clients with its share of good clients.
3. **Weight of Evidence** is `log(bad share / good share)`. Positive values
   indicate relatively more bad clients; negative values indicate relatively
   more good clients.
4. Each bin contributes `(bad share - good share) × WoE` to **Information Value**.
   The contributions add up to the variable's total IV.

The controls change the complexity of the binning. Maximum bins limits the final
groups, minimum bin share avoids poorly supported groups, and the stopping
threshold controls how aggressively adjacent values are combined.

IV summarizes the marginal separation of one predictor. `risk3r` labels it as
unpredictive, weak, medium, strong, or suspicious. A high IV is useful for
screening, but it does not establish causality or guarantee out-of-sample model
quality. Very high values can also reveal leakage and should be investigated.
