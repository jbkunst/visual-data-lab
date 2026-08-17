## How it works

This experiment separates two ideas that are easy to mix up:

1. **Data structure** controls how the simulated data are generated.
2. **Fitted model** controls how we choose to explain those same data.

The simulated relationship is

\[
y_{ij} = \beta_0 + b_{0j} + (\beta_1 + b_{1j})x_{ij} + \varepsilon_{ij},
\]

where the group deviations are centered at zero and the observation noise is normal:

\[
\begin{pmatrix}b_{0j}\\b_{1j}\end{pmatrix} \sim N(0, \Sigma),
\qquad
\varepsilon_{ij} \sim N(0, \sigma^2).
\]

Depending on the selected data structure, the random-intercept or random-slope variance can be exactly zero.

### Pooling

**Complete pooling** ignores group differences and estimates one relationship for everyone.

**No pooling** estimates a separate relationship for each group.

**Partial pooling** is the mixed-model middle ground: groups have their own effects, but those effects are estimated together and share information.

### Shrinkage

Shrinkage is the visible result of partial pooling. A group estimate is pulled toward the population relationship when its own data are uncertain. In random-effect notation, the estimated group deviation is pulled toward zero because zero means "no deviation from the population effect."

Groups with less information generally shrink more. Groups with more observations, less noise, or stronger evidence of genuine between-group differences generally shrink less.

### What to inspect

- **Main view:** simulated truth and fitted group relationships.
- **Residuals vs fitted:** remaining structure or changing residual spread.
- **Normal Q-Q:** whether residuals look compatible with a normal-error assumption.
- **Random effects:** estimated group deviations around zero.
- **Shrinkage:** no-pooling estimates compared with the estimates from the selected model.
- **Variance components:** true simulation standard deviations compared with those estimated by the fitted mixed model.

A singular mixed-model fit is informative here: it often means the fitted random-effect covariance has reached a boundary, commonly because one random-effect variance is estimated close to zero.
