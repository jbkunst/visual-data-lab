## How it works

This experiment separates two ideas that are easy to mix up:

1. **Data structure** controls how the simulated data are generated.
2. **Fitted model** controls how we choose to explain those same data.

Start with **No model** to inspect the grouped observations without fitted or true relationship lines. Then add models and compare what each one assumes.

The simulated relationship is

\\[
y_{ij} = \\beta_0 + b_{0j} + (\\beta_1 + b_{1j})x_{ij} + \\varepsilon_{ij},
\\]

where the group deviations are centered at zero and the observation noise is normal:

\\[
\\begin{pmatrix}b_{0j}\\\\b_{1j}\\end{pmatrix} \\sim N(0, \\Sigma),
\\qquad
\\varepsilon_{ij} \\sim N(0, \\sigma^2).
\\]

Depending on the selected data structure, the random-intercept or random-slope variance can be exactly zero.

For scenarios with intercept differences, groups are observed over partly different ranges of \\(x\\). Those ranges are generated independently of the random effects; they simply make the contrast between a pooled relationship and within-group relationships easier to see.

### Pooling

**Complete pooling** ignores group differences and estimates one relationship for everyone.

**No pooling** estimates a separate relationship for each group using only that group's observations.

**Partial pooling** is the mixed-model middle ground: groups have their own effects, but those effects are estimated together and share information.

### Shrinkage

Shrinkage is the visible result of partial pooling. A group estimate is pulled toward the population relationship when its own data are uncertain. In random-effect notation, the estimated group deviation is pulled toward zero because zero means "no deviation from the population effect."

Groups with less information generally shrink more. Groups with more observations, less noise, or stronger evidence of genuine between-group differences generally shrink less.

### Train and test RMSE

The same simulated dataset is split within every group into approximately 70% training observations and 30% test observations. Each candidate model is fitted on the training observations and evaluated on both sets.

A low training RMSE only says that a model describes the observations it already saw well. The test RMSE asks whether that fitted relationship also predicts unseen observations from the **same groups**. This helps show why no pooling can fit small groups very closely yet produce less stable group-specific relationships than a partially pooled model.

This is not yet a test on completely new groups. Predicting a group that was absent during fitting is a separate hierarchical-model question.

### What to inspect

- **Main view:** raw grouped observations, then simulated truth and fitted group relationships after selecting a model.
- **Residuals vs fitted:** remaining structure or changing residual spread, colored by group.
- **Normal Q-Q:** whether residuals look compatible with a normal-error assumption, with group colors retained.
- **Random effects:** estimated group deviations around zero.
- **Shrinkage:** no-pooling estimates compared with estimates from the selected model.
- **Train / test RMSE:** predictive performance of all candidate models on the same split.

A singular mixed-model fit is informative here: it often means the fitted random-effect covariance has reached a boundary, commonly because one random-effect variance is estimated close to zero.
