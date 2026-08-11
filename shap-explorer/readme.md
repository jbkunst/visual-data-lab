The app explains how a credit-risk model turns a client profile into a
probability of default (PD).

The local SHAP chart compares the magnitude and direction of every contribution.
The waterfall then starts at the background mean and adds those contributions
until it reaches the client's predicted PD. Red increases PD and blue reduces
it. The other charts place the current PD in the test portfolio and show how the
active variable relates to SHAP.

For each reference client \\(z_i\\), the calculation starts at \\(p(z_i)\\) and
replaces one variable at a time with its value in the profile \\(x\\). The change
assigned to variable \\(j\\) is

\\[
\\Delta_{ij} = p(\\text{after } j) - p(\\text{before } j).
\\]

Its SHAP value is the average contribution over the \\(N\\) reference paths:

\\[
\\phi_j = \\frac{1}{N}\\sum_{i=1}^{N}\\Delta_{ij}.
\\]

The contributions reconstruct the prediction from the background mean:

\\[
p(x) = \\frac{1}{N}\\sum_{i=1}^{N}p(z_i) + \\sum_j \\phi_j.
\\]

The app starts from a real observation in the test sample, and the sliders are
ordered by average global SHAP importance. **Random case** loads another test
observation. The readable and optimized SHAP implementations are both kept in
`local_shap.R`; the app uses the optimized one.
