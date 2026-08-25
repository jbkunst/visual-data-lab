Expectation-Maximization alternates between two directions. The **E step** sends the current group parameters toward every observation and calculates soft membership probabilities. The **M step** sends those weighted observations back toward the groups and updates their proportions, means, and standard deviations.

Use the separate E and M buttons to see which part of the interface changes. The histogram always uses every simulated observation, while the table displays 20 at a time. The component curves show \\(\pi_k f_k(x)\\), so their sum is the complete mixture density.

The trajectory panels add a new point only after an M step. When those paths flatten, the parameters and membership probabilities have stabilized.
