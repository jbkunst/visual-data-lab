PDP, ALE, and ICE describe how a model's predicted probability of default changes
with one variable. They answer related questions from different perspectives.

- **PDP** averages predictions after assigning the same value to every client.
- **ALE** accumulates local changes within populated regions of the data.
- **ICE** traces the prediction for individual clients. The dark line is their PDP.
- **Distribution** shows where the observed clients are concentrated.

PDP and ICE are probabilities. ALE is centered at zero and expressed in percentage
points: positive values increase predicted default relative to the model average;
negative values decrease it.

Use **PD scale** to shift the ALE curve by the model's average PD and show it on
the same 0–1 probability scale as PDP. This changes only the vertical reference:
the shape and accumulated effects remain the same. The result is an ALE-adjusted
PD, not a prediction for a particular client.

These are model behaviors, not causal effects. Sparse regions in the distribution
should be interpreted cautiously.

References: [Interpretable Machine Learning](https://christophm.github.io/interpretable-ml-book/index.html)
and [Accumulated local effects](https://en.wikipedia.org/wiki/Accumulated_local_effects).
