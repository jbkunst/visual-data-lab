Nine classifiers learn from the same three-class sample. Points are observations; each background shows a model's prediction across the two-dimensional space.

**Display**

- **Class + confidence** fades the winning class color when its probability or vote share is lower. Contours mark 50%, 70%, and 90%.
- **Predicted class** shows only the class selected at each location.

Confidence is a model score, not necessarily a calibrated probability.

**References**

- [Da Silva, Cook, and Lee (2021)](https://doi.org/10.1080/10618600.2020.1814300) for PPforest.
- [TensorFlow Playground](https://playground.tensorflow.org/) for the confidence color treatment.
