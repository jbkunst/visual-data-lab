Nine classifiers learn from the same three-class data. Points are observations; each background is a model's prediction across the two-dimensional space.

## Display

- **Predicted class** colors every location by the winning class.
- **Class + confidence** strengthens that color as the winning probability or vote share increases. Contours mark scores of 50%, 70%, and 90%.

The score represents model confidence; it is not necessarily a calibrated probability.

## Compare

The models are Multinomial, KNN, SVM, rpart, Naive Bayes, Neural network, Random forest, PPforest, and XGBoost.

Cards can be ordered by test accuracy, test macro-F1, execution time, or model family. **Compare results** adds train/test performance, recall, timing, and agreement. Results describe the current sample, not a repeated benchmark.

## Datasets

Synthetic data include bands, islands, XOR, ellipses, rings, and spirals. Iris and Palmer Penguins provide real examples. Every dataset has three classes.

## References

- Natalia da Silva, Dianne Cook, and Eun-Kyung Lee (2021), [*A Projection Pursuit Forest Algorithm for Supervised Classification*](https://doi.org/10.1080/10618600.2020.1814300).
- Y. D. Lee, Dianne Cook, J. W. Park, and E.-K. Lee (2013), [*PPtree: Projection Pursuit Classification Tree*](https://doi.org/10.1214/13-EJS810).
- [TensorFlow Playground](https://playground.tensorflow.org/) for the confidence color treatment.
